

# Example taken from (but converted to a clinical setting) McElreath, R: Statistical Rethinking (2020), see https://xcelab.net/rm/statistical-rethinking/
# See also (I have taken a lot of code from there):  https://bookdown.org/content/3890/


(list = ls(all.names = TRUE))
gc()

library(brms)
library(tidyverse)
library(ggthemes)

a       <-  1.4          # Log-odds
sigma   <-  1.5
n_hospitals <- 60

 inv_logit_scaled(a)          # Probability


 # Simulate data
set.seed(12)
dsim <-
  tibble(hospital   = 1:n_hospitals,
         ni     = rep(c(5, 10, 25, 35), each = n_hospitals / 4) %>% as.integer(),            # rep(c(20, 20, 20 , 20)
         true_a = rnorm(n = n_hospitals , mean = a, sd = sigma))

head(dsim)

set.seed(12)
(
  dsim <-
    dsim %>%
    mutate(si = rbinom(n = n(), prob = inv_logit_scaled(true_a), size = ni))
)

table(dsim$ni)

####################################################################################################################
# No pooling
dsim <- (
  dsim <-
    dsim %>%
    mutate(p_nopool = si / ni)
)

      nopooloverall <- mean(dsim$p_nopool)
          sum(dsim$p_nopool)/ (dim(dsim)[1])

              chk <- (dsim$ni/sum(dsim$ni))*dsim$p_nopool
              sum(chk)                                      # Viktat medelvärde



####################################################################################################################

dsim2 <- dsim %>% mutate(no_si = ni-si) %>%
  select(hospital, si, no_si , ni , p_nopool)

dsim3 <- dsim2 %>% gather("e" , "Y" , - c(hospital , ni , p_nopool)) %>%
  mutate(event = ifelse(e == "no_si" , 0, 1) , hospital = factor(hospital) ) %>%
  arrange(hospital) %>%
  mutate(index=1:n())

# Density of proportions

ggplot(dsim3, aes(x=p_nopool))+
  geom_density(color="red", fill="lightblue") +
  theme(axis.text.x = element_text( size = 15))

####################################################################################################################

# Estimate overall mean
m_ <- glm(event ~ 1, family="binomial", weight = Y , data = dsim3 )
 inv_logit_scaled(coef(m_))
 inv_logit_scaled(confint(m_))

# One level per hospital
m1 <- glm(event ~  hospital, family="binomial", weight = Y, data = dsim3)
summary(m1)

dsim4 <- dsim3 %>% mutate(p_nopool_reg = inv_logit_scaled(predict(m1)))

# Same as simple acergaes above.
ggplot(dsim4, aes(x=p_nopool_reg))+
  geom_density(color="darkblue", fill="lightblue") +
  theme(axis.text.x = element_text( size = 15))



####################################################################################################################
library(lme4)

#     http://svmiller.com/blog/2018/06/mixed-effects-models-optimizer-checks/

# Hospital as random effect

m2 <- glmer(event ~   (1 | hospital), family="binomial", weight = Y, data = dsim3 ,
            control = glmerControl(optimizer = "bobyqa"),  nAGQ = 10 )
summary(m2)


inv_logit_scaled(fixef(m2))
inv_logit_scaled(confint(m2))


dsim5 <- dsim4 %>% mutate(p_pool_reg = inv_logit_scaled(predict(m2, dsim3)))
dsim6 <- dsim5 %>% filter(e == "si")

# Random effects and global mean effect estimate written out
# Note the shrinkage effect of going from 0.73 (fixed effect model) up to 0.79 (random effect model)

ggplot(dsim6, aes(x=p_pool_reg))+
  geom_density(color="darkblue", fill="lightblue") +
  theme(axis.text.x = element_text( size = 15)) +
geom_vline(xintercept = 0.79, lty = 2) +
  ggplot2::annotate("text",
                    x = 0.85, y = 0.5, # x and y coordinates of the text
                    label = "0.79 (95%CI: 0.72; 0.85)",
                    size = 6, hjust = 1)


#_________________________________________________________________________________________________

# Bayesian model.
TEST <-
  brm(data = dsim,
      family = binomial,
      si | trials(ni) ~ 1 ,
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 12)
fixef(TEST)[, 1]
inv_logit_scaled(fixef(TEST)[, 1])
inv_logit_scaled(fixef(TEST)[, 3])
inv_logit_scaled(fixef(TEST)[, 4])
prior_summary(TEST)

#     https://bookdown.org/content/3890/multilevel-models.html

# Posterior distribution of estimated parameter
library(tidybayes)
posterior_samples(TEST) %>%
  ggplot(aes(x = inv_logit_scaled(b_Intercept) , y = 0)) +
  stat_halfeye(fill = "orange2", .width = .95) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text( size = 15))

                    # One level per hospital
                         TTT <-  brm(data = dsim,
                                      family = binomial,
                                      si | trials(ni) ~ 1 + factor(hospital),
                                      prior(normal(0, 5), class = b),
                                      iter = 2000, warmup = 500, chains = 4, cores = 4,
                                      seed = 12)
                                prior_summary(TTT)
                                      inv_logit_scaled(fixef(TTT)[1, 1])
                                      inv_logit_scaled(fixef(TTT)[1, 3])
                                      inv_logit_scaled(fixef(TTT)[1, 4])
#____________________________________________________________________________________________________

 # One level per hospital . No pooling
bnopool <-
  brm(data = dsim,
      family = binomial,
      si | trials(ni) ~ 0 + factor(hospital),
      prior(normal(0, 5), class = b),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 12)

prior_summary(bnopool)

length(fixef(bnopool)[, 1])

dsim6$bnopool <- inv_logit_scaled(fixef(bnopool)[, 1])


posterior <- as.array(bnopool)
dimnames(posterior)

#    https://cran.r-project.org/web/packages/bayesplot/vignettes/plotting-mcmc-draws.html

# Plot the postiroe estimates and credivle intervals
library(bayesplot)
color_scheme_set("red")
mcmc_intervals(posterior , pars = vars( -(lp__) ) ,
               regex_pars = "beta", transformations = "inv_logit_scaled")

                                    post <- posterior_samples(bnopool, add_chain = T)

# Restultat blir i princip identiska. Bara data upplagt på lite olika sätt
bnopoolX <-
  brm(data = dsim5,
      family = binomial,
      event | weights(Y)  ~ 0 + factor(hospital),
      prior(normal(0, 5), class = b),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 12)

# Hospital has a prior (normal) which has a hyperparameter (sigma that has a Cauchy distribution).
# This causes even more shrinkage than without the hyperparameter)
bpool <-
  brm(data = dsim,
      family = binomial,
      si | trials(ni) ~ 1  + (1 | hospital),
      prior = c(prior(normal(0, 1), class = Intercept),
                prior(cauchy(0, 1), class = sd)),          #         # Between-hospital variation
      iter = 4000, warmup = 1000, chains = 4, cores = 4,
      seed = 12)

prior_summary(bpool)
                                            # Fixed effect part
                                            inv_logit_scaled(fixef(bpool)[, 1])
                                            inv_logit_scaled(fixef(bpool)[, 3])
                                            inv_logit_scaled(fixef(bpool)[, 4])


post <- posterior_samples(bpool, add_chain = T)

                                posterior_ <- as.array(bpool)
                                dimnames(posterior_)

                                color_scheme_set("red")
                                mcmc_intervals(posterior_ , pars = vars( -(lp__) ) ,
                                               regex_pars = "beta", transformations = "inv_logit_scaled")

# Cpmbine all outputs

dsim7 <-
  coef(bpool, robust = T)$hospital[, , ] %>%
  as_tibble() %>%
  bind_cols(dsim6) %>%
  mutate(bpool = inv_logit_scaled(Estimate))

dsim8 <- dsim7 %>% select(hospital , p_nopool_reg ,  p_pool_reg ,  bnopool , bpool) %>%
      pivot_longer(-hospital, names_to = "Type", values_to = "Probability") %>%
  mutate(Type_ = factor(Type)) %>%
  mutate(Type_ = recode(Type_ ,  p_nopool_reg = "No shrinkage",
                                      p_pool_reg = "Random intercept shrinkage" ,
                                      bnopool = "Bayesian No shrinkage" ,
                                      bpool = "Bayesian shrinkage") )


#############################################################################################

library(ggthemes)

# Note that the models with shrinkage has smaller tails (i.e. everything is shrunk to mean overall mean)
dsim8 %>%
  ggplot(aes(x = Probability, fill = Type_)) +
  geom_density(size = 0 , bw = 0.05) +
  scale_fill_manual(values = c("orange1", "orange2" , "orange3" , "orange4")) +
  scale_y_continuous(breaks = NULL) +
  labs(title = " ",
       subtitle = "") +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  facet_wrap(~Type_) +                       #   scales = "free"
  theme(strip.text.x = element_text(size = 16, colour = "black")) +
  theme(axis.text.x = element_text( size = 14))

# Same but without smoothing
dsim8 %>%
  ggplot(aes(x = Probability, fill = Type_)) +
  geom_histogram() +
  scale_fill_manual(values = c("orange1", "orange2" , "orange3" , "orange4")) +
  geom_density(size = 0 , alpha = 0.1 , bw = 0.05) +
  scale_y_continuous(breaks = NULL) +
  labs(title = "",
       subtitle = "") +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  facet_wrap(~Type_) +                       #   scales = "free"
  theme(strip.text.x = element_text(size = 16, colour = "black")) +
  theme(axis.text.x = element_text( size = 14))


#################################################################################################

# Illustrate the shrinkage effect

dsim7 %>% mutate(hospital = as.numeric(hospital)) %>%
  ggplot(aes(x = hospital)) +
  geom_hline(yintercept = inv_logit_scaled(median(post$b_Intercept)), linetype = 4, size = 4/2 , color = "red" ) +
  geom_vline(xintercept = c(15, 30, 45), size = 1/4) +

  geom_point(aes(y = p_nopool_reg), color = "orange2" , size = 2.5) +
  geom_point(aes(y = bpool), shape = 1 , size = 4.5) +

  annotate(geom = "text", x = c(8, 15 + 8, 31 + 6 , 46+6 ), y = 1.05, size = 5 ,
           label = c("few patients", "medium patients", "more patients" , "many patients")) +
  scale_x_continuous(breaks = c(1, 16, 32, 48)) +
  coord_cartesian(ylim = c(0, 1.05)) +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text( size = 14))


dev.new(width=20, height=20)
window()

dsim7 %>% mutate(ni = factor(ni)) %>%

ggplot( aes(y=bpool, x=p_nopool_reg , shape=ni, color=ni)) +
  geom_point(size=3) +
  coord_cartesian(ylim = c(0, 1) , xlim = c(0, 1)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  geom_abline(intercept = 0, slope = 1, color="red",  linetype="dashed", size=0.75)


#######################################################################################################################################

# What is the estimation error for the different models?

DD1 <- dsim %>%
        mutate(true_p = inv_logit_scaled(true_a)) %>%
         mutate(index=1:n() , hospital_ = hospital , hospital = factor(hospital)) %>%
      select( true_p , hospital, hospital_ ,  true_a , ni)

dsim9 <- dsim7 %>% select(hospital, index , p_nopool_reg , bnopool , bpool)

DD2 <- DD1 %>% left_join(dsim9 , by = "hospital")

DD3 <- DD2 %>%
       mutate(nopool_error   = abs(p_nopool_reg   - true_p),
         partpool_error = abs(bpool - true_p))



dfline <-
  DD3 %>%
  select(ni, nopool_error:partpool_error) %>%
  gather(key, value, -ni) %>%
  group_by(key, ni) %>%
  summarise(mean_error = mean(value)) %>%
  mutate(x    = c( 1, 16, 31, 46),
         xend = c(15, 30, 45, 60))

DD3 %>%
  ggplot(aes(x = hospital_)) +
  geom_vline(xintercept = c(15.5, 30.5, 45.4),
             color = "white", size = 2/3) +
  geom_point(aes(y = nopool_error), color = "orange2") +
  geom_point(aes(y = partpool_error), shape = 1) +
  geom_segment(data = dfline,
               aes(x = x, xend = xend,
                   y = mean_error, yend = mean_error),
               color = rep(c("orange2", "black"), each = 4),
               linetype = rep(1:2, each = 4)) +
  annotate("text", x = c(15 - 7.5, 30 - 7.5, 45 - 7.5, 60 - 7.5), y = .45, size = 5 ,
           label = c("few patients", "medium patients", "more patients", "many patients")) +
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60)) +
  labs(title = "Estimate error by model type",
       subtitle = "",
       y = "absolute error") +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank(),
        plot.subtitle = element_text(size = 10)) +
  theme(axis.text.x = element_text( size = 14))








