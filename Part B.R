rm(list = ls(all.names = TRUE))
gc()


library(tidyverse)     # For data data mangement
library(ggplot2)          # For graphics
library(ggthemes)               # For graphics

#___________________________________________________________________________________________________
# Estimation
library(rstan)         # rstan
library(easystats)        # Different statistical analysis tools
library(rstanarm)        # Bayesians calculations
library(bayestestR)         # For easy bayesian analysis
library(insight)          #    Easy Access to Model Information for Various Model Objects

# To make calculations fast
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(7690)
D <- tibble(y = rbinom(20, 1, 0.5))
table(D)
m <- glm(y~ .,  data = D , family = "binomial" )
summary(m)

# Maximum Likelihood estimator (proportion of heads)
ML <- exp(m$coefficients)/(1+exp(m$coefficients))   # Inv. Logit
ML

theta <- seq(0.2, 0.8, by = 0.025)
likelihood <- dbinom(x=9, size=20, prob=theta)
plot( theta, likelihood , type="l" , lty = 1 ,
      xlab="parameter theta" , ylab="Likelihood" , col = "red" , main = "Likelihood get 9 heads in 20 tosses")
text(0.25, 0.15, "Likelihood maximized at theta = 0.45", cex = 1.1, pos=4)


# Bayesian Logistic regression:
model <- stan_glm(y ~ 1 , data=D ,
                  family = binomial(link = "logit"))

prior_summary(model)     # Default priors   (scale = standard deviation, not the variance)

PDs <- as_tibble(insight::get_parameters(model)) %>%
  mutate(PD = `(Intercept)` , PD_ = exp(PD)/(1+exp(PD))) %>%
  dplyr::select(PD, PD_)

mean <- exp(mean(PDs$PD))/(1 + exp(mean(PDs$PD)))     # Inverse logit to get back to probability scale
median <- exp(median(PDs$PD))/(1 + exp(median(PDs$PD)))
map <-  exp(map_estimate(PDs$PD))/(1 + exp(map_estimate(PDs$PD)))


par(mar=c(5,0,5,0))
dev.new(width=20, height=20)
window()

ggplot(PDs, aes(x = PD_ ) ) +
  geom_density(fill = "orange") +
  geom_vline(xintercept=map, color="blue", size=1) +
  labs(title="Posterior distribution")  +
  annotate("text",
           x = c(0.325, 0.325, 0.325, 0.325),
           y =  c(3, 2.75, 2.5, 2.25 ),
           label =   c(paste("ML Estimate:", round(ML, 2)),
                       paste("Mean PD:", round(mean, 2)),
                       paste("Median PD:", round(median, 2)),
                       paste("MAP PD:", round(map, 2))) ,   size = 5, hjust = 1)

#_____________________________________________________________________________________________________
# Interval estimation
# Frequentist Confience interval
exp(confint(m))/(1+exp(confint(m)))
confint(m, level = 0.89)

# Bayesian Credible interval
# Highest Density Interval (HDI) is one credible interval

ci_hdi <- ci(PDs$PD_, method = "HDI")

ggplot(PDs, aes(x = PD_) ) +
  geom_density(fill = "orange") +
  labs(title="Posterior distribution with 89% Highest Density Interval ")  +
  annotate("text",
           x = c( 0.3, 0.7 , 0.55),
           y =  c( 1.75, 1.75,  1.75  ),
           label =   c( "0.28", "0.64" , "Probability is 89% \n that theta is between\n  0.28 and 0.64") ,
           size = c(5, 5, 4), hjust = 1)  +
  geom_vline(xintercept = ci_hdi$CI_low, color = "royalblue", size = 1, lty = 2) +
  geom_vline(xintercept = ci_hdi$CI_high, color = "royalblue", size = 1, lty = 2)

#___________________________________________________________________________________________________
# Hypothesis testing
# Frequentist
Pvalue <- summary(m)$coefficients[4]   # P-value two-sided test
Pvalue/2      # P-value one-sided test
# This is evidence against H0: theta=0.5

# Visual illustration of a p-value:
DN <- tibble(X = rnorm(50000, 0, 1))
p <-  ggplot(DN, aes(X)) +
  geom_density(fill="grey") +
  geom_vline(xintercept=0)
d <- ggplot_build(p)$data[[1]]

# Two-sided test
p2 <- p + geom_area(data = subset(d, x < -1.6), aes(x=x, y=y), fill="red") +
  annotate("text", x=4, y=0.25,
           label = "P-value: \n Probability to get data equal to \n that in the current sample\n  (or more exteme)
                                                        given H0 true", size = 3, hjust = 1)

p3 <- p2 + geom_area(data = subset(d, x > 1.6), aes(x=x, y=y), fill="red")

par(mar=c(5,0,5,0))
dev.new(width=20, height=20)
window()
p3
# One-sided test
p2

# Bayesian
# Probability of Direction (pd)
# Since map = -0.16 (approx) then pd gives us probability that theta is < 0 given y (through our posterior distribution):
pd <- p_direction(PDs$PD)
1-pd   # Probability theta is => 0 given y (through our posterior distribution)
2*(1-pd)  #


# Illustration:
DN <- tibble(X = rnorm(50000, -1, 1))
p <-  ggplot(DN, aes(X)) +
  geom_density(fill="grey") +
  geom_vline(xintercept=0)
d <- ggplot_build(p)$data[[1]]
p <- p + geom_area(data = subset(d, x <=0 ), aes(x=x, y=y), fill="red") +
  annotate("text", x=c(-2.5), y=c(0.2),
           label = c("Part of the distribution \n that is below zero" )
           , size = 3, hjust = 1)
dev.new(width=20, height=20)
window()
p
# This is very similar to one-sided frequentist p-value:
# A one-sided p-value would be
One_sided_Pvalue <- Pvalue/2          # Probability achieve proportion = 0.45 or less assuming H0: theta => 0.5


Pvalue

posterior <- rnorm(1000, mean = -2, sd = 1)
p_direction(posterior)


# ROPE
# For logistic regression often this range is used:
rope_value <- rope_range(model)
rope_value
thetaNull=0.5
log(thetaNull/(1-thetaNull))
# Let ROPE be 0.18 around log-odds. This corresponds to the following around theta:
exp(-0.18)/(1+exp(-0.18))
exp(0.18)/(1+exp(0.18))

Perc_in_ROPE_Of_HDI <- rope(PDs$PD, range = rope_value, ci=0.89)
Perc_in_ROPE_Of_HDI

Perc_in_ROPE_full_Dist <- rope(PDs$PD, range = rope_value, ci=1)
Perc_in_ROPE_full_Dist

dev.new(width=20, height=20)
window()

ggplot(PDs, aes(x = PD_) ) +
  geom_density(fill = "orange") +
  labs(title="Posterior distribution with 89% Highest Density Interval ")  +
  annotate("text",
           x = c( 0.3, 0.7 , 0.55),
           y =  c( 1.75, 1.75,  1.75  ),
           label =   c( "0.28", "0.64" ,
                        "ROPE:0.46; 0.54 \n 32% of the \n 89% HDI \n includes ROPE \n 29% of whole distribution \n includes ROPE") ,
           size = c(4, 4, 4), hjust = 1)  +
  geom_vline(xintercept = ci_hdi$CI_low, color = "royalblue", size = 1, lty = 2) +
  geom_vline(xintercept = ci_hdi$CI_high, color = "royalblue", size = 1, lty = 2) +
  geom_vline(xintercept = exp(-0.18)/(1+exp(-0.18)), color = "red", size = 1, lty = 2) +
  geom_vline(xintercept = exp(0.18)/(1+exp(0.18)), color = "red", size = 1, lty = 2)

# A default special plot from the easystat package:
# Since it says CI = 100% this means that the shaded area is 29% of the whole distribution
plot(Perc_in_ROPE)   # This is on the log-odds scale

#_________________________________________________________________________________________________________
# Bayes factor

# Bayes factor and p-value often indicate similar conclusions
d <- tibble(pvalue = seq(0, 0.1, by = 0.0025),
            Bayes_Factor = 1/pvalue)

dev.new(width=20, height=20)
window()

plot(d$pvalue, d$Bayes_Factor  , yaxt='n')
text(0.01, 120, "Large BF in favor of H1 \n coincide with small p-value", cex = 1.1, pos = 4)
text(0.06, 80, "Small BF in favor of H1 \n coincide with large p-value", cex = 1.1, pos=4)


set.seed(6754)
D2 <- tibble(y = rbinom(20, 1, 0.4))
table(D2$y)
ml_ <- log((mean(D2$y)/(1-mean(D2$y))))

MDL_ <- stan_glm(y ~ 1 , data=D2 ,
                 family = binomial(link = "logit") , prior_intercept = normal(location = 0, scale = 1))

prior_summary(MDL_)

PDs2 <- as_tibble(insight::get_parameters(MDL_)) %>%
  mutate(PD = `(Intercept)` ) %>%  dplyr::select(PD)

lwr_rope <- log((0.45/(1-0.45)))
upr_rope <- log((0.55/(1-0.55)))

BF_ <- bayesfactor_parameters(MDL_, null = c(lwr_rope, upr_rope))
BF_

# Built in plotting:
plot(BF_)

# How to interpret the value of the Bayes factor?
library(effectsize)
effectsize::interpret_bf(as.numeric(BF_))

# We can do the plots ourseleves again of we want:

DN <- tibble(X = rnorm(50000, 0, 1))
p <-  ggplot(DN, aes(X)) +
  geom_density(fill="grey") +
  theme_bw()
d <- ggplot_build(p)$data[[1]]
p <- p + geom_area(data = subset(d, x > lwr_rope & x < upr_rope ), aes(x=x, y=y), fill="red") +
  labs(title="Prior distribution (N(0, 1))" , size = 10) +
  xlim(-3, 3) +
  annotate("text", x=c(-2, -2), y=c(0.175, 0.1),
           label = c("P(H0)", "P(H1)" ) , col = c("red" , "grey40"), size = 4.25, hjust = 1)

# Plot the posterior
pp <- ggplot(PDs2, aes(x = PD) ) +
  geom_density(fill = "orange") +
  theme_bw()
d <- ggplot_build(pp)$data[[1]]
pp <- pp + geom_area(data = subset(d, x > lwr_rope & x < upr_rope ), aes(x=x, y=y), fill="red") +
  geom_vline(xintercept=0 , lty = 2) +
  geom_vline(xintercept=ml_ , lty = 2) +
  labs(title="Posterior distribution" , size = 10) +
  xlim(-3, 3) +
  annotate("text", x=c(-2, -2 , -0.8 ), y=c(0.7, 0.55, 0.80),
           label = c("P(H0|y)", "P(H1|y)" , "-0.62") , col = c("red" , "orange", "black"), size = 4.25, hjust = 1)

dev.new(width=20, height=20)
window()
see::plots(p, pp, n_rows = 2)


#_______________________________________________________________________________________________________________

# A function that plots both the prior and posterior and calculates Bayes factors

FNK = function(N, mypr, spr,  mypo, spo , A , B)
{
  set.seed(656877)
  D <- tibble(Prior = rnorm(N, mypr, spr), Post = rnorm(N, mypo, spo))

  bf2 <- as.numeric(bayesfactor_parameters( posterior = D$Post, prior = D$Prior, direction = "two-sided", null = 0))
  bfl <- as.numeric(bayesfactor_parameters( posterior = D$Post, prior = D$Prior, direction = "left", null = 0))
  bfr <- as.numeric(bayesfactor_parameters( posterior = D$Post, prior = D$Prior, direction = "right", null = 0))
  pvalue <- as.numeric(t.test(D$Post)[3])


  # plot the prior
  p <-  ggplot(D, aes(Prior)) +
    geom_density(fill="grey") +
    theme_bw()
  d <- ggplot_build(p)$data[[1]]
  p <- p + geom_area(data = subset(d, x > -0.075 & x < 0.075 ), aes(x=x, y=y), fill="red") +
    labs(title = A , size = 10) +
    xlim(quantile(D$Prior)[1] ,  quantile(D$Prior)[5] )

  # Plot the posterior
  pp <-  ggplot(D, aes(Post)) +
    geom_density(fill="grey") +
    theme_bw()
  d <- ggplot_build(pp)$data[[1]]
  pp <- pp + geom_area(data = subset(d, x > -0.075 & x < 0.075 ), aes(x=x, y=y), fill="red") +
    labs(title = B , size = 10) +
    xlim(quantile(D$Prior)[1] ,  quantile(D$Prior)[5] ) +
    annotate("text",
             x = c(-1, -1, -1, -1),
             y =  c(0.35, 0.3, 0.25, 0.175 ),
             label =   c(paste("Two-sided:", round(bf2, 2)),
                         paste("Left:", round(bfl, 2)),
                         paste("Right:", round(bfr, 2)),
                         paste("P-value:", round( pvalue, 2))) ,   size = 4.25, hjust = 1)


  ppp <- see::plots(p, pp, n_rows = 2)

}

A_1 <- FNK(N = 10000, mypr = 0 , spr = 1 , mypo = 0 , spo = 1 , A = "Prior: N(0, 1)", B = "Posterior: N(0, 1)")
A_1 <- FNK(N = 10000, mypr = 0 , spr = 1 , mypo = 1 , spo = 1 , A = "Prior: N(0, 1)", B = "Posterior: N(1, 1)")
A_1 <- FNK(N = 10000, mypr = 0 , spr = 1 , mypo = -1 , spo = 1 , A = "Prior: N(0, 1)", B = "Posterior: N(-1, 1)")

dev.new(width=20, height=20)
window()

plot(A_1)

#   You can also use the plotting function:
Prr <- rnorm(10000, 0, 1)
Pst <- rnorm(10000, -1, 1)
bf1<-bayesfactor_parameters( posterior = Pst, prior = Prr, direction = "left", null = 0)
plot(bf1)

#_____________________________________________________________________________________
# Bayes factors for model comparison

set.seed(4345)
Dx <- tibble( x = runif(70, 0, 3),
              lgt = 0.1 + 0.1*x ,
              p = exp(lgt)/(1+exp(lgt)) ,
              Disease = factor(rbinom(70, 1, p)),
              Smoking = factor(rbinom(70, 1, 0.5)))

dev.new(width=20, height=20)
window()
p <- ggplot(Dx, aes( Disease, x ))
p + geom_boxplot() +
  coord_flip()

p <- ggplot(Dx, aes( Smoking, x ))
p + geom_boxplot() +
  coord_flip()


# Disease:
# Model 1: Intercept Only (a)
# Model 2: a+b*X1

#Frequenist model:
F1 <- glm(Disease ~1 , family = binomial(link = "logit"), data = Dx)
F2 <- glm(Disease ~x , family = binomial(link = "logit"), data = Dx)

# Likelihood ratio test.  https://api.rpubs.com/tomanderson_34/lrt
library(lmtest)
A <- logLik(F1)
B <- logLik(F2)
(teststat <- -2 * (as.numeric(A)-as.numeric(B)))
(p.val <- pchisq(teststat, df = 1, lower.tail = FALSE))

M1 <- stan_glm(Disease ~ 1 , data=Dx , family = binomial(link = "logit") , seed = 139,
               chains=3,iter=50000,cores=3,
               diagnostic_file = file.path(tempdir(), "df.csv") )
M2 <- stan_glm(Disease ~ x , data=Dx , family = binomial(link = "logit") , seed = 7856,
               chains=3,iter=50000,cores=3,
               diagnostic_file = file.path(tempdir(), "df.csv") )
get_priors(M1)
get_priors(M2)
comparison <- bayesfactor_models(M2,  denominator = M1)
comparison
effectsize::interpret_bf(as.numeric(comparison))


# Smoker:
# Model 1: Intercept Only (a)
# Model 2: a+b*X1

#Frequenist model:
F1 <- glm( Smoking ~1 , family = binomial(link = "logit"), data = Dx)
F2 <- glm( Smoking ~x , family = binomial(link = "logit"), data = Dx)
A <- logLik(F1)
B <- logLik(F2)
(teststat <- -2 * (as.numeric(A)-as.numeric(B)))
(p.val <- pchisq(teststat, df = 1, lower.tail = FALSE))

M11 <- stan_glm(Smoking ~ 1 , data=Dx , family = binomial(link = "logit") , seed = 139,
                chains=3,iter=50000,cores=3,
                diagnostic_file = file.path(tempdir(), "df.csv") )
M21 <- stan_glm(Smoking ~ x , data=Dx , family = binomial(link = "logit") , seed = 7856,
                chains=3,iter=50000,cores=3,
                diagnostic_file = file.path(tempdir(), "df.csv") )
get_priors(M11)
get_priors(M21)
comparison <- bayesfactor_models(M21,  denominator = M11)
comparison
effectsize::interpret_bf(as.numeric(comparison))


#______________________________________________________________________________________________________

# Posterior predictive distribution

#   https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html#model
#   https://solomonkurz.netlify.app/post/make-rotated-gaussians-kruschke-style/

# Model
library(brms)
library(tidybayes)
library(modelr)

set.seed(543)
n = 20
ABC =
  tibble(
    treatment = rep(c("A","B","C"), n),
    response = rnorm(n * 3, c(0,1,2), 0.5) )

m = brm(  response ~ treatment,  family = gaussian , data = ABC ,
           prior("student_t(3, 0.9, 2.5)", class = "b") )
prior_summary(m)

dev.new(width=20, height=20)
window()

ABC %>%
  data_grid(treatment) %>%
  add_fitted_draws(m, dpar = c("mu", "sigma")) %>%
  sample_draws(30) %>%
  ggplot(aes(y = treatment)) +
  theme_bw() +
  stat_dist_slab(aes(dist = "norm", arg1 = mu, arg2 = sigma),
                 slab_color = "gray10", alpha = 1/10, fill = NA
  ) +
  geom_point(aes(x = response), data = ABC, shape = 21, fill = "#9ECAE1", size = 2)


#______________________________________________________________________________

DAT <- tibble( X = rnorm(25, 3, 1) , Y = 0.5 + 0.5*X  + rnorm(25, 0, 1)  )

mdl = brm(  Y ~ X , family = gaussian ,
            prior("student_t(10, 0, 1)", class = "b"),  data = DAT)

prior_summary(mdl)

dev.new(width=20, height=20)
window()
DAT %>%
  add_fitted_draws(mdl, n = 100) %>%
  ggplot(aes(x = X, y = Y)) +
  geom_line(aes(y = .value, group = .draw, alpha = .1)) +
  geom_point(data = DAT) +
  scale_color_brewer(palette = "Dark2")

dev.new(width=20, height=20)
window()
DAT %>%
  add_predicted_draws(mdl, n = 100) %>%
  ggplot(aes(x = X, y = Y)) +
  geom_line(aes(y = .prediction, group = .draw, alpha = .1)) +
  geom_point(data = DAT) +
  scale_color_brewer(palette = "Dark2")




