rm(list = ls(all.names = TRUE))
gc()


library(tidyverse)     # For data data mangement
library(ggplot2)          # For graphics
library(ggthemes)               # For graphics
library(rstan)         # rstan
library(easystats)        # Different statistical analysis tools
library(rstanarm)        # Bayesians calculations
library(bayestestR)         # For easy bayesian analysis
library(insight)          #    Easy Access to Model Information for Various Model Objects

# To make calculations fast
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


#______________________________________________________________________________________________________
library(waffle)         # To make waffle chart


wf <- waffle(parts = c(10, 90) , rows = 10, keep = T, size = 2 , reverse = F,  pad = 0,
             legend_pos = "none" , colors = c("red", "blue") )
wf
P<-10
N<-90
TP<-9
FP<-10
TN<-N-FP
Sens<-TP/P
Spec <- TN/N

Prevalence <- P/(P + N)

Prob.test.pos <- Sens*Prevalence + (1-Spec)*(1-Prevalence)

PPV1 <- (Sens*Prevalence)/(Prob.test.pos)
PPV1
PriorD <- PPV1 ;
Prob.test.pos2 <- Sens*PriorD + (1-Spec)*(1-PriorD)
PPV2 <- (Sens*PriorD)/(Prob.test.pos2)
PPV2

T1 <- tibble(Sensitivity = Sens , Prevalence = Prevalence , PPV=PPV1) %>%
  pivot_longer( everything()  ,  names_to = "Var" , values_to = "Val" ) %>% mutate(Test = "Test 1" )
T2 <- tibble(Sensitivity = Sens , Prevalence = PriorD , PPV=PPV2) %>%
  pivot_longer( everything()  ,  names_to = "Var" , values_to = "Val" ) %>% mutate(Test = "Test 2 given test1" )

rbind(T1, T2) %>%
  ggplot( aes(x=Var, y=Val)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits = c("Sensitivity", "Prevalence", "PPV")) +
  facet_wrap(~Test , nrow = 1)

#_____________________________________________________________________________________________________________________
# Estimation
# Estimation
my <- seq(60, 100, by = 0.1)
likelihood <- dnorm(x=88.5, mean = my, sd = 20, log = FALSE)
plot( my, likelihood , type="l" , lty = 1 ,
      xlab="my" , ylab="Likelihood" , main = "Likelihood get X=88.5 when X~N(my, 20)" )
abline(v = 88.5, col="blue", lwd=2, lty=2)
abline(h = dnorm(x=88.5, mean = 88.5, sd = 20, log = FALSE), col="green", lwd=2, lty=2)
text(77.5, 0.015, "Parameter that maximize \n likelihood given x=88.5 is my=88.5", cex = 0.9)


#_________________________________________________________________________________________________________________
# Single parameter example
# Frequentist model
# Log-likelihood.
set.seed(564)
y <- 4 + rnorm(10)
Y <- tibble(y)
sq<- sum(y^2); m<- mean(y)
confint(lm(y~1))

mu<-seq(3, 4,.01)
LogLik<- -1*(sq+10*mu^2-2*10*m*mu )
par(mar=c(5,0,5,0))
dev.new(width=20, height=20)
window()
plot(mu, LogLik, type = "l", lty = 1)
abline(v = 3.525, col="blue", lwd=2, lty=2)
abline(h = -6.06, lwd=2, col="blue", lty=2)


# Bayesian model
library(rethinking)   # Bayesian analysis
aa0 <- quap(
  alist(
    y ~ dnorm( mu ,1) , # normal likelihood
    mu ~ dunif(-5, 5) # uniform prior
  ) ,
  data=list(y) )
precis(aa0)
post <- extract.samples(aa0,n=1e4)
prior <- extract.prior(aa0,n=1e4)
par(mfrow=c(1, 3))
pr <-   dens(prior$mu , main = "Prior")
lik <- plot(mu, LogLik, type = "l", lty = 1, main = "Likelihood")
po <-  post_plot <- dens(post$mu, main = "Posterior" , xlim=c(2, 5))
abline(v = 3.53, col="blue", lwd=2, lty=2, cex = 0.05)

aa <- quap(
  alist(
    y ~ dnorm( mu ,1) , # normal likelihood
    mu ~ dnorm(-1, 0.1) # normal prior. Strong prior belief mu = -1
  ) ,
  data=list(y) )
precis(aa)
post <- extract.samples(aa,n=1e4)
prior <- extract.prior(aa,n=1e4)

par(mar=c(5,0,5,0))
dev.new(width=20, height=20)
window()
par(mfrow=c(1, 3))
pr <-   dens(prior$mu , main = "Prior")
lik <- plot(mu, LogLik, type = "l", lty = 1, main = "Likelihood")
po <-   dens(post$mu, main = "Posterior" , xlim=c(-1.5, 0))
abline(v = -0.59, col="blue", lwd=2, lty=2, cex = 0.05)


M <- stan_glm(y ~ 1, data=Y , chains=3,iter=50000,cores=3,
              prior_intercept = normal(location = 1, scale = 1) ,
              prior_aux = exponential(rate = 1))
prior_summary(M)
plot(M)
posteriors <- tibble( P = insight::get_parameters(M))
ggplot(posteriors, aes(x = P$`(Intercept)`)) +
  geom_density(fill = "orange") +
  geom_vline(xintercept=map_estimate(posteriors$P$`(Intercept)`), color="purple", size=1) +
  geom_vline(xintercept=3.53, color="red", size=1)

ci_hdi <- ci(posteriors$P$`(Intercept)`, method = "HDI", ci = 0.95  )

# What is the bayes factor of a mean level estimated by data and a level of 0 ?

bayesfactor_parameters(M, direction = "two-sided", null = 3)
bayesfactor_parameters(M, direction = "right", null = 3)

library(BEST)
BESTout1g <- BESTmcmc(y, priors=NULL, parallel=FALSE)
plotPostPred(BESTout1g)

#_____________________________________________________________________________________________________________________
# The role of the prior: Shrinkage ("regularization")

set.seed(581)
y <- 6 + 2*rnorm(15)
Y <- tibble(y)
 m<- mean(y)
 a1 <- quap(
   alist(
     y ~ dnorm( mu ,1) , # normal likelihood
     mu ~ dnorm(0, 0.1) # normal prior. Strong prior belief mu = 0
   ) ,
   data=list(y) )
 precis(a1)
 post1 <- extract.samples(a1,n=1e4)
 prior1 <- extract.prior(a1,n=1e4)
  prior1$prior=c("N(0, 0.1)")
  post1$prior=c("N(0, 0.1)")

 a2 <- quap(
   alist(
     y ~ dnorm( mu ,1) , # normal likelihood
     mu ~ dnorm(0, 1) # normal prior. Less strong prior belief mu = 0
   ) ,
   data=list(y) )
 precis(a2)
 post2 <- extract.samples(a2,n=1e4)
 prior2 <- extract.prior(a2,n=1e4)
 prior2$prior=c("N(0, 1)")
 post2$prior=c("N(0, 1)")

 a3 <- quap(
   alist(
     y ~ dnorm( mu ,1) , # normal likelihood
     mu ~ dnorm(0, 5) # normal prior. No strong prior belief mu = 0
   ) ,
   data=list(y) )
 precis(a3)
 post3 <- extract.samples(a3,n=1e4)
 prior3 <- extract.prior(a3,n=1e4)
 prior3$prior=c("N(0, 5)")
 post3$prior=c("N(0, 5)")

 par(mar=c(5,0,5,0))
 dev.new(width=20, height=20)
 window()
 par(mfrow=c(3, 2 ))
   dens(prior1$mu , main = "Prior N(0, 0.1)", xlim = c(-10, 10))
   dens(post1$mu, main = "Posterior N(0, 0.1)" , xlim =c(0, 8) )    #   , xlim=c(-1.5, 0)
   dens(prior2$mu , main = "Prior N(0, 1)" , xlim =c(-10, 10))
   dens(post2$mu, main = "Posterior N(0, 1)" , xlim =c(0, 8))    #   , xlim=c(-1.5, 0)
   dens(prior3$mu , main = "Prior N(0, 5)" , xlim =c(-10, 10))
   dens(post3$mu, main = "Posterior N(0, 5)" , xlim =c(0, 8))    #   , xlim=c(-1.5, 0)



   library(extraDistr)
  H <- tibble( Half_Cauchy_1 = rhcauchy( n = 10000, sigma = 1) ,
               Half_Cauchy_2 = rhcauchy( n = 10000, sigma = 2) ,
               Half_Cauchy_4 = rhcauchy( n = 10000, sigma = 4) ,
   Half_t_3df = rht(n = 10000, nu = 3, sigma = 1) ,
   Half_Normal = rhnorm(n = 10000, sigma = 1) ,
   Exponential = rexp(n = 10000, rate = 1) ) %>%
      pivot_longer(c(Half_Cauchy_1 , Half_Cauchy_2 , Half_Cauchy_4  , Half_t_3df , Half_Normal ,  Exponential ) )

  dev.new(width=20, height=20)
  window()

    H %>%
      ggplot(aes(x = value , color = name)) +
       geom_density() +
       xlim(0, 10) +
      theme(legend.position="bottom")




#_____________________________________________________________________________________________________________________
# Linear regression

iris2 <- tibble(iris) %>% filter(Species == "versicolor")

par(mar=c(5,0,5,0))
dev.new(width=20, height=20)
window()

ggplot(iris2, aes(x=Petal.Length, y=Sepal.Length)) +
  theme_bw() +
  geom_point()

set.seed(564)
models <- tibble(
  a1 = runif(10, 2.5, 2.6),
  a2 = runif(10, 0.6, 0.9) )

ggplot(iris2, aes(x=Petal.Length, y=Sepal.Length)) +
  geom_point() +
  theme_bw() +
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/2)

# Frequentist model

model <- lm(Sepal.Length ~ Petal.Length, data=iris2)
summary(model)
lrtest(model)
confint(model)
dev.new(width=20, height=20)
window()
ggplot(iris2, aes(x=Petal.Length, y=Sepal.Length)) +
  theme_bw() +
  geom_point() +  # This adds the points
  geom_smooth(method="lm")

# Bayesian model
model <- stan_glm(Sepal.Length ~ Petal.Length, data=iris2)
prior_summary(model)
posteriors <- insight::get_parameters(model)
ggplot(posteriors, aes(x = Petal.Length)) +
  geom_density(fill = "orange") +
  geom_vline(xintercept=map_estimate(posteriors$Petal.Length), color="purple", size=1)

map_estimate(posteriors$Petal.Length)
ci_hdi <- ci(posteriors$Petal.Length, method = "HDI", ci = 0.95  )

plot(model)

pd <- p_direction(posteriors$Petal.Length)
1-pd   # Probability theta is => 0 given y (through our posterior distribution)
2*(1-pd)
plot(pd)

Perc_in_ROPE_full_Dist <- rope(posteriors$Petal.Length, range = c(0.95, 1.05), ci=1)
Perc_in_ROPE_full_Dist
plot(Perc_in_ROPE_full_Dist)

model1 <- stan_glm(Sepal.Length ~ 1, data=iris2 , chains=3,iter=50000,cores=3,
                   diagnostic_file = file.path(tempdir(), "df.csv"))
model2 <- stan_glm(Sepal.Length ~ Petal.Length, data=iris2 , chains=3,iter=50000,cores=3,
                   diagnostic_file = file.path(tempdir(), "df.csv"))
prior_summary(model1)
prior_summary(model2)

comparison <- bayesfactor_models(model2,  denominator = model1)
plot(comparison)

#__________________________________________________________________________________________________
# What happens if we have different prior belief about the slope?

# Strong prior belief slope is 2
MA <-  stan_glm(Sepal.Length ~ Petal.Length, data=iris2 , chains=3,iter=50000,cores=3,
                     prior_intercept = normal(location = 6.93, scale = 2.5),
                    prior = normal(location = 2, scale = 0.5),
                    prior_aux = exponential(rate = 1) ,
                  diagnostic_file = file.path(tempdir(), "df.csv"))
# Strong prior belief slope is 0
MB <-  stan_glm(Sepal.Length ~ Petal.Length, data=iris2 , chains=3,iter=50000,cores=3,
                prior_intercept = normal(location = 6.93, scale = 2.5),
                prior = normal(location = 0, scale = 0.5),
                prior_aux = exponential(rate = 1) ,
                diagnostic_file = file.path(tempdir(), "df.csv"))

prior_summary(MA)
prior_summary(MB)
plot(MA)
pMA <- insight::get_parameters(MA) %>% mutate(Prior = "slope = 2")
pMB <- insight::get_parameters(MB) %>% mutate(Prior = "slope = 0")
map_estimate(pMA$Petal.Length)
map_estimate(pMB$Petal.Length)
pMAB <- full_join(pMA, pMB)

ggplot( pMAB , aes(x=Petal.Length, fill=Prior)) +
  geom_density(alpha=0.4) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "bottom") +
  annotate("text", x = 0.5, y =  3 , label = c("ML Estimate: \n 0.82"),  size = 3.5, hjust = 1) +
  geom_vline(xintercept = 0.82, color = "royalblue", size = 1, lty = 2)

comparison <- bayesfactor_models(MB,  denominator = MA)
comparison
#______________________________________________________________________________________

# Make posterior predictions
mdl = brm(  Sepal.Length ~ Petal.Length, data=iris2 , family = gaussian ,
            prior = c(prior(normal(5.9, 2.5), class = Intercept),
                      prior(normal(0, 2.5), class = "b"),
                      prior(exponential(1), class = sigma))   )          #   prior(cauchy(0, 1), class = sigma)

prior_summary(mdl)
describe_posterior(mdl)

dev.new(width=20, height=20)
window()
iris2 %>%
  add_fitted_draws(mdl, n = 100) %>%
  ggplot(aes(x = Petal.Length, y = Sepal.Length)) +
  geom_line(aes(y = .value, group = .draw, alpha = .1)) +
  geom_point(data = iris2) +
  scale_color_brewer(palette = "Dark2")

dev.new(width=20, height=20)
window()
iris2 %>%
  add_predicted_draws(mdl, n = 100) %>%
  ggplot(aes(x = Petal.Length, y = Sepal.Length)) +
  geom_line(aes(y = .prediction, group = .draw, alpha = .1)) +
  geom_point(data = iris2) +
  scale_color_brewer(palette = "Dark2")





