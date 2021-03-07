
rm(list = ls(all.names = TRUE))
gc()

#Instructions:
#Run the code step by step and try to get an overall understanding of what
#it does and that you also understand the output.
#Make minor modifications of the code in order to address the questions below.
#Write down short comments in response to the questions.

#_____________________________________________________________________________________________________

# Load required libraries
library(tidyverse)     # For data data mangement
library(ggplot2)          # For graphics
library(ggthemes)               # For graphics
library(easystats)        # Different statistical analysis tools
library(insight)          #    Easy Access to Model Information for Various Model Objects

# To make calculations fast
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


#______________________________________________________________________________________________________

# Exercise 1: Analysing binary outcomes

# Go back to lab 1 , Exercise 1.
# Create data set A0 and thereafter A1 by running that code again. .

# Now estimate a Bayesian logistic regression
# Let´s say we first use defult priors from stan_glm (N(0, 2.5))

library(rstanarm)

Bay_M1 <-  stan_glm(Y ~ X, data = A1 , family = binomial(link = "logit"))
prior_summary(Bay_M1)   # Check the priors turned out correct

plot(Bay_M1)
prm<-parameters(Bay_M1)
exp(prm[2,2])   # Odds ratio

# Use rethinking package
library(rethinking)
Bay_M2 <- quap( alist(
  Y ~ dbinom( 1 , p ) ,
  logit(p) <- a + b*X ,
  a ~ dnorm( 0 , 2.5 ) ,
  b ~ dnorm( 0 , 2.5 ) ) ,  data=A1 )
summary(Bay_M2)
exp(precis(Bay_M2)[2, 1])    # Odds ratio


# Use brms package
library(brms)
Bay_M3 <-   brm(data = A1,
                family = bernoulli,
                Y ~ X,
                prior = c(prior(normal(0, 2.5), class = Intercept),
                          prior(normal(0, 2.5), class = b)),
                iter = 4000, warmup = 1000, chains = 4, cores = 4,
                seed = 12)

#Every parameter is summarized using the mean (Estimate) and the standard
#deviation (Est.Error) of the posterior distribution as well as two-sided 95% Credible intervals
#(l-95% CI and u-95% CI) based on quantiles. The Eff.Sample value is an estimation of the
#effective sample size; that is the number of independent samples from the posterior distribution
#that would be expected to yield the same standard error of the posterior mean as is obtained
#from the dependent samples returned by the MCMC algorithm.
#The Rhat value provides information on the convergence of the algorithm (cf., Gelman and Rubin, 1992). If Rhat is
#considerably greater than 1 (i.e., > 1.1), the chains have not yet converged and it is necessary
#to run more iterations and/or set stronger priors.

Bay_M3


prm3<-parameters(Bay_M3)
exp(prm3[2,2])    # Odds ratio
prior_summary(Bay_M3)

# Q1: If you compare the Median Posterior value of the parameter with the frequentist estimate in Lab1.
#     What is you conclusion?

# Calculate the bayesfactor

#________________________________________________________________________________________________
# Exercise 2:

# Go back to lab 1 , Exercise 2.
# Create the data again:

set.seed(5653)
Data <- tibble( Subject = seq(1:160),
                Liver_condition =rbinom(160, 1, 0.15),
                DrugConc = 0.1 * Liver_condition + 1.7*rnorm(160),
                Sex = rbinom(160, 1, 0.4),            # 0 = male, 1 = female
                logit = -0.6 + 0.7*DrugConc + 0.3*Sex + 0.5* DrugConc*Sex  ,
                p = exp(logit)/(1+exp(logit)),
                AE = rbinom(160, 1, p)) %>%
  mutate(Sex = factor(Sex, labels = c("Male", "Female")),
         Liver_condition = factor(Liver_condition, labels = c("No", "Yes")),
         AE = factor(AE, labels = c("No AE", "AE"))) %>%
  dplyr::select(-logit)


# Estimate a corresponding Bayesian model. Present the MAP and HDI.
# Try to display the distribution graphically

library(rstanarm)

BML <- stan_glm(DrugConc ~ Liver_condition  , data = Data ,
                chains=3,iter=50000,cores=3,
                prior_intercept = normal(location = 0, scale = 2),
                prior = normal(location = 0, scale = 2)  )
prior_summary(BML)   # Check the priors turned out correct
P = insight::get_parameters(BML)  # Get predictions
map_estimate(P$Liver_condition)
ci(P$Liver_condition, method = "HDI", ci = 0.95  )

plot(BML)

# Q1: Estimate the same model as above, but add Sex to it. Estimate the probability that the
# parameters are above (or below) zero with
result <- p_direction(BML)
plot(result)

#_____________________________________________________________________________________________________
# Exercise 3:

# Go back to lab 1 , Exercise 3.
# Create the data again:

N <- 10         # Sample size. This can be varied
delta <- 0.5      # True mean difference. This can be varied
set.seed(4535)
Data <- tibble(A = rnorm(N, 0, 1), B = delta + rnorm(N, 0, 1)) %>%
  pivot_longer(everything() , names_to = "Group", values_to = "Y") %>%
  mutate(Group = factor(Group))

# Estimate bayesian model:

BML <- stan_glm(Y ~ Group , data = Data ,
                chains=3,iter=50000,cores=3,
                prior = normal(location = 0, scale = 2)  )

# Plot the prior and the posterior distribution for the difference.
# Try different values of location and scale for the prior for the difference

result <- p_direction(BML)
plot(result, priors = TRUE)
# Plot the posterior and prior distribution
# Q1: What happens to the relationship between prior, posterior when you vary the prior?
# Try also to vary the sample size.
# Q2: What happens to the "Probability of Direction"?

# Go to https://rpsychologist.com/d3/bayes/
#  Try a few values for the specifications, and see if you get a similar impression as in the experimentation above


# Calculate the Bayes factor for assessing whether it is reasonable to assume the difference is zero
BF_ <- bayesfactor_parameters(BML, null = 0)
BF_
plot(BF_)
# What is the degree of evidence against H0: difference = 0 ?

#_______________________________________________________________________________________

# Exercise 4:
  # Go back to lab 1 , Exercise 4.
  # Create the data again:

N <- 30         # Sample size. This can be varied
delta <- 0.5      # True mean difference. This can be varied

Data <- tibble(subj = seq(1:N) ,
               A = rep(c(-1, 0, 1), times = 10),
               B = rep(c(0, 1), times = 15),
               Y = 1 + 0.5*A - 0.75*B + 1*A*B + rnorm(N, 0, 1)) %>%
  mutate(A = factor(A , labels =  c("A1", "A2", "A3")),
         B = factor(B , labels =  c("B1", "B2")))

model_a <- stan_glm(Y~A + B + A*B , data = Data ,
                    chains=3,iter=50000,cores=3,
                    prior = normal(location = 0, scale = 2)  )
prior_summary(model_a)

# Show the posterior medians and credibility intervals graphically

# Use brms package
library(brms)
model_aa <-   brm(data = Data,
                family = gaussian,
                Y ~ A + B + A*B,
                prior = c(prior(normal(0, 2), class = Intercept),
                          prior(normal(0, 2), class = b)),
                iter = 4000, warmup = 1000, chains = 4, cores = 4,
                seed = 12)

parameters(model_aa)
plot(model_aa)


#____________________________________________________________________________________________________

# Exercise 5:
# Go back to lab 1 , Exercise 5.
# Create the data again:

# Create an "not in" operator:
'%!in%' <- function(x,y)!('%in%'(x,y))

cars_ <- cars %>%
  mutate(index = seq(1:n()) ,        # Create index variable to make selections from
         SET = case_when(index %!in% c(5, 8, 14, 20, 26, 31, 38, 41, 44, 49) ~ "Train" ,
                         index %in% c(5, 8, 14, 20, 26, 31, 38, 41, 44, 49) ~ "Test")  )
#  Create train data and test data
train <- cars_ %>%
  filter(SET == "Train")
test <- cars_ %>%
  filter(SET == "Test")

# Estimate a bayesian lienar regression:

# Use a vague prior (large sigma) to allow data have strong influence
library(brms)
Bm <-   brm(data = train,
                family = gaussian,
                dist ~ speed,
                prior = c(prior(normal(0, 10), class = Intercept),
                          prior(normal(0, 10), class = b)),
                iter = 4000, warmup = 1000, chains = 4, cores = 4,
                seed = 12)
prior_summary(Bm)
plot(Bm)

# Another model with much shrinkage for the slope:
#    Here you see what a horseshow prior look like:
#    https://images.app.goo.gl/bmjNJVsLMJd9yaYBA

Bm2 <-   brm(data = train,
             family = gaussian,
             dist ~ speed,
             prior = c(prior(normal(0, 10), class = Intercept),
                       prior(horseshoe(df = 3, par_ratio = 0.1), class = b)),
             iter = 4000, warmup = 1000, chains = 4, cores = 4,
             seed = 12)
prior_summary(Bm2)


#   Visualize the results:

library(tidybayes)
P1 <- Bm %>%
  spread_draws(b_speed) %>%
  ggplot(aes(y = NULL, x = b_speed)) +
  stat_halfeye()
P2 <- Bm2 %>%
  spread_draws(b_speed) %>%
  ggplot(aes(y = NULL, x = b_speed)) +
  stat_halfeye()

see::plots(P1, P2, n_rows = 2)

# Calculate the posterior predictive distribution and plot it.
# Make posterior predictions:


dev.new(width=20, height=20)
window()

library(modelr)
PLT <- train %>%
  data_grid(speed = seq_range(speed, n = 51)) %>%
  add_fitted_draws(Bm) %>%
  ggplot(aes(x = speed, y =dist)) +
  stat_lineribbon(aes(y = .value)) +
  geom_point(data = train) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2")

PLT2 <- train %>%
  data_grid(speed = seq_range(speed, n = 51)) %>%
  add_fitted_draws(Bm2) %>%
  ggplot(aes(x = speed, y =dist)) +
  stat_lineribbon(aes(y = .value)) +
  geom_point(data = train) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2")

see::plots(PLT, PLT2, n_columns =  2)

# Similar to the plots above:

PLTx <- train %>%
  data_grid(speed = seq_range(speed, n = 51)) %>%
  add_fitted_draws(Bm , n = 50) %>%
  mutate(model_1 = .value)
PLTxx <- train %>%
  data_grid(speed = seq_range(speed, n = 51)) %>%
  add_fitted_draws(Bm2 , n = 50) %>%
  mutate(model_2 = .value)

 rbind(PLTx , PLTxx) %>%
 ggplot(aes(x = speed, y =dist)) +
  theme_bw() +
  geom_line(aes(y = model_1, group =  .draw), alpha = .3, color = "red" ) +
  geom_line(aes(y = model_2, group =  .draw), alpha = .3, color = "blue" ) +
  geom_point(data = train) +
  scale_color_brewer(palette = "Dark2")


 # Q1: Can you see any difference in slopes between the two models? Think about the reason why they might differ

 A <- train   # test
 A2 <- A %>%
            mutate(pred1 = predict(Bm, newdata = A)[,1] ,
                    pred2 = predict(Bm, newdata = A)[,1] ,
                   resid = dist - pred1 ,
                   resid2 = dist - pred2)
 MSE <- mean(A2$resid^2)
 MSE2 <- mean(A2$resid2^2)
MSE ; MSE2

# Q2: Make a brief comparison between the models.




#___________________________________________________________________________________________



















