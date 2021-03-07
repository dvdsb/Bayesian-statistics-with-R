
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

# Exercise 1:
# Lets say you plan for an experiment where are to measure the relationship between
# Sepal.Length (Y) and Petal.Length (X) on flowers.

# Sepal.Length  ~ Normal(my, sigma)   Likelihood
#    my = a + b*Petal.Length                Likelihood
#    a ~ N(0, 8)
#    b ~ N(1, 8)
#    sigma ~ student_t(3, 0, 2.5)     (default)

# Your prior for the slope, b ~ N(1, 8), was based on previous studies (a positive relationship )
# You would like to see if you can get realistic predictions from these priors (prior predictive distribution).
# This is done before you have collected data in you experiment (hence, you can use "dummy" data set):
Empty <- tibble(Sepal.Length = 1 , Petal.Length = 1)

library(brms)
library(tidybayes)



prior_ = c(prior(normal(0, 8), class = Intercept),
          prior(normal(1, 8), class = b) )

BPr <-   brm(data = Empty,
             family = gaussian,
             Sepal.Length ~ Petal.Length,
             prior = prior_,
            sample_prior = TRUE ,
         # sample_prior = "only",
             iter = 4000, warmup = 1000, chains = 4, cores = 4,
             seed = 12)

prior_summary(BPr)
plot(BPr)
prior <- prior_samples(BPr) %>%
    pivot_longer(c(Intercept, b, sigma)) %>%
 ggplot( aes(x=value )) +
  geom_density() +
  facet_wrap(~name ,  scales = "free")

prior

# Plot the Prior predictions:
draws_prior <- Empty %>%
  tidyr::expand(Petal.Length = seq(3, 5, by = 0.1)) %>%
  tidybayes::add_fitted_draws(BPr, n = 100) %>%
  mutate( Sepal.Length  = .value)

                          #_______________________________________________________________
                          # This is our "future" data
                          iris2 <- tibble(iris) %>% filter(Species == "versicolor")
                          #_______________________________________________________________

p1 <- ggplot(draws_prior) +
  aes(x = Petal.Length, y = Sepal.Length) +
  geom_line(aes(group = .draw), alpha = .3) +
  theme_bw() +
  geom_point(data = iris2)     # Now we add the data (just get some perspective. In reality this will be available later)

p1

# Here you see that the priors suggests that data can be very extreme

# Q1: What happens if you reduce the SD in priors substantially?

# Do this by trying this:

prior_2 = c(prior(normal(0, 1), class = Intercept),     # For example SD as small as 1
           prior(normal(1, 1), class = b) )

BPr2 <- update(BPr , prior = prior_2)

# Re-do the plot but use prior predictions with the new lower SD

#_____________________________________________________________________________
# Now we perform the study and have our data:
    summary(iris2)
# Now lets estimate the model


Bposterior <- brm(data = iris2,
             family = gaussian,
             Sepal.Length ~ Petal.Length,
          #   prior = prior_,
             sample_prior = TRUE ,
             iter = 4000, warmup = 1000, chains = 4, cores = 4,
             seed = 12)

plot(Bposterior)


# Let us now make posterior predictions

draws_Post <- iris2 %>%
  tidyr::expand(Petal.Length = seq(3, 5, by = 0.1)) %>%
  tidybayes::add_fitted_draws(Bposterior, n = 100) %>%
  mutate( Sepal.Length  = .value)

p11 <- ggplot(draws_Post) +
  aes(x = Petal.Length, y = Sepal.Length) +
  geom_line(aes(group = .draw), alpha = .3, color = "red") +
 # geom_line( data = draws_prior , aes(group = .draw), alpha = .3, color = "green") +   #   Add the prior predictions
  theme_bw() +
  geom_point(data = iris2)     # Now we add the data (since we have it available now)
p11

# The Y-axis becomes not so good because of the huge spread in the prior predictions.
# Q2: Can you re-do the plot but remove the prior predictions, so the scale of the Y-axis becomes easier to look at


#  Make individual predictions from the posterior distribution:
# I´m not 100% sure if this is what makes the difference between ":add_fitted_draws" and "add_predicted_draws",
# but it seems reasonable
dev.new(width=20, height=20)
window()
iris2 %>%
  add_predicted_draws(Bposterior, n = 100) %>%
  ggplot(aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point(aes(x = Petal.Length, y = .prediction) , alpha = 0.2 , color = "red" ,  shape = 1 ,
                      size = 1 , position = position_jitter(w = 0.015, h = 0) ) +
  theme_bw() +
  geom_point(data = iris2 , color = "blue")


#___________________________________________________________________________________________________________

# Exercise 2:

# Lets say are planning for an observational study of the rate of complications after surgery.
# You are interested to see if the rate depends on a frailty score (the high score the more frail are the patients)
# The likelihood is on the form:
   # Number of complications  ~ Poisson(lambda)   Likelihood
   #    a = log(lambda)                 Likelihood
   #    a ~ N(0, 4)
   #    b ~ N(0.5, 4)
# b is chosen based on previous studies where a positive relationship has been observed. I choose a large SD´s
# to reflect my uncertainty however.

# Simulate data:
   a <- 0.1 ; b <- 0.25
   N <- 14          # Number of patients
   set.seed(657)
  Data <- tibble( frailty = c(4, 1, 10, 3, 8, 2, 5, 9, 7, 6, 5, 10, 3, 2),
                  lambda = a + b * frailty ,
                  Complications = rpois(N , lambda = lambda))

  Data_ <- tibble( frailty = 1,
                     Complications = 1)

  library(brms)
  library(tidybayes)


  #   We can use sample_prior = "only" to have brms ignore the data and sample from the prior distribution.
  BPr <-   brm(data = Data_,
               family = poisson,
               Complications ~ frailty,
               prior = c(prior(normal(0, 4), class = Intercept),
                         prior(normal(0.5, 4), class = b)),
             sample_prior = TRUE ,
               iter = 4000, warmup = 1000, chains = 4, cores = 4,
               seed = 12)


  draws_prior <- Data_ %>%
    tidyr::expand(frailty = seq(0, 10, by = 1)) %>%
    tidybayes::add_fitted_draws(BPr, n = 100) %>%
    mutate( Complications  = .value)

  # We got so extreme values so we cut it at 50 before we do the graph:
  draws_prior2 <-  draws_prior %>% filter(Complications < 50)

  p1 <- ggplot(draws_prior2) +
    aes(x = frailty, y = Complications) +
    geom_line(aes(group = .draw), alpha = .3) +
    theme_bw() +
    geom_point(data = Data)     # Now we add the data (just get some perspective)

  p1
  # This looks insane. Maybe I did something wrong. If this is correct,
  # it means the priors can give rise to very extreme predictions.
  # I´m not sure how a correlation between the parameters values is accounted for.

#________________________________________________________________________________________
# Now use the data to caclulate the posterior distribution

  BPost <-   brm(data = Data,
               family = poisson,
               Complications ~ frailty,
               prior = c(prior(normal(0, 1), class = Intercept),
                         prior(normal(0.5, 1), class = b)),
               sample_prior = TRUE ,
               iter = 4000, warmup = 1000, chains = 4, cores = 4,
               seed = 12)



  postprior <- posterior_samples(BPost) %>%
    pivot_longer(c(b_Intercept ,  b_frailty , prior_Intercept , prior_b)) %>%
  ggplot( aes(x=value )) +
    geom_density() +
    facet_wrap(~name ,  scales = "free")
  postprior


  draws_Post <- Data %>%
    tidyr::expand(frailty = seq(0, 10, by = 1)) %>%
    tidybayes::add_fitted_draws(BPost, n = 100) %>%
    mutate( Complications  = .value)

  # Model predictions of the mean level

  p11 <- ggplot(draws_Post) +
    aes(x = frailty, y = Complications) +
    geom_line(aes(group = .draw), alpha = .3, color = "red") +
    theme_bw() +
    geom_point(data = Data)     # Now we add the data (since we have it available now)
  p11

  # Individual posterior predictions

  dev.new(width=20, height=20)
  window()
  Data %>%
    add_predicted_draws(BPost, n = 100) %>%
    ggplot(aes(x = frailty, y = Complications)) +
    geom_point(aes(x = frailty, y = .prediction) , alpha = 0.6 , color = "red" ,  shape = 1 ,
               size = 0.5 , position = position_jitter(w = 0.1, h = 0) ) +
    theme_bw() +
    geom_point(data = Data , color = "blue")

#______________________________________________________________________________________________________________


