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

#_________________________________________________________________________________________________
# The components of a probability / statistical model

#1. Example of a probability model:
#  a) The probability distribution
set.seed(3435)
DAT1 <- tibble(X = rnorm(n=10000, 0, 1))
P1<-   DAT1 %>% ggplot(aes(x=X)) +
  theme_bw() +
  geom_density(alpha=0.4) +
  theme(legend.position = "none") +
  annotate("text", x = c(-0.5), y = c(0.30),
           label =  "Probability distribution"  ,
           size = 6, hjust = 1) +
  theme(axis.text.x=element_text(colour = "white") ) +
  annotate("text",
           x = c(0.2, 0.2),
           y =  c(0, 0.1),
           label =   c(expression("the mean:" ~ mu), expression("the variance:" ~ sigma^2)  ) ,
           size = 6, hjust = 1) +
  theme(axis.text.x=element_text(colour = "white") )
#  b) Functional form (curvature) of the conditional mean
#  c) The parameter values
set.seed(3435)
DD <- tibble (X = rnorm(n=100, 0, 1) , X2 = X**2 ,
              Mean = 5 + 1.25*X - 0.9*(X**2) ,
              Y = Mean + rnorm(n=100, 0, 1) )
P2<-    DD %>%   ggplot(aes(x=X, y=Y)) +
  geom_point() +
  geom_line(aes(x=X, y=Mean)) +
  theme_bw() +
  annotate("text",
           x = c(-0.5),
           y =  c(0  ),
           label =   expression("Conditional mean" ~ mu: beta[0]+beta[1]*X+beta[2]*X^2 )  ,
           size = 6, hjust = 0)  +
  theme(axis.text.x=element_text(colour = "white") ,
        axis.text.y=element_text(colour = "white"))
par(mar=c(5,0,5,0))
dev.new(width=20, height=20)
window()
see::plots(P1, P2 , n_rows=1)

#_________________________________________________________________________________________________
# Example hos probability distributions change when their parameter values are changed.
DAT1 <- tibble(X = c(rnorm(n=10000, 0, 1), rnorm(n=10000, 2, 1)), Z = c(rep("A", times=10000), rep(rep("B", times=10000) )))
P1 <- DAT1 %>% ggplot(aes(x=X, fill=Z)) +
  geom_density(alpha=0.4) +
  theme_void() +                        # This theme makes the plot minimal
  theme(legend.position = "none") +
  annotate("text",
           x = c(-0.5, 2),
           y =  c(0.10, 0.10  ),
           label =  c( expression(mu[1]) , expression(mu[2])   )  ,
           size = 10, hjust = 0) +
  theme(axis.text.x=element_text(colour = "white") )

DAT11 <- tibble(X = c(rexp(n=10000, 0.75), rexp(n=10000, 0.5)), Z = c(rep("A", times=10000), rep(rep("B", times=10000) )))
P11 <- DAT11 %>% ggplot(aes(x=X, fill=Z)) +
  geom_density(alpha=0.4) +
  theme(panel.background = element_blank(), axis.text = element_blank()) +
  theme(legend.position = "none") +
  xlim(0, 10) +

  theme_void() +
  theme(legend.position = "none") +

  annotate("text",
           x = c(1, 1.5),
           y =  c(0.20, 0.40  ),
           label =  c( expression(lambda[1]) , expression(lambda[2])   )  ,
           size = 8, hjust = 0) +
  theme(axis.text.x=element_text(colour = "white") )

par(mar=c(5,0,5,0))
dev.new(width=20, height=20)
window()
see::plots(P1, P11 , n_rows=1)
#_____________________________________________________________________________________________________

#_____________________________________________________________________________________________________
# Frequentist characterization of random behavior

# Sampling distribution
# Experiment 1: Toss a coin 10 times. Estimate relative frequency of heads
# Do this many times, to see how the relative frequencies vary
# This variation is the sampling distribution
# Experiment 2: Toss a coin 20 times
# Experiment 3: Toss a coin 40 times

S1 <- rep(0, times = 1000)
S2 <- rep(0, times = 1000)
S3 <- rep(0, times = 1000)
for (i in 1:1000) {
  S1[i] <- mean(rbinom(10, 1, 0.5))
  S2[i] <- mean(rbinom(20, 1, 0.5))
  S3[i] <- mean(rbinom(40, 1, 0.5))
}
S <- as_tibble(cbind(S1, S2, S3)) %>%
  pivot_longer(everything(),
               names_to = "Experiment",
               values_to = "Proportion"  ) %>%
  mutate(Experiment = factor(Experiment, labels = c("10 tosses", "20 tosses", "40 tosses"))) %>%
  ggplot( aes(x=Proportion, fill=Experiment)) +
  geom_density(alpha=0.4) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "bottom")

dev.new(width=20, height=20)
window()

S

#_______________________________________________________________________________________________________

# Tossing a coin 10 times (random sample from a population of tosses)
set.seed(676)
D <- tibble(y=rbinom(10, 1, 0.5))
m <- glm(y~ .,  data = D , family = "binomial" )
summary(m)
exp(m$coefficients)/(1+exp(m$coefficients))   # Inv. Logit
exp(confint(m))/(1+exp(confint(m)))


# Uncertainty about theta becomes smaller when sample size increase.

set.seed(3264)
Data <- tibble(e1 = mean(rbinom(2, 1, 0.5)),
               e2 = mean(rbinom(4, 1, 0.5)),
               e3 = mean(rbinom(8, 1, 0.5)),
               e4 = mean(rbinom(16, 1, 0.5)),
               e5 = mean(rbinom(32, 1, 0.5)),
               e6 = mean(rbinom(64, 1, 0.5)),
               e7 = mean(rbinom(128, 1, 0.5))) %>%
  pivot_longer( everything(), names_to = "Repetitions", values_to = "proportion")

dev.new(width=20, height=20)
window()
p<-ggplot(data=Data, aes(x=Repetitions, y=proportion)) +
  geom_bar(stat="identity" ,  color="blue", fill="white") +
  theme_bw() +
  labs(title="Relative propertion of heads in \n single experiment of different size") +
  geom_hline(yintercept=0.5, linetype="dashed",
             color = "red", size=1) +
  annotate("text",
           x = c(3, 1, 2, 3, 4, 5, 6, 7),
           y =  c(0.85, rep(0.75, times = 7)),
           label =   c("Number of repetitions:", "2", "4", "8", "16", "32", "64", "128" ) ,
           size = 5, hjust = 1) +
  annotate("text",x=c(7), y=c(0.55), label=c(expression(theta)), size = 6, hjust = 0, color = "red")
p

#_____________________________________________________________________________________________________
# Logistic function
# Explain the value of theta by a variable X

LG <- tibble( X = seq(-5, 5, by = 0.1),
              logOdds = 0.1 + 0.9*X,
              Odds = exp(logOdds),
              Probability = Odds/(1+Odds)) %>%
  pivot_longer(cols = -X,
               names_to = "Function",
               values_to = "Value"  ) %>%
  mutate(Function = factor(Function, levels = c("Probability", "Odds", "logOdds"))) %>%
  ggplot(aes(x=X, y=Value)) +
  geom_line() +
  facet_wrap(~Function, scales = "free_y")
dev.new(width=20, height=20)
window()
LG

#____________________________________________________________________________________________________
# Sampling distribution again
# Calculating a t-test statistic 1000 times when H0: mu = 0 is true
set.seed(453)
t <- rep(0, times = 10000)
for (i in 1:10000) {
  d <- rnorm(20, 0, 1)
  t[i] <- mean(d)/sd(d)  }
plot(density(t))
text(-1.0, 0.8, "Distribution of t-test statistic (N=20) \n (1000 times) when H0: \n mu = 0 is true", cex = 1.1, pos=4)
text(0.4, 0.5, "P-value: Probability observe at \n least t=0.5 \n given H0 true = 0.31", cex = 1.1, pos=4)


#_____________________________________________________________________________________________________
# Prior distribution
library(ggridges)          # For graphics

tibble(A = rnorm(100000, 4, 1.75), B  = rnorm(100000, 1, 2.25), C = rnorm(100000, 0, 4)) %>%
  pivot_longer( everything(), names_to = "Person", values_to = "theta") %>%
  ggplot( aes(x = theta, y = Person)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.025, 0.975), alpha = 0.7) +
  xlim(-10,10)

DAT1 <- tibble(theta = rnorm(n=10000, 0.75, 0.4))
P1<-   DAT1 %>% ggplot(aes(x=theta)) +
  theme_bw() +
  geom_density(alpha=0.4) +
  theme(legend.position = "none") +
  annotate("text", x = c(1), y = c(0.65),
           label =  expression("Prior distribution for:" ~ theta ~":Normal")  ,
           size = 5.5, hjust = 1) +
  theme(axis.text.x=element_text(colour = "white") ) +
  annotate("text",
           x = c(1, 1),
           y =  c(0.275, 0.2),
           label =   c(expression("the mean:" ~ mu ~"=0.75"), expression("the variance:" ~ sigma^2~"=0.4")   ) ,
           size = 5.5, hjust = 1) +
  theme(axis.text.x=element_text(colour = "black") )
P1

tibble(A = rnorm(100000, 4, 1.5), B  = rnorm(100000, 0, 4), C = runif(100000, -9, 9)) %>%
  pivot_longer( everything(), names_to = "Person", values_to = "theta") %>%
  ggplot( aes(x = theta, y = Person)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.025, 0.975), alpha = 0.7) +
  xlim(-10,10)



#_____________________________________________________________________________________________________
# Likelihood

par(mar=c(5,0,5,0))
dev.new(width=20, height=20)
window()
par(mfrow=c(1,2))

# Aspects of the probability model that are contained in the likelihood function:
# Distribution
y <- seq(-2, 2, 0.01)
p <- dnorm(y, mean = 0, sd = 0.8, log = FALSE)
p2 <- dnorm(y, mean = 0.2, sd = 1, log = FALSE)
plot(y, p, type = "l", lty = 1)
lines(y, p2,  col = "red", type = "l", lty = 2)
text(-1, 0.2, "A: Which probability distribution?", cex = 1.1, pos=4)

# Functional form
x <- seq(1,10, 0.1)
mu <- 0.25 + 0.6*x-0.05*x^2
p2 <- 0.9 + 0.1*x
p3 <- 0.05 + 0.7*x-0.07*x^2
p4 <- 1.1 + 0.075*x
plot(x, mu, type = "l", lty = 1)
lines(x, p2,  col = "red", type = "l", lty = 2)
lines(x, p3,  col = "blue", type = "l", lty = 3)
lines(x, p4,  col = "green", type = "l", lty = 4, lwd = 3.25)
text(1, 2, "B: Which form of the conditional dependence?", cex = 1.1, pos = 4)
text(1, 1.9, "C: Which values of the parameters?", cex = 1.1, pos=4)

#__________________________________________________________________-____________________
par(mfrow=c(1,1))

set.seed(7684)
y <-  rnorm(10)

sq<- sum(y^2); m<- mean(y)

mu<-seq(-1, 0.5,.01)
Likelihood<-  exp(-1*(sq+10*mu^2-2*10*m*mu ))
max(Likelihood)
par(mar=c(5,0,5,0))
dev.new(width=20, height=20)
window()

Txt <- paste(round(m, digits = 3))

plot(mu, Likelihood, type = "l", lty = 1)
abline(v = -0.384, col="blue", lwd=2, lty=2, cex = 0.5)
abline(h = 0.0044, lwd=2, col="blue", lty=2, cex = 0.5)
text(-0.15, 0, Txt, cex = 1.5)

p <- seq(0, 1, by = 0.025)
likelihood <- dbinom(x=2, size=2, prob=p)
plot( p, likelihood , type="l" , lty = 1 ,
      xlab="parameter p" , ylab="Likelihood" , col = "red" , main = "Likelihood 2 strikes in 2 attemps in bowling")
abline(v = 0.5, col="blue", lwd=2, lty=2)
abline(h = 0.25, col="green", lwd=2, lty=2)
abline(v = 1, col="red", lwd=2, lty=2)
abline(h = 1, col="pink", lwd=2, lty=2)

my <- seq(60, 100, by = 0.1)
likelihood <- dnorm(x=88.5, mean = my, sd = 20, log = FALSE)
plot( my, likelihood , type="l" , lty = 1 ,
      xlab="my" , ylab="Likelihood" , main = "Likelihood get X=88.5 when X~N(my, 20)" )
abline(v = 75, col="blue", lwd=2, lty=2)
abline(h = dnorm(x=88.5, mean = 75, sd = 20, log = FALSE), col="green", lwd=2, lty=2)


#__________________________________________________________________________________________________
# Code taken from https://bookdown.org/content/4857/small-worlds-and-large-worlds.html

sequence_length <- 1e3

d <-
  tibble(probability = seq(from = 0, to = 1, length.out = sequence_length)) %>%
  expand(probability, row = c("flat", "stepped", "Laplace")) %>%
  arrange(row, probability) %>%
  mutate(prior = ifelse(row == "flat", 1,
                        ifelse(row == "stepped", rep(0:1, each = sequence_length / 2),
                               exp(-abs(probability - 0.5) / .25) / ( 2 * 0.25))),
         likelihood = dbinom(x = 6, size = 9, prob = probability)) %>%
  group_by(row) %>%
  mutate(posterior = prior * likelihood / sum(prior * likelihood)) %>%
  pivot_longer(prior:posterior)  %>%
  ungroup() %>%
  mutate(name = factor(name, levels = c("prior", "likelihood", "posterior")),
         row  = factor(row, levels = c("flat", "stepped", "Laplace")))

p1 <-
  d %>%
  filter(row == "flat") %>%
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~name, scales = "free_y")

p2 <-
  d %>%
  filter(row == "stepped") %>%
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  facet_wrap(~name, scales = "free_y")

p3 <-
  d %>%
  filter(row == "Laplace") %>%
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = c(0, .5, 1)) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  facet_wrap(~name, scales = "free_y")

library(patchwork)          # For putting plots beside eachother.    https://github.com/thomasp85/patchwork


p1 / p2 / p3

#__________________________________________________________________________________________________
# How to calculate the posterior distribution

set.seed(3433)
Rs1 <- tibble(x=rnorm(500)) %>%
  ggplot(aes(x=x)) +
  theme_bw() +
  geom_histogram()

Rs1

Rs2 <- tibble(x=rnorm(100000)) %>%
  ggplot(aes(x=x)) +
  theme_bw() +
  geom_density(alpha=0.4)


par(mar=c(5,0,5,0))
dev.new(width=20, height=20)
window()
see::plots(Rs1, Rs2 , n_rows=1)

#__________________________________________________________________________________________________
# Rain example
Rp <- 0.3; Rn <- 1-Rp;           # Prior
CRp <- 0.9 ; CRn <- 0.4   # Likelihood
C <- CRp*Rp + CRn*Rn    # Probability of dark clouds
# Probability of rain given observed dark clouds
# Bayes formula:
RpC <- (CRp*Rp)/C
RpC

NRp <- 0.1 ; NRn <- 0.8 ;
N <- NRp*Rp + NRn*Rn
RpN <- (NRp*Rp)/N
RpN

#___________________________________________________________________________________________________
