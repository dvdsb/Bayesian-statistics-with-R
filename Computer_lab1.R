
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
library(broom)  # A package for presenting output nicely
#______________________________________________________________________________________________________

# Exercise 1: Analysing binary outcomes

# Logistic function
# Explain the value of theta by a variable X

a <- 0.1   # Intercept
b <- 1   # Log-odds ratio
# This corresponds to an odds ratio of :
exp(b)
# This is ratio of odds when you compare two individuals where the individual in the numerator
# as ONE unit higher value of X as compared with the individual in the denominator
# If OR > 1 then the odds for event increase when you increase X one unit.
# If OR < 1 then the odds for event decrease when you increase X one unit

A0 <- tibble( X = seq(-5, 5, by = 0.1),
              logOdds = a + b*X,
              Odds = exp(logOdds),
              Probability = Odds/(1+Odds))

# Visualize the three ways of characterizing the logistic function
Plott <-  A0 %>%
  pivot_longer(cols = -X,
               names_to = "Function",
               values_to = "Value"  ) %>%
  mutate(Function = factor(Function, levels = c("Probability", "Odds", "logOdds"))) %>%
  ggplot(aes(x=X, y=Value)) +
  geom_line() +
  facet_wrap(~Function, scales = "free_y")
dev.new(width=20, height=20)
window()
Plott

# Let us generate some data from the structure above (5 observation from each value of the underlying probability):

set.seed(5467)

A1 <- A0 %>%
  expand(Probability , rep = c(1, 2, 3, 4, 5)) %>%
  mutate( Y =  rbinom(505, 1, Probability) )
X <- tibble(X = seq(-5, 5, by = 0.1)) %>%
  expand(X , rep_ = c(1, 2, 3, 4, 5))
A1 <- cbind(A1, X)


# Let us now estimate a traditional frequentist logistic regression.
# Compare the results with the values of a (intercept) and b (and exp(b)) you specified in the beginning

Freq_M1 <-glm(Y ~X, data = A1, family = binomial)

p1_res <- broom::tidy(Freq_M1 , conf.int = T,  conf.level = 0.95,  exponentiate = T,  p.values = T)
exp(a) ; exp(b) ;
p1_res    # Results presented as Odds ratio


# Q1: How are the estimates as compared with the true values?


# Assess whether X can explain the variability in outcome by means of a LR test:
library(lmtest)
lrtest(Freq_M1)

# Q2: What is you conclusion based on the LR test?


# Visualze the fitted curve
library(ggiraphExtra)
P<-ggPredict(Freq_M1,se=TRUE,interactive=TRUE,digits=3)
P
# Now look what happens when you have a smaller sample size.
# Take random sample  (for example 20% or 10%) and re-do the analysis

sA1 <- A1 %>% sample_frac(0.1)
sFreq_M1 <-glm(Y ~X, data = sA1, family = binomial)
broom::tidy(sFreq_M1 , conf.int = T,  conf.level = 0.95,  exponentiate = T,  p.values = T)

ggPredict(sFreq_M1,se=TRUE,interactive=TRUE,digits=3)

# Do the LR test again.
lrtest(sFreq_M1)

# Q3: What is the difference in parameter estimates and it´s uncertainty when you have a smaller sample size?
# Q4:  What happens to the p-value?

#_______________________________________________________________________________________________________

# Exercise 2:  Analysing binary outcomes
# You are analysing data on adverse events in clinical trial.
# You want to address of there is a relationship between risk of adverse event and drug concentration (log scale)
# in the blood, and sex/gender. Some patients has a liver disease that may accept plasma concentration

# Create simulated data
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

# Inspect the data set:
 head(Data)
 glimpse(Data)
# Q1: How many adverse events are there in the data?
 table(Data$AE)

# Q2: How many  males and females are there in the data? How many have the liver disease?

# Make a crosstable to present prevalence of male and females that has AE

# Q3: What is the mean and standard deviation for log(Concentration) among those with and without AE?

 desc_ <- group_by(Data, AE) %>%
   summarise(  count = n(),
     mean = mean(DrugConc, na.rm = TRUE),
     sd = sd(DrugConc, na.rm = TRUE) )

# To the same as Q4 but separated on Sex.
# Q4: What is mean and standard deviation för males and females?

# Visualize the relationship between log(Concentration) and AE by means of:
  #   a) boxplot
  #   b) density plot
# Q5: What is you interpretation of the relationship between log(Concentration) and AE?

#  Perform a standard logistic regression with AE as dependent variable and DrugConc is independent variable
     M <- glm(AE ~ DrugConc , family = "binomial" , data = Data)
    coef(M)
    summary(M)
    confint(M)
  # What is you conclusion regarding the relationship between AE and drug concentration?

# Visualize the relationship by a plot
    library(ggeffects)
     ggpredict(M) %>% plot()

# Estimate the logistic regression mdoel again, but add
    #  a) Sex into the the model so you have two independent variables
    #  b) Sex and interaction effect (DrugConc*Sex) into the the model
# Then evaluate which of the models that are reasonable by mean of a Likelihood ratio test:

    m0<- glm(AE ~ DrugConc , family = "binomial" , data = Data)
    m1<- glm(AE ~ DrugConc + Sex, family = "binomial" , data = Data)
    m2<- glm(AE ~ DrugConc + Sex + DrugConc*Sex, family = "binomial" , data = Data)

    A <- logLik(m0)
    B <- logLik(m1)
    (teststat <- -2 * (as.numeric(A)-as.numeric(B)))
    (p.val <- pchisq(teststat, df = 1, lower.tail = FALSE))

    library(lmtest)
    lrtest(m0, m1)

# Q6: What is your conclusion based on the test? Which of the three models seems to be most adequate?

# Study the relationship between Drug concentration and liver disease. Do in the same way as  above.
# Q7: What is your conclusion regarding the to what extent liver disease influence Drug concentration?

# Do a frequentist two-sample t-test (linear model) to estimate the mean difference in log(Concentration) between those
#  with and without liver disease.

    m <- lm(DrugConc ~ Liver_condition , data = Data)
    summary(m)
    tidy(m , conf.int = T)

    t.test(DrugConc ~ Liver_condition , data = Data)

# Q7: Is the difference statistically significant? What is the 95% confidence interval for the difference?


#_______________________________________________________________________________________________________________________

# Exercise 3:

    # Two-sample t-test
    # Simulate two group with true mean differences as well as sample size that you decide.

    N <- 20         # Sample size. This can be varied
    delta <- 0.5      # True mean difference. This can be varied
    set.seed(4535)
    Data <- tibble(A = rnorm(N, 0, 1), B = delta + rnorm(N, 0, 1)) %>%
      pivot_longer(everything() , names_to = "Group", values_to = "Y") %>%
      mutate(Group = factor(Group))

# Estimate the mean difference between the groups by a two-sample t-test (linear model).
# Present p-value and confidence interval for the difference in means
# Try a few different values of delta for a fixed N. What happens with the confidence interval and the p-value?

# Q1: What is your conclusion?

#_____________________________________________________________________________________________________________________

# Exercise 4:

    # Replicated factorial design
    # You perform an experiment with 3 levels of stimuli A, Two levels of stimuli B.
    # The outcome has a normal distribution

    N <- 30         # Sample size. This can be varied
    delta <- 0.5      # True mean difference. This can be varied

    Data <- tibble(subj = seq(1:N) ,
                   A = rep(c(-1, 0, 1), times = 10),
                   B = rep(c(0, 1), times = 15),
                   Y = 1 + 0.5*A - 0.75*B + 1*A*B + rnorm(N, 0, 1)) %>%
      mutate(A = factor(A , labels =  c("A1", "A2", "A3")),
             B = factor(B , labels =  c("B1", "B2")))

# Visualize the variability and mean levels across the factors using boxplot and histogram/density plot

# Q1:  What is your first impression on how the response is related to the different factor levels?

 # Estimate a two-way ANOVA (linear model) with interaction between A and B
    m <- lm(Y~A + B + A*B, data = Data)
  # Make a brief presentation and interpretation of the results.
  # Use pairwise comparisons using for example the "emmeans" package

    library(emmeans)
    emm1 = emmeans(m, specs = pairwise ~ A:B)
    emm1$contrasts %>%
      confint()
# Visualize graphically estimated mean effects and 95% confidence intervals using for example the "ggeffects" package,
# Try to add data points (observations) to the plot

    library(ggeffects)

    mmm <- ggpredict(m , terms = c("A", "B"))
    plot(mmm, add.data = TRUE , connect.lines = TRUE)

# Q2 : What is your conlusion on the effect of the different levels of A and B?

#__________________________________________________________________________________________________

# Exercise 5:
# Cars data
#  The data give the speed of cars and the distances taken to stop. Note that the data were recorded in the 1920s
#  Look at the relationship between speed and time it takes to stop
#  Estimate a model that tries to predict stopping time given speed.
#  Use a part of data to estimate the model and a part to evaluate the prediction performance

# Look at data
    head(cars)
    summary(cars)
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

# Calculate the correlation coefficient
    cor(train$speed, train$dist , method = "pearson")
    cor(train$speed, train$dist , method =  "spearman")

# Make a quadratic cubic term of speed

    train <- train %>% mutate(speed2 = speed^2 , speed3 = speed^3)
    test <- test %>% mutate(speed2 = speed^2 , speed3 = speed^3)

# Pearson measures linear association. Spearman measures monotonic association (based on ranks). Spearman is "non-parametric"

# Estimate a linear regression: dist = a + b * speed
    reg <- lm(dist ~ speed, data = train)
    tidy(reg , conf.int = T)

# Estimate another with quadratic term  regression: dist = a + b * speed + c*speed^2
    reg2 <- lm(dist ~ speed + speed2, data = train)
    tidy(reg2 , conf.int = T)

# Estimate another with quadratic and cubic term  regression: dist = a + b * speed + c*speed^2 + d*speed^3
    reg3 <- lm(dist ~ speed + speed2 + speed3, data = train)
    tidy(reg3 , conf.int = T)

# Finally a super flexible model:
    library(splines)
    reg4 <- lm(dist ~ ns(speed, 7), data = train)
    tidy(reg4 , conf.int = T)

# Make a couple of comparisons between the four models with a likelihood ratio test.

# Q1: Which one seems most reasonable?

# Make a plot and add the regression lines

    ggplot(train, aes(x=speed, y=dist)) +
      geom_point() +
      geom_smooth(method = lm , formula = y ~ poly(x, 1))   # Linear

    ggplot(train, aes(x=speed, y=dist)) +
      geom_point() +
      geom_smooth(method = lm , formula = y ~ poly(x, 2))  # With quadratic term

    ggplot(train, aes(x=speed, y=dist)) +
      geom_point() +
      geom_smooth(method = lm , formula = y ~ poly(x, 3))  # With quadratic and cubic term

    ggplot(train, aes(x=speed, y=dist)) +
      geom_point() +
      geom_smooth(method = lm , formula = y ~ ns(x, 7))  # Super flexible curve


# How well does the models fit the train data?
    Pred_train <- tibble( pred = predict(reg, newdata = train),
                          pred2 = predict(reg2, newdata = train),
                          pred3 = predict(reg3, newdata = train),
                          pred4 = predict(reg4, newdata = train))
    Pred_train <- cbind(Pred_train , train)       %>%                    # Combine with the actual values
            mutate(residual =  (dist - pred) ,
                   residual2 =  (dist - pred2) ,
                   residual3 =  (dist - pred3),
                   residual4 =  (dist - pred4))

    MSE <- mean(Pred_train$residual^2)
    MSE2 <- mean(Pred_train$residual2^2)
    MSE3 <- mean(Pred_train$residual3^2)
    MSE4 <- mean(Pred_train$residual4^2)

    MSE ; MSE2 ; MSE3; MSE4

# Q2 : Which model gives best prediction?

# Now Make prediction on the test data set
    Pred_test <- tibble( pred = predict(reg, newdata = test),
                         pred2 = predict(reg2, newdata = test),
                         pred3 = predict(reg3, newdata = test),
                         pred4 = predict(reg4, newdata = test))
    Pred_test <- cbind(Pred_test , test)       %>%                    # Combine with the actual values
      mutate(residual =  (dist - pred) ,
             residual2 =  (dist - pred2) ,
             residual3 =  (dist - pred3) ,
             residual4 =  (dist - pred4))

    MSE <- mean(Pred_test$residual^2)
    MSE2 <- mean(Pred_test$residual2^2)
    MSE3 <- mean(Pred_test$residual3^2)
    MSE4 <- mean(Pred_test$residual4^2)

    MSE ; MSE2 ; MSE3 ; MSE4

# Q3 : Which model gives best prediction? Can you compare the predictive performance on Train and Test and
  #   discuss on why results are how they are?

#_______________________________________________________________________________________________________________________
