
# COURSE 1- Tidyverse  ------------------------------------------

# Load packages
library(moderndive)
library(ggplot2)

# Plot the histogram
ggplot(evals, aes(x = age)) +
  geom_histogram(binwidth = 5) +
  labs(x = "age", y = "count")

# Load packages
library(moderndive)
library(dplyr)

# Compute summary stats
evals %>%
  summarize(mean_age = mean(age),
            median_age = median(age),
            sd_age = sd(age))

# visualize the relations with a categorical variable, use geom_boxplot 
# geom_jitter to see scater plot


# explaining teaching score with age ----------------------------

# the model only pick up signal but noy noise 

# Load packages
library(ggplot2)
library(dplyr)
library(moderndive)

# Plot 
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "beauty score", y = "score") +
  geom_smooth(method = "lm", se = FALSE)
# Aids the eye in seeing patterns in the presence of overplotting


# Plot 
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "beauty score", y = "score") +
  geom_smooth(method = "lm", se = FALSE)
# Aids the eye in seeing patterns in the presence of overplotting

# load the plotting library
library(ggplot2)
random_number <- sample(1:10, 100, replace=T)
random_number <- as.data.frame(random_number)
temp <- temp %>% 
  mutate(n = 1:n())

plot(random_number)


# load the plotting library
library(ggplot2)
# load the function to manupulate data frame 
library(dplyr)

# sample random number
# for question 3 
test <- rpois(100,0.6) 

rpois(100,0.4)

52 * 7 
rpois(364,0.5)

rpois(100,1)

rpois(100,4)

rpois(100,20)

rpois(100,900)

# for question 4 
#test <- sample(1:5, 100, c(.05,.05,.2,.3,.4), replace = T)

# convert temp into a data frame that ggplot function can read 
test <- as.data.frame(test)

# calculate the count for each random number that occurs
test <-  test %>% 
  group_by(test) %>% 
  summarise(count = n(),
         possibility = count / 100)

# plot the graph
ggplot(test, aes(x = test, y = possibility)) +
  geom_point() +
  labs(x = "random number", y = "possibility") 

plot(test$test, test$possibility)


# get 100 random number between 1:10 
temp <- sample(1:10, 100, replace=T)

# convert temp into a data frame that ggplot function can read 
temp <- as.data.frame(temp)
temp <- temp %>% 
  mutate(n = 1:n())

ggplot(temp, aes(x = n, y = temp)) +
  geom_point() +
  labs(x = "index of the random number", y = "random number") 

# Fit regression model, linear model
model_score_2 <- lm(score ~ bty_avg, data = evals)

# Get regression table
get_regression_table(model_score_2)

# Get all fitted/predicted values and residuals
get_regression_points(model_score_2) %>% 
  mutate(score_hat_2 = ___ + ___ * bty_avg)

#Regression tables for categorical explanatory variables show differences in means relative to a baseline.

# lack of fit of the predictions to truth 


# model assesment and selection ---------------------------------
# refresher: multiple regression 
# sum of square of residuals , 1- var(residual)/var(y) the higher R square is, the better fit it is 
# assessing predictions with RMSE (root mean square error/mean square residual), average 
# rmse has the same unit as the original parameter. 
# Woo hoo! The RMSE is 0.167. You can think of this as the “typical” prediction error this model makes.


# validation set prediction framework ---------------------------

# 1. tran/fit your model
# 2. evaluate your predicitve power --> validate your model 

sample_frac(size = 1, replace = FALSE)

# Set random number generator seed value for reproducibility
set.seed(76)

# Randomly reorder the rows
house_prices_shuffled <- house_prices %>% 
  sample_frac(size = 1, replace = FALSE)

# Train/test split
train <- house_prices_shuffled %>%
  slice(1:10000)
test <- house_prices_shuffled %>%
  slice(10000:21613)


# Working with Data in the Tidyverse ----------------------------

readr()
read_csv()


# COURSE 2 - supervised learning with R ------------------------------------

library(tidyverse) # ggplot2, tidyer, dyplyr 
library(caret) # creating training and test data 


# Deselect the 2 columns to create cars_vars
cars_vars <- cars2018 %>%
  select(-Model, -`Model Index`)

# Fit a linear model with all predictors 
fit_all <- lm(MPG ~ ., data = cars_vars)

library(yardstick) # estimate how well models are working using tidy data principals 
# https://www.rdocumentation.org/packages/yardstick 

# Print the summary of the model
summary(fit_all)

# Load caret
library(caret)

# Split the data into training and test sets
set.seed(1234)
in_train <- createDataPartition(cars_vars$Transmission, p = 0.8, list = FALSE)
training <- cars_vars[in_train, ]
testing <- cars_vars[-in_train, ]

# Notice that we are fitting to log(MPG) since the fuel efficiency had a log normal distribution

# Train a random forest model
fit_rf <- train(log(MPG) ~ ., method = "rf", data = training,
                trControl = trainControl(method = "none"))

# Print the model object
fit_rf

library(yardstick)

# Create the new columns
results <- training %>%
  mutate(`Linear regression` = predict(fit_lm, training),
         `Random forest` = predict(fit_rf, training))

# Evaluate the performance
metrics(results, truth = MPG, estimate = `Linear regression`)
metrics(results, truth = MPG, estimate = `Random forest`)

# Great! The metrics are not worse on the testing data for either model, indicating we have not overfitted in either case.

# bootstraping resampling with caret  

results %>%
  gather(Method, Result, `Linear regression`:`Random forest`) %>%
  ggplot(aes(log(MPG), Result, color = Method)) +
  geom_point(size = 1.5, alpha = 0.5) +
  facet_wrap(~Method) +
  geom_abline(lty = 2, color = "gray50") +
  geom_smooth(method = "lm")

# Build a simple logistic regression model
simple_glm <- stackoverflow %>%
  select(-Respondent) %>%
  glm(Remote ~ .,  # Generalized linear model - Wikipedia
      family = "binomial",
      data = .)

# Print the summary of the model
summary(simple_glm)

# unsampling vs sampling 
# Load caret
library(caret)

stack_select <- stackoverflow %>%
  select(-Respondent)

# Split the data into training and testing sets
set.seed(1234)
in_train <- createDataPartition(stack_select$Remote, p=0.8, list = FALSE)
training <- stack_select[in_train,]
testing <- stack_select[-in_train,]

up_train <- upSample(x = select(training, -Remote),
                     y = training$Remote,
                     yname = "Remote") %>%
  as_tibble()

up_train %>%
  count(Remote)

# Correct! Upsampling samples with replacement until the class distributions are equal, 
# so there are the same number of remote and non-remote developers after upsampling.


# classification models -----------------------------------------

# logistic regression and randome models 
# use 
# Build a logistic regression model
stack_glm <- train(Remote ~ ., method = "glm", family = "binomial",
                   data = training,
                   trControl = trainControl(method = "boot",
                                            sampling = "up"))

# Print the model object 
stack_glm

# Set seed
set.seed(123)

# Confusion matrix for logistic regression model
confusionMatrix(predict(stack_glm, testing),
                testing$Remote)
# - voter turnout with R ------------------------------------
# How do the reponses on the survey vary with voting behavior?
voters %>%
  group_by(turnout16_2016) %>%
  summarize(`Elections don't matter` = mean(RIGGED_SYSTEM_1_2016 <= 2),
            `Economy is getting better` = mean(econtrend_2016 == 1),
            `Crime is very important` = mean(imiss_a_2016 == 2))

## Visualize difference by voter turnout
voters %>%
  ggplot(aes(econtrend_2016, ..density.., fill = turnout16_2016)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  labs(title = "Overall, is the economy getting better or worse?")

# Remove the case_indetifier column
voters_select <- voters %>%
  select(-case_identifier)

# Build a simple logistic regression model
simple_glm <- glm(turnout16_2016 ~ .,family = "binomial", 
                  data = voters_select)

# Print the summary                  
summary(simple_glm)

# use a simple model to see the preliminary insights on the data 

# Perform logistic regression with upsampling and no resampling
vote_glm <- train(turnout16_2016 ~ ., method = "glm", family = "binomial",
                  data = training,
                  trControl = trainControl(method = "none", # or choose to use "boot" method 
                                           sampling = "up"))

# 

# cross validation ----------------------------------------------
# partitioning the data into subsets and using one subset for validation 
# 10 groups, train using 9 others fold, evaluate using each of them in iteration. 
# train 10 models using 10 different 9 set of folds 
# repeated cross-validation can be time-consuming
# parallel processing 

# Logistic regression using cross validation method 
vote_glm <- train(turnout16_2016 ~ ., method = "glm", family = "binomial",
                  data = training,
                  trControl = trainControl(method = "repeatedcv",
                                           repeats = 2,
                                           sampling = "up"))

# Print vote_glm
vote_glm

# Random forest using cross validation method 
vote_rf <- train(turnout16_2016 ~ ., method = "rf", 
                 data = training,
                 trControl = trainControl(method = "repeatedcv",
                                          repeats = 2,
                                          sampling = "up"))

# Print vote_rf
vote_rf

# randomforest model does better job than glm method
# sensitivity , specificity  comparisons


# overfitting: memorizing the training data, but fail to capture the important features... 
# sens()  spec()
# between taining and testing data, if  sensitivity and specificity are similar, then that means the trained model is not overfit. 

#  surveying catholic sisters in 1967 ------------------------------------

# voter turnout from survey ------------------------------------

# Visualize agreement with age
tidy_sisters %>%
  filter(key %in% paste0("v", 153:170)) %>%
  group_by(key, value) %>%
  summarize(age = mean(age, na.rm = TRUE)) %>%
  ggplot(aes(value, age, color = key)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~key, nrow = 4)


# practicing data validating and training partition -------------
# Split the data into training and validation/test sets
set.seed(1234)
in_train <- createDataPartition(sisters_select$age, 
                                p = 0.6, list = FALSE)
training <- sisters_select[in_train, ]
validation_test <- sisters_select[-in_train, ]

# Split the validation and test sets
set.seed(1234)
in_test <- createDataPartition(validation_test$age, 
                               p = 0.5, list = FALSE)
testing <- validation_test[in_test, ]
validation <- validation_test[-in_test, ]


# models to use  ------------------------------------------------
# rpart --? 
# xgblinear
# gbm 

extreme gradient boosting model: preidictve , emsembling, time consuming 
# Make predictions on the three models
modeling_results <- validation %>%
  mutate(CART = predict(sisters_cart, validation),
         XGB = predict(sisters_xgb, validation),
         GBM = predict(sisters_gbm, validation))

# View the predictions
modeling_results %>% 
  select(CART, XGB, GBM)


# R_markdown ----------------------------------------------------

library(ggvis)
