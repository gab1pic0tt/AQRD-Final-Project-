library(modelsummary)
library(summarytools)
library(glmnetUtils)
library(gtsummary)
library(tidyverse)
library(rsample)
library(glmnet)
library(dplyr)
library(broom)
library(glue)
library(pROC)
library(gt)


#DATA
broward_clean <- read_csv("allData/BROWARD_CLEAN.csv")
charges <- read_csv("allData/CHARGE_ID.csv")
#mturk_predictions <- read_csv("allData/MTURK_RACE.csv")
demographics <- read_csv("allData/MTURK_RACE_DEMOGRAPHICS.csv")

broward_clean <- broward_clean |>
  rename(charge_degree = `charge_degree (misd/fel)`) |>
  mutate(sex = ifelse(sex == 0, 'male', 'female'))

broward_clean <- broward_clean |>
  mutate(race = case_when(
    race == 1 ~ "White",
    race == 2 ~ "Black",
    race == 3 ~ "Hispanic",
    race == 4 ~ "Asian",
    race == 5 ~ "Native American",
    race == 6 ~ "Other"
  ))


#-------------Estimating Equation with 7 Features----------------------Gabi 12/18
#Training Data - 80/20 split
set.seed(438)
model_results <- list()
conf_matrix7 <- matrix(0, nrow = 2, ncol = 2)


# train 1000 times on 80/20 test/train split
for (i in 1:1000) {
  # Split the data into training and testing sets
  broward_split <- rsample::initial_split(broward_clean, prop = 0.8)
  broward_train <- training(broward_split)
  broward_test <- testing(broward_split)
  
  # Define 7 feature logistic regression formula
  form_7 <- as.formula("two_year_recid ~ age + sex + juv_misd_count + juv_fel_count + priors_count + charge_id + charge_degree")
  model_7 <- glm(form_7, data = broward_train, family = binomial)
  
  # Run model on test data
  predictions <- predict(model_7, newdata = broward_test, type = "response")
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Calculate confusion matrix
  conf_matrix_iteration <- table(Actual = broward_test$two_year_recid, Predicted = predicted_labels)
  
  # Update overall confusion matrix
  conf_matrix7 <- conf_matrix7 + conf_matrix_iteration
  
  # Store model & results
  model_results[[i]] <- list(model = model_7, accuracy = mean(predicted_labels == broward_test$two_year_recid))
}





# Extract relevant information from the results
accuracies_7 <- sapply(model_results, function(x) x$accuracy)

# Print the summary of the accuracies
cat("Mean Testing Accuracy:", mean(accuracies_7), "\n")
cat("Standard Deviation of Testing Accuracy:", sd(accuracies_7), "\n")

#create dataframe of classification accuracies using confusion matrix
conf_matrix7

precision_df <- data.frame(
  Classification = c("Actually No", "Actually Yes"),
  Predicted_No = c(622216, 298801),
  Predicted_Yes = c(170569, 351414)
)

precision_table7 <- precision_df |>
  gt() |>
  tab_spanner(
    label = "7 Feature Classifier Prediction",
    columns = c("Predicted_No", "Predicted_Yes")
  ) |>
  fmt_number(
    columns = vars(Predicted_No, Predicted_Yes),
    decimals = 0) |>
  tab_header(
    title = "7 Feature Classifier Prediction of Recidivism vs Actual Recidivism",
    subtitle = "Results from training 1000 times on defendant data with features age, sex juvenile misdemeanor count, juvenile felony count, priors count, charge type, & charge degree"
  ) 

precision_table7



#7 feature logit regression model coefficent table---------------
seven_form <- as.formula("two_year_recid ~ age + sex + juv_misd_count + juv_fel_count + priors_count + charge_id + charge_degree")
seven_model <- glm(seven_form, data = broward_train, family = binomial)

tidy_summary <- tidy(seven_model)

seven_feat_table <- tidy_summary |>
  gt() |>
  tab_header(
    title = "Seven Feature Logistic Regression Model Summary",
    subtitle = "Predictors of Recidivism"
  ) %>%
  fmt_number(
    columns = vars(estimate, std.error, statistic, p.value),
    decimals = 3
  ) %>%
  fmt_number(
    columns = vars(estimate),
    suffixing = TRUE
  ) %>%
  fmt_number(
    columns = vars(p.value),
    decimals = 4
  ) %>%
  tab_spanner(
    label = "Coefficient",
    columns = vars(estimate, std.error)
  ) %>%
  tab_spanner(
    label = "Statistical Significance",
    columns = vars(statistic, p.value)
  )

#This is our logistic regression model we use to train the data
seven_feat_table




#-------------Estimating Equation with 2 Features----------------------Gabi 12/18
#Repeat this process with 2 features, age and prior convictions
set.seed(438)
model_results2 <- list()
conf_matrix2 <- matrix(0, nrow = 2, ncol = 2)


# train 1000 times on 80/20 test/train split
for (i in 1:1000) {
  # Split the data into training and testing sets
  broward_split <- rsample::initial_split(broward_clean, prop = 0.8)
  broward_train <- training(broward_split)
  broward_test <- testing(broward_split)
  
  # Define 7 feature logistic regression formula
  form_2 <- as.formula("two_year_recid ~ age + priors_count")
  model_2 <- glm(form_2, data = broward_train, family = binomial)
  
  # Run model on test data
  predictions <- predict(model_2, newdata = broward_test, type = "response")
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Calculate confusion matrix
  conf_matrix_iteration <- table(Actual = broward_test$two_year_recid, Predicted = predicted_labels)
  
  # Update overall confusion matrix
  conf_matrix2 <- conf_matrix2 + conf_matrix_iteration
  
  # Store model & results
  model_results2[[i]] <- list(model = model_2, accuracy = mean(predicted_labels == broward_test$two_year_recid))
}

# Extract relevant information from the results
accuracies_2 <- sapply(model_results2, function(x) x$accuracy)

# Print the summary of the accuracies
cat("Mean Testing Accuracy:", mean(accuracies_2), "\n")
cat("Standard Deviation of Testing Accuracy:", sd(accuracies_2), "\n")

#create dataframe of classification accuracies using confusion matrix
conf_matrix2

precision_df2 <- data.frame(
  Classification = c("Actually No", "Actually Yes"),
  Predicted_No = c(639492, 311872),
  Predicted_Yes = c(153293, 338343)
)

precision_table2 <- precision_df2 |>
  gt() |>
  tab_spanner(
    label = "2 Feature Classifier Prediction",
    columns = c("Predicted_No", "Predicted_Yes")
  ) |>
  fmt_number(
    columns = vars(Predicted_No, Predicted_Yes),
    decimals = 0) |>
  tab_header(
    title = "2 Feature Classifier Prediction of Recidivism vs Actual Recidivism",
    subtitle = "Results from training 1000 times on defendant data with features age & prior count",
  ) 

precision_table2



#------------Our 2 feature logit regression model coefficent table----------------GAbi 
two_form <- as.formula("two_year_recid ~ age + priors_count")
two_model <- glm(two_form, data = broward_train, family = binomial)

tidy_summary <- tidy(two_model)

two_feat_table <- tidy_summary |>
  gt() |>
  tab_header(
    title = "Two Feature Logistic Regression Model Summary",
    subtitle = "Predictors of Recidivism"
  ) %>%
  fmt_number(
    columns = vars(estimate, std.error, statistic, p.value),
    decimals = 3
  ) %>%
  fmt_number(
    columns = vars(estimate),
    suffixing = TRUE
  ) %>%
  fmt_number(
    columns = vars(p.value),
    decimals = 4
  ) %>%
  tab_spanner(
    label = "Coefficient",
    columns = vars(estimate, std.error)
  ) %>%
  tab_spanner(
    label = "Statistical Significance",
    columns = vars(statistic, p.value)
  )

#This is our 2 feature logistic regression model we use to train the data
two_feat_table

