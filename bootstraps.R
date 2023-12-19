library(tidyverse)
library(caret)

#Gabi's attempt for Black ppl 7 feature model
set.seed(438)
blk_results <- list()

blk_broward <- broward_clean |>
  filter(race == "Black")

# train 1000 times on 80/20 test/train split
for (i in 1:1000) {
  # Split the data into training and testing sets
  blk_split <- rsample::initial_split(blk_broward, prop = 0.8)
  blk_train <- training(blk_split)
  blk_test <- testing(blk_split)
  
  # Define 7 feature logistic regression formula with only black defendants
  blk_form <- as.formula("two_year_recid ~ age + sex + juv_misd_count + juv_fel_count + priors_count + charge_id + charge_degree")
  blk_model <- glm(blk_form, data = blk_train, family = binomial)
  
  # Run model on test data
  predictions <- predict(blk_model, newdata = blk_test, type = "response")
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Store model & results
  blk_results[[i]] <- list(model = blk_model, accuracy = mean(predicted_labels == blk_test$two_year_recid))
}

# Extract relevant information from the results
blk_accuracies <- sapply(blk_results, function(x) x$accuracy)

# Print the summary of the accuracies
accuracy_blk_7 <- mean(blk_accuracies)
cat("Standard Deviation of Testing Accuracy:", sd(blk_accuracies), "\n")



#Gabi's attempt for White ppl 7 feature model-----------
wht_results <- list()

wht_broward <- broward_clean |>
  filter(race == "White")

# train 1000 times on 80/20 test/train split
for (i in 1:1000) {
  # Split the data into training and testing sets
  wht_split <- rsample::initial_split(wht_broward, prop = 0.8)
  wht_train <- training(wht_split)
  wht_test <- testing(wht_split)
  
  # Define 7 feature logistic regression formula with only black defendants
  wht_form <- as.formula("two_year_recid ~ age + sex + juv_misd_count + juv_fel_count + priors_count + charge_id + charge_degree")
  wht_model <- glm(wht_form, data = wht_train, family = binomial)
  
  # Run model on test data
  predictions <- predict(wht_model, newdata = wht_test, type = "response")
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Store model & results
  wht_results[[i]] <- list(model = wht_model, accuracy = mean(predicted_labels == wht_test$two_year_recid))
}

# Extract relevant information from the results
wht_accuracies <- sapply(wht_results, function(x) x$accuracy)

# Print the summary of the accuracies
accuracy_wht_7 <- mean(wht_accuracies)
cat("Standard Deviation of Testing Accuracy:", sd(wht_accuracies), "\n")



#Gabi's attempt for White ppl 2 feature model------------------------
wht_results2 <- list()

# train 1000 times on 80/20 test/train split
for (i in 1:1000) {
  # Split the data into training and testing sets
  wht_split <- rsample::initial_split(wht_broward, prop = 0.8)
  wht_train <- training(wht_split)
  wht_test <- testing(wht_split)
  
  # Define 7 feature logistic regression formula with only black defendants
  wht_form2 <- as.formula("two_year_recid ~ age + priors_count")
  wht_model2 <- glm(wht_form2, data = wht_train, family = binomial)
  
  # Run model on test data
  predictions <- predict(wht_model2, newdata = wht_test, type = "response")
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Store model & results
  wht_results2[[i]] <- list(model = wht_model2, accuracy = mean(predicted_labels == wht_test$two_year_recid))
}

# Extract relevant information from the results
wht_accuracies2 <- sapply(wht_results2, function(x) x$accuracy)

# Print the summary of the accuracies
accuracy_wht_2 <- mean(wht_accuracies2)
cat("Standard Deviation of Testing Accuracy:", sd(wht_accuracies2), "\n")



#Gabi's attempt for White people 2 feature model-----------------------------------
blk_results2 <- list()

# train 1000 times on 80/20 test/train split
for (i in 1:1000) {
  # Split the data into training and testing sets
  blk_split <- rsample::initial_split(blk_broward, prop = 0.8)
  blk_train <- training(blk_split)
  blk_test <- testing(blk_split)
  
  # Define 7 feature logistic regression formula with only black defendants
  blk_form2 <- as.formula("two_year_recid ~ age + priors_count")
  blk_model2 <- glm(blk_form2, data = blk_train, family = binomial)
  
  # Run model on test data
  predictions <- predict(blk_model2, newdata = blk_test, type = "response")
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Store model & results
  blk_results2[[i]] <- list(model = blk_model2, accuracy = mean(predicted_labels == blk_test$two_year_recid))
}

# Extract relevant information from the results
blk_accuracies2 <- sapply(blk_results2, function(x) x$accuracy)

# Print the summary of the accuracies
accuracy_blk_2 <- mean(blk_accuracies2)
cat("Standard Deviation of Testing Accuracy:", sd(blk_accuracies2), "\n")


#FINAL RESULTS
accuracy_blk_7
accuracy_wht_7
accuracy_blk_2
accuracy_wht_2 


#CONFIDENCE INTERVALS
confidence_interval <- function(vector, interval) {
  # Standard deviation of sample
  vec_sd <- sd(vector)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(result)
}

#Overall accuracy confidence interval
confidence_interval(accuracies_7, 0.95)
confidence_interval(accuracies_2, 0.95)

#Black accuracy confidence interval
confidence_interval(blk_accuracies, 0.95)
confidence_interval(blk_accuracies2, 0.95)

#White accuracy confidence interval
confidence_interval(wht_accuracies, 0.95)
confidence_interval(wht_accuracies2, 0.95)




