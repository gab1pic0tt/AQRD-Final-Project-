#Replicate Table 1.2 (Accuracy Rates)
##COMPAS using 1000 person subset

broward_subset <- read_csv("allData/BROWARD_CLEAN_SUBSET.csv")

broward_subset <- broward_subset |>
  rename(charge_degree = `charge_degree (misd/fel)`) |>
  mutate(sex = ifelse(sex == 0, 'male', 'female'))

broward_subset <- broward_subset |>
  mutate(race = case_when(
    race == 1 ~ "White",
    race == 2 ~ "Black",
    race == 3 ~ "Hispanic",
    race == 4 ~ "Asian",
    race == 5 ~ "Native American",
    race == 6 ~ "Other"
  ))

broward_subset$compas_prediction <- ifelse(broward_subset$compas_decile_score > 4, 1, 0) 
broward_subset$compas_correct <- ifelse(broward_subset$compas_prediction == broward_subset$two_year_recid, 1, 0)

#calculate overall accuracy 
accuracy_overall <- broward_subset|>
  summarise(accuracy_overall = sum(compas_correct) / n())


accuracies <- broward_subset |>
  group_by(race)|>
  summarize(
    race_accuracy = sum(compas_correct) / n(),
    false_pos = sum(compas_correct == 0 & compas_prediction == 1) / (sum(two_year_recid == 0)),
    false_neg = sum(compas_correct == 0 & compas_prediction == 0) / (sum(two_year_recid == 1))
  ) |>
  mutate(
    accuracy_overall = accuracy_overall$accuracy_overall
  )|>
  filter(race %in% c("Black", "White"))


#AUC-ROC for Table 1.2
roc_auc_values <- numeric(1000)  # To store AUC-ROC values

for (i in 1:1000) {
  # Split the data into training and testing sets
  broward_split <- rsample::initial_split(broward_subset, prop = 0.8)
  broward_train <- training(broward_split)
  broward_test <- testing(broward_split)
  
  # Define 7 feature logistic regression formula
  form_7 <- as.formula("two_year_recid ~ age + sex + juv_misd_count + juv_fel_count + priors_count + charge_id + charge_degree")
  model_7 <- glm(form_7, data = broward_train, family = binomial)
  
  # Run model on test data
  predictions <- predict(model_7, newdata = broward_test, type = "response")
  predicted_labels <- ifelse(predictions > 0.5, 1, 0)
  
  # Calculate ROC curve
  roc_curve <- roc(broward_test$two_year_recid, predictions)
  
  # Store AUC-ROC value
  roc_auc_values[i] <- auc(roc_curve)
  
  # Store model & results
  model_results[[i]] <- list(model = model_7, accuracy = mean(predicted_labels == broward_test$two_year_recid))
}

roc_curve <- roc(broward_test$two_year_recid, predictions)
auc_roc <- auc(roc_curve)

#Finalizing Table 1.2
accuracy_data <- data.frame(
  . = c("Accuracy (overall)", "AUC-ROC (overall)", "Accuracy", "False positive", "False negative"),
  White = c(accuracies$accuracy_overall[2], auc_roc , accuracies$race_accuracy[2], accuracies$false_pos[2], accuracies$false_neg[2]),
  Black = c(accuracies$accuracy_overall[1], auc_roc , accuracies$race_accuracy[1], accuracies$false_pos[1], accuracies$false_neg[1])
)

accuracies_table <- accuracy_data |>
  gt() |>
  tab_spanner(
    label = "Defendant Race",
    columns = c("White", "Black")
  ) |>
  fmt_percent(
    columns = vars(White, Black),
    decimals = 1
  ) |>
  tab_header(
    title = "COMPAS algorithmic predictions from 1000 defendants",
    subtitle = "Overall accuracy is specified as percent correct"
  )
accuracies_table




