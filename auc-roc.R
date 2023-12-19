#Calculate the AUC ROC using both the predictive models we made 


#7 feature model AUC ROC
# Initialize variables to store results
roc_auc_values <- numeric(1000)  # To store AUC-ROC values

# Train 1000 times on 80/20 test/train split
for (i in 1:1000) {
  # Calculate ROC curve
  roc_curve <- roc(broward_test$two_year_recid, predictions)
  
  # Store AUC-ROC value
  roc_auc_values[i] <- auc(roc_curve)
  
  # Store model & results
  model_results[[i]] <- list(model = model_7, accuracy = mean(predicted_labels == broward_test$two_year_recid))
}

# Print mean and standard deviation of AUC-ROC values
cat("Mean AUC-ROC:", mean(roc_auc_values), "\n")

roc_curve <- roc(broward_test$two_year_recid, predictions)









