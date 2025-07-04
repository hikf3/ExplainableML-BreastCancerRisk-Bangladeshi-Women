# Wrapper for XGBoost predictions (Hormone Positive)
predict_wrapper_xgb <- function(object, newdata) {
  predict(object, newdata, type = "prob")[, "Hormone_Positive"]
}

# Get the predicted probabilities for the "Hormone_Positive" class
predicted_probs_hormone <- predict_wrapper_xgb(fit_xgb, train_data_encoded[, -c(which(names(train_data_encoded) == "type"),1:7)])
# View the probabilities
head(predicted_probs_hormone)

# Wrapper for Random Forest predictions (Triple Negative)
predict_wrapper_rf <- function(object, newdata) {
  predict(object, newdata, type = "prob")[, "Triple_Negative"]
}

# Compute SHAP values for Hormone Positive (XGBoost)
shap_hormone <- explain(
  object = fit_xgb,  # The trained XGBoost model
  X = train_data_encoded[, -c(which(names(train_data_encoded) == "type"),1:7)],  # Background data
  pred_wrapper = predict_wrapper_xgb,  # Prediction wrapper function
  nsim = 10  # Number of simulations for approximation
)

# Compute SHAP values for Triple Negative (Random Forest)
shap_triple <- explain(
  object = fit_rf,  # The trained Random Forest model
  X = train_data_encoded[, -c(which(names(train_data_encoded) == "type"),1:7)], # Background data
  pred_wrapper = predict_wrapper_rf,  # Prediction wrapper function
  nsim = 10  # Number of simulations for approximation
)

colnames(shap_hormone) <- paste0("SHAP_", colnames(shap_hormone))
colnames(shap_triple) <- paste0("SHAP_", colnames(shap_triple))


# Combine SHAP values with the original data
shap_hormone <- cbind(train_data, shap_hormone)
shap_triple <- cbind(train_data, shap_triple)


###################### Wilcox test ############

# Add a significant column to the Wilcoxon results
compute_wilcoxon_test <- function(dataset, column_range) {
  wilcoxon_results <- data.frame(
    Feature = character(0),
    Median = numeric(0),
    P_Value = numeric(0),
    Significant = logical(0),  # New column for significance
    stringsAsFactors = FALSE
  )
  
  for (col in column_range) {
    feature_name <- colnames(dataset)[col]
    shap_values <- dataset[[col]]
    
    # Perform Wilcoxon signed-rank test
    wilcox_test <- wilcox.test(shap_values, mu = 0, exact = FALSE)
    
    wilcoxon_results <- rbind(
      wilcoxon_results,
      data.frame(
        Feature = feature_name,
        Median = median(shap_values, na.rm = TRUE),
        P_Value = wilcox_test$p.value,
        Significant = wilcox_test$p.value < 0.05
      )
    )
  }
  
  return(wilcoxon_results)
}

# Compute Wilcoxon results for hormone and triple datasets
shap_columns <- 24:43
wilcoxon_hormone <- compute_wilcoxon_test(shap_hormone, shap_columns)
wilcoxon_triple <- compute_wilcoxon_test(shap_triple, shap_columns)


###################### Paired Wilcox test ############
# Function to perform paired Wilcoxon test
compute_paired_wilcox <- function( shap_triple,shap_hormone, column_range) {
  paired_wilcox_results <- data.frame(
    Feature = character(0),
    Hormone_Mean = numeric(0),
    Triple_Mean = numeric(0),
    W_Statistic = numeric(0),
    P_Value = numeric(0),
    Significant = logical(0),
    stringsAsFactors = FALSE
  )
  
  for (col in column_range) {
    feature_name <- colnames(shap_hormone)[col]
    hormone_values <- shap_hormone[[col]]
    triple_values <- shap_triple[[col]]
    
    # Perform paired Wilcoxon test
    wilcox_test <- wilcox.test(triple_values, hormone_values, paired = TRUE, alternative="greater")
    
    # Add results to the dataframe
    paired_wilcox_results <- rbind(
      paired_wilcox_results,
      data.frame(
        Feature = feature_name,
        Hormone_Mean = mean(hormone_values, na.rm = TRUE),
        Triple_Mean = mean(triple_values, na.rm = TRUE),
        W_Statistic = wilcox_test$statistic,
        P_Value = wilcox_test$p.value,
        Significant = wilcox_test$p.value < 0.05
      )
    )
  }
  
  return(paired_wilcox_results)
}

# Specify column range for SHAP values
column_range <- 24:43

# Perform paired Wilcoxon test
paired_wilcox_triple_greater_than_hormone <- compute_paired_wilcox(shap_triple,shap_hormone,  column_range)
