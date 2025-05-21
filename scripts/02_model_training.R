# Set seed for reproducibility
set.seed(123)

# 1. Split the dataset into 90% training and 10% testing
train_index <- createDataPartition(breastcancer$type, p = 0.8, list = FALSE)
train_data <- breastcancer[train_index, ]
test_data <- breastcancer[-train_index, ]

# Separate numeric and categorical variables
numeric_vars <- c("age", "marr_first", "baby_first", "menarchae", 
                  "gap_menrache_firstbaby", "Gap_marriage_firstbaby","Age_Group")
normalized_vars <- paste0(numeric_vars[-length(numeric_vars)], "_norm") # Exclude 'Age_Group'

categorical_vars <- setdiff(names(train_data), c(numeric_vars, normalized_vars, "type"))  # Exclude target

# Apply one-hot encoding only to categorical variables
dummy_vars <- dummyVars(~ ., data = train_data[c(categorical_vars)], fullRank = TRUE)
categorical_encoded_train <- predict(dummy_vars, newdata = train_data) %>% as.data.frame()
categorical_encoded_test <- predict(dummy_vars, newdata = test_data) %>% as.data.frame()

# Combine numeric variables and one-hot encoded categorical variables
train_data_encoded <- cbind(
  train_data[c(numeric_vars, normalized_vars)], 
  categorical_encoded_train,
  type = train_data$type  # Include the target variable
)

test_data_encoded <- cbind(
  test_data[c(numeric_vars, normalized_vars)], 
  categorical_encoded_test,
  type = test_data$type  # Include the target variable
)


# 2. Define trainControl for 5-fold cross-validation
control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = TRUE
)
# Train the XGBoost model for Hormone Positive
fit_xgb <- train(
  type ~ ., 
  data = train_data_encoded[,-c(1:7)], 
  method = "xgbTree", 
  trControl = control, 
  verbose = 0
)

# Train the Random Forest model for Triple Negative
fit_rf <- train(
  type ~ ., 
  data = train_data_encoded[,-c(1:7)], 
  method = "rf", 
  trControl = control
)
