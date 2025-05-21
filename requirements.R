required_packages <- c("tidyverse", "caret", "xgboost", "randomForest", "glmnet", 
                       "nnet", "fastshap", "ggplot2", "ggcorrplot", "rstatix")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
}
lapply(required_packages, install_if_missing)
