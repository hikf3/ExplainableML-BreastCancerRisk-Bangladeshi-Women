## Load packages
library(dplyr)
library(caret)
library(glmnet)
library(randomForest)
library(xgboost)
library(lightgbm)
library(e1071)
library(pROC)
library(PRROC)
library(kernlab)
library(fastshap)
library(tidyr)
library(iml)
library(ggplot2)
library(ggbeeswarm)
library(tibble)
library(purrr)

# Load and prepare dataset
breastcancer <- read.csv("data/brstfin.csv")

# Rename and convert variables
age_groups <- c("[18-30]", "(30-40]", "(40-50]", "(50-60]", "(60-70]", "(70-75]")
breastcancer <- breastcancer %>%
  mutate(Age_Group = cut(
    age, 
    breaks = c(17, 30, 40, 50, 60, 70, 75),  # Define the boundaries
    labels = age_groups, 
    right = TRUE  # Include the upper limit
  ))


# Convert specified variables to categorical
breastcancer <- breastcancer %>%
  mutate(across(c(type, res, prof, parity, delv, type_men, stat_mens, abortion, nutrition, education_level), as.factor)) %>%
  # Convert specified variables to numeric
  mutate(across(c(age, marr_first, baby_first, menarchae), as.numeric)) %>%
  # Convert specified variables to counts
  mutate(across(c(gap_menrache_firstbaby, Gap_marriage_firstbaby), as.integer))

# Normalize numeric variables
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
breastcancer <- breastcancer %>%
  mutate(across(
    c(age, marr_first, baby_first, menarchae, gap_menrache_firstbaby, Gap_marriage_firstbaby), 
    list(norm = normalize)  # Appends "_norm" to the names of the normalized variables
  ))

# Ensure the target variable is a factor and rename its levels
breastcancer$type <- factor(
  breastcancer$type,
  levels = c(0, 1, 2),  # Original levels
  labels = c("No_Cancer",  "Hormone_Positive", "Triple_Negative")  # New levels
)
