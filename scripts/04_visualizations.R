# Function to compute range of SHAP values
compute_shap_ranges <- function(shap_data, case_label) {
  shap_data %>%
    select(starts_with("SHAP_")) %>%
    pivot_longer(cols = everything(), names_to = "Feature", values_to = "SHAP_Value") %>%
    mutate(Feature = gsub("SHAP_", "", Feature)) %>%
    group_by(Feature) %>%
    summarise(
      Min_Value = min(SHAP_Value, na.rm = TRUE),
      Max_Value = max(SHAP_Value, na.rm = TRUE),
      Range = Max_Value - Min_Value
    ) %>%
    mutate(Case = case_label) %>%
    arrange(desc(Range))
}

# Compute ranges for Hormone Positive and Triple Negative
shap_ranges_hormone <- compute_shap_ranges(shap_hormone, "Hormone Positive")
shap_ranges_triple <- compute_shap_ranges(shap_triple, "Triple Negative")



########################################################################################
################# Global mean with 95% CI #################


# Function to compute mean and 95% confidence interval
compute_mean_ci <- function(data, confidence = 0.95) {
  n <- nrow(data)
  mean_values <- colMeans(data, na.rm = TRUE)
  std_errors <- apply(data, 2, sd, na.rm = TRUE) / sqrt(n)
  error_margin <- qt((1 + confidence) / 2, df = n - 1) * std_errors
  
  lower_ci <- mean_values - error_margin
  upper_ci <- mean_values + error_margin
  
  result <- data.frame(
    Feature = colnames(data),
    Mean = mean_values,
    Lower_CI = lower_ci,
    Upper_CI = upper_ci
  )
  
  return(result)
}


# Select only SHAP-related columns
shap_hormone_numeric <- shap_hormone %>% select(starts_with("SHAP_"))
shap_triple_numeric <- shap_triple %>% select(starts_with("SHAP_"))

# Compute mean and CI for both datasets
hormone_summary <- compute_mean_ci(shap_hormone_numeric)
triple_summary <- compute_mean_ci(shap_triple_numeric)
# Combine results for easier comparison
combined_summary <- hormone_summary %>%
  rename_with(~ paste0(.x, "_Hormone"), -Feature) %>%
  left_join(
    triple_summary %>% rename_with(~ paste0(.x, "_Triple"), -Feature),
    by = c(Feature = "Feature")  # Ensure `Feature` is used for joining
  )

# Display the combined summary
print(combined_summary)

# Save results to CSV
write.csv(combined_summary, "SHAP_Global_Mean_CI.csv", row.names = FALSE)

library(ggplot2)
library(dplyr)

# Create a mapping for feature names
feature_names <- c(
  "SHAP_age_norm" = "Age",
  "SHAP_marr_first_norm" = "Age at First Marriage",
  "SHAP_baby_first_norm" = "Age at First Baby",
  "SHAP_menarchae_norm" = "Age at Menarche",
  "SHAP_gap_menrache_firstbaby_norm" = "Gap between Menarche and First Baby",
  "SHAP_Gap_marriage_firstbaby_norm" = "Gap between First Marriage and First Baby",
  "SHAP_res.1" = "Residence:Urban (Ref: Rural)",
  "SHAP_res.2" = "Residence:Metropolitan",
  "SHAP_prof.1" = "Profession:Employed (Ref: Unemployed)",
  "SHAP_parity.1" = "Parity > 1 (Ref: Parity<=1)",
  "SHAP_delv.1" = "Type of Delivery:Cesarean Section (Ref: Vaginal)",
  "SHAP_delv.2" = "Type of Delivery:Both",
  "SHAP_type_men.1" = "Type of Menstruation:Irregular (Ref: Regular)",
  "SHAP_stat_mens.1" = "Menstrual Status:Postmenopausal (Ref: Premenopausal)",
  "SHAP_abortion.1" = "Abortion = 2 (Ref: Abortion = 0)",
  "SHAP_abortion.2" = "Abortion > 2",
  "SHAP_nutrition.1" = "BMI: Obese (Ref: Healthy)",
  "SHAP_nutrition.2" = "BMI: Undernutrition",
  "SHAP_education_level.1" = "Education: 6-12 Years (Ref: 0-5 Years)",
  "SHAP_education_level.2" = "Education: 13-20 Years"
)

# Update feature names in combined summary
combined_summary <- combined_summary %>%
  mutate(Feature = recode(Feature, !!!feature_names))


# Add the relative contribution column
combined_summary <- combined_summary %>%
  mutate(
    Relative_Contribution = Mean_Triple / Mean_Hormone,
    Relative_Contribution = ifelse(is.nan(Relative_Contribution), NA, Relative_Contribution) # Handle division by zero
  )




# Create the feature name mapping
feature_name_mapping <- c(
  "SHAP_age_norm" = "Age",
  "SHAP_marr_first_norm" = "Age at First Marriage",
  "SHAP_baby_first_norm" = "Age at First Baby",
  "SHAP_menarchae_norm" = "Age at Menarche",
  "SHAP_gap_menrache_firstbaby_norm" = "Gap between Menarche and First Baby",
  "SHAP_Gap_marriage_firstbaby_norm" = "Gap between First Marriage and First Baby",
  "SHAP_res.1" = "Residence:Urban (Ref: Rural)",
  "SHAP_res.2" = "Residence:Metropolitan",
  "SHAP_prof.1" = "Profession:Employed (Ref: Unemployed)",
  "SHAP_parity.1" = "Parity > 1 (Ref: Parity<=1)",
  "SHAP_delv.1" = "Type of Delivery:Cesarean Section (Ref: Vaginal)",
  "SHAP_delv.2" = "Type of Delivery:Both",
  "SHAP_type_men.1" = "Type of Menstruation:Irregular (Ref: Regular)",
  "SHAP_stat_mens.1" = "Menstrual Status:Postmenopausal (Ref: Premenopausal)",
  "SHAP_abortion.1" = "Abortion = 1 (Ref: Abortion = 0)",
  "SHAP_abortion.2" = "Abortion > 2",
  "SHAP_nutrition.1" = "BMI: Obese (Ref: Healthy)",
  "SHAP_nutrition.2" = "BMI: Undernutrition",
  "SHAP_education_level.1" = "Education: 6-12 Years (Ref: 0-5 Years)",
  "SHAP_education_level.2" = "Education: 13-20 Years"
)

# Update feature names in wilcoxon_hormone and wilcoxon_triple
wilcoxon_hormone <- wilcoxon_hormone %>%
  mutate(Feature = recode(Feature, !!!feature_name_mapping))

wilcoxon_triple <- wilcoxon_triple %>%
  mutate(Feature = recode(Feature, !!!feature_name_mapping))

# Add significant columns to combined_summary
combined_summary <- combined_summary %>%
  left_join(
    wilcoxon_hormone %>% select(Feature, Hormone_Significant = Significant),
    by = "Feature"
  ) %>%
  left_join(
    wilcoxon_triple %>% select(Feature, Triple_Significant = Significant),
    by = "Feature"
  )


# Update combined_summary to include significance annotations
combined_summary <- combined_summary %>%
  mutate(
    Hormone_Significance = ifelse(Hormone_Significant, "HR+ Mean (p-value < 0.05)", "HR+ Mean"),
    Triple_Significance = ifelse(Triple_Significant, "TNBC Mean (p-value < 0.05)", "TNBC Mean")
  )

# Create the plot
plot <- ggplot(combined_summary) +
  geom_point(aes(x = Mean_Hormone, y = Feature, color = Hormone_Significance, shape = Hormone_Significance), size = 3) +
  geom_errorbarh(aes(
    xmin = Lower_CI_Hormone,
    xmax = Upper_CI_Hormone,
    y = Feature,
    color = Hormone_Significance
  ), height = 0.2) +
  geom_point(aes(x = Mean_Triple, y = Feature, color = Triple_Significance, shape = Triple_Significance), size = 3) +
  geom_errorbarh(aes(
    xmin = Lower_CI_Triple,
    xmax = Upper_CI_Triple,
    y = Feature,
    color = Triple_Significance
  ), height = 0.2) +
  scale_color_manual(
    values = c(
      "HR+ Mean" = "red",
      "HR+ Mean (p-value < 0.05)" = "darkred",
      "TNBC Mean" = "blue",
      "TNBC Mean (p-value < 0.05)" = "darkblue"
    )
  ) +
  scale_shape_manual(
    values = c(
      "HR+ Mean" = 16,  # Point
      "HR+ Mean (p-value < 0.05)" = 18,  # Diamond
      "TNBC Mean" = 16,  # Point
      "TNBC Mean (p-value < 0.05)" = 18   # Diamond
    )
  ) +
  labs(
    title = "SHAP Values with 95% Confidence Interval",
    x = "SHAP Value (Global Mean with CI)",
    y = "Feature",
    color = "Legend",
    shape = "Legend"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        shape = c(16, 18, 16, 18),
        color = c("red", "darkred", "blue", "darkblue")
      )
    ),
    shape = guide_legend(
      override.aes = list(
        shape = c(16, 18, 16, 18),
        color = c("red", "darkred", "blue", "darkblue")
      )
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title.x = element_text(size = 14, face = "bold", color = "black"),
    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
    legend.position = "right",
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 14, face = "bold")
  )

# Save the plot as a high-quality PNG
ggsave("SHAP_Global_Mean_CI_Plot_With_Legend_Updates.png", plot, width = 12, height = 8, dpi = 300)

# Display the plot
print(plot)
########################################################################################
################# Shap values compared with wilcox paired test #################

library(dplyr)
library(ggplot2)

# Assuming your datasets are:
# paired_wilcox_different_than_zero
# paired_wilcox_results
# paired_wilcox_triple_greater_than_hormone

# Combine datasets into a single one
combined_data <- paired_wilcox_different_than_zero %>%
  select(Feature, Significant_Different_Than_Zero = Significant) %>%
  left_join(
    paired_wilcox_results %>%
      select(Feature, Triple_Less_Than_Hormone = Significant),
    by = "Feature"
  ) %>%
  left_join(
    paired_wilcox_triple_greater_than_hormone %>%
      select(Feature, Triple_Greater_Than_Hormone = Significant),
    by = "Feature"
  )

# Define groups based on significance
combined_data <- combined_data %>%
  mutate(
    Group = case_when(
      Triple_Less_Than_Hormone == TRUE ~ "μ_TN < μ_HR+",
      Triple_Greater_Than_Hormone == TRUE ~ "μ_TN > μ_HR+",
      Significant_Different_Than_Zero == FALSE ~ "μ_TN = μ_HR+",
      TRUE ~ "Other"
    )
  )

# Define a mapping for feature names
feature_name_mapping <- c(
  "SHAP_age_norm" = "Age",
  "SHAP_marr_first_norm" = "Age at First Marriage",
  "SHAP_baby_first_norm" = "Age at First Baby",
  "SHAP_menarchae_norm" = "Age at Menarche",
  "SHAP_gap_menrache_firstbaby_norm" = "Gap between Menarche and First Baby",
  "SHAP_Gap_marriage_firstbaby_norm" = "Gap between First Marriage and First Baby",
  "SHAP_res.1" = "Residence:Urban (Ref: Rural)",
  "SHAP_res.2" = "Residence:Metropolitan",
  "SHAP_prof.1" = "Profession:Employed (Ref: Unemployed)",
  "SHAP_parity.1" = "Parity > 1 (Ref: Parity<=1)",
  "SHAP_delv.1" = "Type of Delivery:Cesarean Section (Ref: Vaginal)",
  "SHAP_delv.2" = "Type of Delivery:Both",
  "SHAP_type_men.1" = "Type of Menstruation:Irregular (Ref: Regular)",
  "SHAP_stat_mens.1" = "Menstrual Status:Postmenopausal (Ref: Premenopausal)",
  "SHAP_abortion.1" = "Abortion = 2 (Ref: Abortion = 0)",
  "SHAP_abortion.2" = "Abortion > 2",
  "SHAP_nutrition.1" = "BMI: Obese (Ref: Healthy)",
  "SHAP_nutrition.2" = "BMI: Undernutrition",
  "SHAP_education_level.1" = "Education: 6-12 Years (Ref: 0-5 Years)",
  "SHAP_education_level.2" = "Education: 13-20 Years"
)

# Apply feature name mapping
combined_data <- combined_data %>%
  mutate(Feature = recode(Feature, !!!feature_name_mapping))

# Plot the data
plot <- ggplot(combined_data, aes(x = Group, y = Feature, color = Group)) +
  geom_point(size = 4) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "red") + # Optional vertical line
  scale_color_manual(values = c("μ_TN < μ_HR+" = "blue", "μ_TN > μ_HR+" = "red", "μ_TN = μ_HR+" = "black")) +
  labs(
    title = "Feature Comparison: TNBC vs. HR+",
    x = "Hypotheses (μ_TN vs. μ_HR+)",
    y = "Features"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "bold", color="black"),
    legend.position = "right",
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 14, face = "bold")
  )

# Save the plot
ggsave("Relative_Contributions_Hypotheses.png", plot, width = 12, height = 8, dpi = 300)

# Display the plot
print(plot)

########################################################################################
################# Shap values compared with numeric feature values #################

# Function to create faceted plot with feature values on x-axis and SHAP values on y-axis
create_facet_plot <- function(data, numeric_features, shap_features, title, feature_name_mapping) {
  # Map numeric features to SHAP counterparts
  feature_mapping <- setNames(shap_features, numeric_features)
  
  # Create a long-form dataframe with both feature values and SHAP values
  shap_long <- do.call(rbind, lapply(numeric_features, function(feature) {
    shap_feature <- feature_mapping[[feature]]
    data.frame(
      Feature = feature,
      Feature_Value = data[[feature]],
      SHAP_Value = data[[shap_feature]]
    )
  }))
  
  # Add descriptive labels for features
  shap_long$Feature_Label <- recode(shap_long$Feature, !!!feature_name_mapping)
  
  # Create the faceted plot
  plot <- ggplot(shap_long, aes(x = Feature_Value, y = SHAP_Value)) +
    geom_point(alpha = 0.5, size = 1, color = "blue") +
    geom_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.2, color = "red", formula = 'y ~ x') +
    facet_wrap(~ Feature_Label, scales = "free", ncol = 1) +
    labs(
      title = title,
      x = "Feature Value",
      y = "SHAP Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      strip.text = element_text(face = "bold", size = 10)
    )
  
  return(plot)
}

# Define numeric features and their SHAP counterparts
numeric_features <- c("age", "menarchae", "gap_menrache_firstbaby", 
                      "Gap_marriage_firstbaby", "baby_first", "marr_first")
shap_features <- c("SHAP_age_norm", "SHAP_menarchae_norm", "SHAP_gap_menrache_firstbaby_norm", 
                   "SHAP_Gap_marriage_firstbaby_norm", "SHAP_baby_first_norm", "SHAP_marr_first_norm")

# Feature name mapping
feature_name_mapping <- c(
  "age" = "Age",
  "menarchae" = "Age at Menarche",
  "marr_first" = "Age at First Marriage",
  "baby_first" = "Age at First Baby",
  "gap_menrache_firstbaby" = "Gap between Menarche and First Baby",
  "Gap_marriage_firstbaby" = "Gap between First Marriage and First Baby"
)

# Create faceted plot for Hormone Positive
plot_hormone_facet <- create_facet_plot(
  shap_hormone, numeric_features, shap_features, 
  "Shapley Values: Hormone Positive (Numeric Features)", feature_name_mapping
)

# Save the Hormone Positive plot
ggsave("Shapley_Values_Hormone_Positive_Facet.png", plot_hormone_facet, width = 10, height = 12, dpi = 300)

# Create faceted plot for Triple Negative
plot_triple_facet <- create_facet_plot(
  shap_triple, numeric_features, shap_features, 
  "Shapley Values: Triple Negative (Numeric Features)", feature_name_mapping
)

# Save the Triple Negative plot
ggsave("Shapley_Values_Triple_Negative_Facet.png", plot_triple_facet, width = 10, height = 12, dpi = 300)

########################################################################################
################# Relative Contribution Shap values compared with age group #################



# Map feature names for better readability
relative_contributions_final_long <- relative_contributions_final %>%
  pivot_longer(
    cols = -Age_Group,
    names_to = "Feature",
    values_to = "Relative_Contribution"
  ) %>%
  mutate(
    Feature = recode(Feature, !!!feature_names)  # Map feature names
  )

# Plot relative contributions
plot_relative_contributions <- ggplot(relative_contributions_final_long, aes(
  x = Age_Group,
  y = Relative_Contribution,
  color = Feature,
  group = Feature
)) +
  geom_line(size = 1.2) +  # Line plot for each variable
  geom_point(size = 2) +  # Add points for clarity
  scale_color_manual(values = rainbow(length(unique(relative_contributions_final_long$Feature)))) +
  labs(
    title = "Relative Contributions of Features by Age Group",
    x = "Age Group",
    y = "Relative Contribution",
    color = "Feature"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Save the plot as a high-quality PNG
ggsave("Relative_Contributions_Plot.png", plot_relative_contributions, width = 12, height = 8, dpi = 300)

# Display the plot
print(plot_relative_contributions)
########################################################################################
################# Global Relative Contribution Shap values #################

library(ggplot2)

# Plot global relative contributions with updated sizes and bold styles
plot_global_relative_contribution <- ggplot(combined_summary, aes(
  x = Relative_Contribution,
  y = reorder(Feature, Relative_Contribution),
  label = Feature
)) +
  geom_point(color = "blue", size = 5) +  # Plot points
  geom_text(
    aes(hjust = ifelse(Relative_Contribution < 0, 1.2, -0.2)),  # Left for < 0, right for > 0
    vjust = 0.5,
    size = 5,  # Increased text size
    fontface = "bold"  # Bold text
  ) +
  geom_vline(xintercept = 0, color = "red", size = 1.2) +  # Red vertical line at 0
  geom_segment(
    aes(
      x = 0, xend = Relative_Contribution,  # From 0 to feature's dot
      yend = reorder(Feature, Relative_Contribution)
    ),
    color = "yellow",
    size = 1.8  # Normal thickness solid lines
  ) +
  labs(
    title = "Global Relative Contribution of Triple Negative Vs. Hormone Positive Cases",
    x = "Relative Contribution",
    y = "Features"
  ) +
  scale_x_continuous(limits = c(-50, 50)) +  # Set x-axis range
  #theme_light() +  # Use grid-style theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Larger and bold title
    axis.title.x = element_text(face = "bold", size = 18),  # Larger and bold x-axis label
    axis.title.y = element_text(face = "bold", size = 18),  # Larger and bold y-axis label
    axis.text.x = element_text(face = "bold", size = 15),  # Larger and bold x-axis text
    axis.text.y = element_blank(),  # Hide y-axis text (feature labels shown as text annotations)
    axis.ticks.y = element_blank(),  # Hide y-axis ticks
    panel.grid.major.x = element_line(color = "gray80"),  # Gridlines for x-axis
    panel.grid.major.y = element_blank(),  # Remove gridlines for y-axis
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )

# Save the plot as a high-quality PNG with adjusted dimensions
ggsave(
  "Global_Relative_Contribution_Plot_Bold.png", 
  plot_global_relative_contribution, 
  width = 16,  # Narrow width
  height = 18,  # Taller height
  dpi = 300
)

# Display the plot
print(plot_global_relative_contribution)





