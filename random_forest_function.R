# Load necessary packages
library(tidyverse)
library(randomForest)
library(janitor)
library(caret)
library(missForest)
library(NADA)
library(ggplot2)

# Define parameters
data_file <- "Copy of River_Plastics_Sample_Data - DATA.csv"
target_variable <- "imputed_standardized_data"
features <- c("bsldem30m", "lc01dev_lc11dev", "x50_percent_aep_flood", "deployment_method", "sample_size", "top_particle", "filter_size")

# Function to load and preprocess data
load_and_preprocess_data <- function(data_file) {
  water_basin <- read.csv(data_file)
  water_basin <- clean_names(water_basin)
  cleaned_data <- water_basin %>%
    select(spatial_file_name, standardized_data_in_ppm3, all_of(features), precip, drnarea)
  columns_to_check <- c("bsldem30m", "lc01dev_lc11dev", "x50_percent_aep_flood", "precip", "drnarea")
  cleaned_data <- cleaned_data %>%
    filter(!rowSums(is.na(.[, columns_to_check])) == length(columns_to_check))
  cleaned_data <- cleaned_data %>%
    mutate(censored = ifelse(standardized_data_in_ppm3 == 0, TRUE, FALSE))
  
  return(cleaned_data)
}

# Function to impute missing mp conc values using NADA package
impute_missing_values_nada <- function(data) {
  fit <- cenros(data$standardized_data_in_ppm3, data$censored)
  set.seed(211)
  fittedvalues <- sample(fit$modeled[fit$censored], length(fit$modeled[fit$censored]), replace = FALSE)
  cleaned_data <- data %>%
    mutate(imputed_standardized_data = ifelse(censored, fittedvalues, standardized_data_in_ppm3))
  return(cleaned_data)
}


# Function to impute missing values using missForest
impute_missing_values_missforest <- function(data) {
  extracted_column1 <- data$deployment_method
  data <- data %>%
    select(-spatial_file_name, -censored, -deployment_method)
  imputed_data <- missForest(data)
  imputed_matrix <- imputed_data$ximp
  imputed_dataframe <- as.data.frame(imputed_matrix)
  data[is.na(data)] <- imputed_dataframe[is.na(data)]
  data <- data %>%
    mutate(deployment_method = extracted_column1)
  
  return(data)
}

# Function to split data into training and testing sets
split_data <- function(data) {
  full_data <- data[, c(target_variable, features)]
  return(full_data)
}

# Function to create and train random forest model
random_forest <- function(data, target_variable = target_variable) {
  rf_model <- randomForest(
    formula = imputed_standardized_data ~ .,
    data = data,
    ntree = 100
  )
  return(rf_model)
}

# Function to evaluate the model
evaluate_model <- function(model, data, target_variable) {
  predictions <- predict(model, newdata = data)
  
  rmse <- sqrt(mean((predictions - data$imputed_standardized_data)^2))
  
  # Calculate baseline prediction (mean or median)
  baseline_prediction <- mean(data$imputed_standardized_data)
  
  # Create a vector of baseline predictions for the full dataset
  baseline_predictions <- rep(baseline_prediction, nrow(data))
  
  # Calculate baseline RMSE
  baseline_rmse <- sqrt(mean((baseline_predictions - data$imputed_standardized_data)^2))
  
  # Calculate your model's RMSE
  model_rmse <- rmse
  
  return(list(baseline_rmse = baseline_rmse, model_rmse = rmse))
}

# Function for visualization
visualize_result_imputed <- function(data) {
  # Add visualization steps using ggplot
  ggplot(data, aes(x = standardized_data_in_ppm3, fill = "Original")) +
    geom_density(alpha = 0.5) +
    geom_density(data = data, aes(x = imputed_standardized_data, fill = "Imputed"), alpha = 0.5) +
    labs(title = "Density Plot: Original vs. Imputed",
         x = "original",
         y = "imputed") +
    scale_fill_manual(values = c("Original" = "blue", "Imputed" = "red")) +
    scale_x_log10() + # Add logarithmic scale to the x-axis
    theme_minimal()
}

# Density plot of macro/micro vs imputed_standardized_data with logarithmic x-axis
visualize_result_micro_macro <- function(data) {
  ggplot(data, aes(x = imputed_standardized_data, fill = macro_or_micro)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Imputed Standardized Data by Macro/Micro",
       x = "Imputed Standardized Data (Log Scale)",
       y = "Density") +
  scale_x_log10() +  # Add logarithmic scale to the x-axis
  theme_minimal()
}

# Density plot of deployment_method vs imputed_standardized_data with logarithmic x-axis
visualize_result_deployment <- function(data) {
  ggplot(data, aes(x = imputed_standardized_data, fill = deployment_method)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of Imputed Standardized Data by Deployment Method",
         x = "Imputed Standardized Data",
         y = "Density") +
    scale_x_log10() +  # Add logarithmic scale to the x-axis
    theme_minimal()
}

# Smoothed scatter plot of imputed_standardized_data vs filter_size
visualize_result_filter <- function(data) {
  ggplot(data, aes(x = top_particle, y = imputed_standardized_data)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +  # Add smoothed line without confidence interval
    labs(title = "Smoothed Scatter Plot: Imputed Standardized Data vs. Filter Size",
         x = "Filter Size",
         y = "Imputed Standardized Data") +
    scale_x_log10() +
    scale_y_log10() +
    theme_minimal()
}

# Main function
main <- function() {
  cleaned_data <- load_and_preprocess_data(data_file)
  imputed_data_nada <- impute_missing_values_nada(cleaned_data)
  imputed_data_missforest <- impute_missing_values_missforest(imputed_data_nada)
  set.seed(211)
  full_data <- split_data(imputed_data_missforest)
  rf_model <- random_forest(full_data)
  evaluation <- evaluate_model(rf_model, full_data, target_variable)
  # Visualize Results
  visualize_result_imputed(imputed_data_missforest)
  visualize_result_micro_macro(imputed_data_missforest)
  visualize_result_deployment(imputed_data_missforest)
  visualize_result_filter(imputed_data_missforest)
  
  # Examine feature importance
  importance_scores <- rf_model$importance
  
  # Print evaluation results
  cat("Model RMSE:", evaluation$model_rmse, "\n")
  cat("Baseline RMSE:", evaluation$baseline_rmse, "\n")
}

# Call the main function to run the entire process
main()
