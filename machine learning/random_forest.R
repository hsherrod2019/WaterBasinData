# Install and load necessary packages
library(tidyverse)
library(randomForest)
library(janitor)
library(caret)
library(missForest)
library(NADA)
library(ggplot2)
library(dplyr)


# Parameters
data_file <- "Copy of River_Plastics_Sample_Data - DATA.csv"
target_variable <- "imputed_standardized_data"
features <- c("bsldem30m", "lc01dev_lc11dev", "x50_percent_aep_flood", "deployment_method", "sample_size", "top_particle", "filter_size")

# Load and preprocess data
water_basin <- read.csv(data_file)
water_basin <- clean_names(water_basin)
cleaned_data <- water_basin %>%
  select(spatial_file_name, standardized_data_in_ppm3, all_of(features), precip, drnarea)
columns_to_check <- c("bsldem30m", "lc01dev_lc11dev", "x50_percent_aep_flood", "precip", "drnarea")
cleaned_data <- cleaned_data %>%
  filter(!rowSums(is.na(.[, columns_to_check])) == length(columns_to_check))
cleaned_data <- cleaned_data %>%
  mutate(censored = ifelse(standardized_data_in_ppm3 == 0, TRUE, FALSE))
cleaned_data <- cleaned_data %>%
    mutate(doi_part = str_extract(spatial_file_name, 'doi\\.org/[^|]+'))

# Unique doi_part values
unique_doi_parts <- unique(cleaned_data$doi_part)

# Initialize an empty list to store the imputed data frames
imputed_data_list <- list()

# Loop through each unique doi_part
for (doi_part_value in unique_doi_parts) {
  # Subset data for the current doi_part
  subset_data <- cleaned_data %>% filter(doi_part == doi_part_value)
  
  tryCatch({
    # Perform cenros on the subset data
    fit <- cenros(subset_data$standardized_data_in_ppm3, subset_data$censored)
    set.seed(211)
    fittedvalues <- sample(fit$modeled[fit$censored], length(fit$modeled[fit$censored]), replace = FALSE)
    
    # Create a new data frame with imputed values
    imputed_subset_data <- subset_data %>%
      mutate(imputed_standardized_data = ifelse(censored, fittedvalues, standardized_data_in_ppm3))
    
    # Append the imputed data to the list
    imputed_data_list[[doi_part_value]] <- imputed_subset_data
  }, error = function(e) {
    # If an error occurs (e.g., not enough non-missing values for cenros),
    # simply append the subset_data to the list
    imputed_data_list[[doi_part_value]] <- subset_data
  })
}

# Combine the imputed data frames from the list into a single data frame
imputed_mp_data <- bind_rows(imputed_data_list)

# Function to impute missing values using missForest
extracted_column1 <- imputed_mp_data$deployment_method
imputed_mp_data <- imputed_mp_data %>%
  select(-spatial_file_name, -censored, -deployment_method, -doi_part)
imputed_data <- missForest(imputed_mp_data)
imputed_matrix <- imputed_data$ximp
imputed_dataframe <- as.data.frame(imputed_matrix)
imputed_mp_data[is.na(imputed_mp_data)] <- imputed_dataframe[is.na(imputed_mp_data)]
imputed_data <- imputed_mp_data %>%
  mutate(deployment_method = extracted_column1)

# Function to split data into training and testing sets
full_data <- imputed_data %>%
  select(-precip, - drnarea, -standardized_data_in_ppm3) %>%
  mutate(imputed_standardized_data = log10(imputed_standardized_data))


# Function to create and train random forest model
rf_model <- randomForest(
  formula = imputed_standardized_data ~ .,
  data = full_data,
  ntree = 100
)

saveRDS(rf_model, file = "rf_model.rds")

saveRDS(full_data, file = "full_data.rds")

# Examine feature importance
importance_scores <- rf_model$importance

# Function to evaluate the model
predictions <- predict(rf_model, full_data)

rmse <- sqrt(mean((predictions - full_data$imputed_standardized_data)^2))

# Calculate baseline prediction (mean or median)
baseline_prediction <- mean(full_data$imputed_standardized_data)

# Create a vector of baseline predictions for the full dataset
baseline_predictions <- rep(baseline_prediction, nrow(full_data))

# Calculate baseline RMSE
baseline_rmse <- sqrt(mean((baseline_predictions - full_data$imputed_standardized_data)^2))

# Calculate your model's RMSE
model_rmse <- rmse
list(baseline_rmse = baseline_rmse, model_rmse = rmse)

# Accuracy 
(baseline_rmse - model_rmse) / baseline_rmse * 100

# Reverse log transform
10^predictions

################ Plots
# Density plot of macro/micro vs imputed_standardized_data 
ggplot(full_data, aes(x = imputed_standardized_data, fill = macro_or_micro)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Imputed Standardized Data by Macro/Micro",
       x = "Imputed Standardized Data (Log Scale)",
       y = "Density") +
  scale_x_log10() +  # Add logarithmic scale to the x-axis
  theme_minimal()

# Density plot of deployment_method vs imputed_standardized_data
ggplot(full_data, aes(x = imputed_standardized_data, fill = deployment_method)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Imputed Standardized Data by Deployment Method",
       x = "Imputed Standardized Data",
       y = "Density") +
  scale_x_log10() +  # Add logarithmic scale to the x-axis
  theme_minimal()


# Original vs imputed_standardized_data
ggplot(data, aes(x = standardized_data_in_ppm3, fill = "Original")) +
geom_density(alpha = 0.5) +
  geom_density(data = data, aes(x = imputed_standardized_data, fill = "Imputed"), alpha = 0.5) +
  labs(title = "Density Plot: Original vs. Imputed",
       x = "original",
       y = "imputed") +
  scale_fill_manual(values = c("Original" = "blue", "Imputed" = "red")) +
  scale_x_log10() + # Add logarithmic scale to the x-axis
  theme_minimal()

# Smoothed scatter plot of imputed_standardized_data vs filter_size
ggplot(full_data, aes(x = filter_size, y = imputed_standardized_data)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +  # Add smoothed line without confidence interval
  labs(title = "Smoothed Scatter Plot: Imputed Standardized Data vs. Filter Size",
       x = "Filter Size",
       y = "Imputed Standardized Data") +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal()
