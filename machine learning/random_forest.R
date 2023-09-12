# Install and load necessary packages
library(tidyverse)    # Load the tidyverse packages for data manipulation and visualization
library(randomForest)  # Load the randomForest package for building random forest models
library(janitor)      # Load the janitor package for data cleaning
library(caret)        # Load the caret package for model training
library(missForest)   # Load the missForest package for missing data imputation
library(NADA)         # Load the NADA package for statistical analysis of missing data
library(ggplot2)      # Load ggplot2 for data visualization
library(data.table)   # Load data.table for data manipulation efficiency
library(dplyr)        # Load dplyr for data manipulation

# Parameters
data_file <- "Copy of River_Plastics_Sample_Data - DATA.csv"  # Set the data file path
target_variable <- "corrected_concentration"  # Define the target variable
features <- c("bsldem30m", "lc01dev_lc11dev", "x50_percent_aep_flood", "deployment_method", "sample_size", "top_particle", "filter_size", "macro_or_micro")  # Define feature variables

# Load and preprocess data
water_basin <- read.csv(data_file)  # Read the CSV data file
water_basin <- clean_names(water_basin)  # Clean column names
cleaned_data <- water_basin %>%
  select(spatial_file_name, standardized_data_in_ppm3, all_of(features), precip, drnarea, standardized_concentration_units, study_media, longitude, latitude, river_name)  # Select relevant columns
columns_to_check <- c("bsldem30m", "lc01dev_lc11dev", "x50_percent_aep_flood", "precip", "drnarea")
cleaned_data <- cleaned_data %>%
  filter(!rowSums(is.na(.[, columns_to_check])) == length(columns_to_check))  # Remove rows with missing values in specific columns
cleaned_data <- cleaned_data %>%
  mutate(censored = ifelse(standardized_data_in_ppm3 == 0, TRUE, FALSE))  # Create a 'censored' column
cleaned_data <- cleaned_data %>%
  mutate(doi_part = str_extract(spatial_file_name, 'doi\\.org/[^|]+'))  # Extract 'doi_part' from 'spatial_file_name'

# Unique doi_part values
unique_doi_parts <- unique(cleaned_data$doi_part)  # Find unique 'doi_part' values

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
# This function uses the missForest package to impute missing values in the 'imputed_mp_data' data frame.
# Extract columns for future restoration
extracted_column1 <- imputed_mp_data$deployment_method
extracted_column2 <- imputed_mp_data$macro_or_micro
extracted_column3 <- imputed_mp_data$standardized_concentration_units
extracted_column4 <- imputed_mp_data$study_media
extracted_column5 <- imputed_mp_data$longitude
extracted_column6 <- imputed_mp_data$latitude
extracted_column7 <- imputed_mp_data$river_name

# Remove selected columns that will be imputed
imputed_mp_data <- imputed_mp_data %>%
  select(-spatial_file_name, -censored, -deployment_method, -doi_part, -macro_or_micro, -standardized_concentration_units, -study_media, -longitude, -latitude, -river_name)

# Impute missing values in the remaining columns using missForest
imputed_data <- missForest(imputed_mp_data)

# Extract the imputed data as a matrix and convert it to a data frame
imputed_matrix <- imputed_data$ximp
imputed_dataframe <- as.data.frame(imputed_matrix)

# Restore imputed values back to the original data frame
imputed_mp_data[is.na(imputed_mp_data)] <- imputed_dataframe[is.na(imputed_mp_data)]

# Assign imputed values back to their respective columns
imputed_data <- imputed_mp_data %>%
  mutate(deployment_method = extracted_column1, 
         macro_or_micro = extracted_column2,
         standardized_concentration_units = extracted_column3,
         study_media = extracted_column4,
         longitude = extracted_column5,
         latitude = extracted_column6,
         river_name = extracted_column7
  )

# Function to split data into training and testing sets
# This section prepares the data for model training by selecting relevant columns and creating a data frame for alpha values.

# Exclude columns 'precip' and 'drnarea' as they won't be used for model training
imputed_data <- imputed_data %>%
  select(-precip, -drnarea, -standardized_data_in_ppm3)

# Define a function to derive the correction factor (CF) from Koelmans et al. (equation 2)
# This function calculates the correction factor based on specified parameters.
CFfnx = function(a, # Default alpha from Koelmans et al. (2020)
                 x2D, # Upper default size range (1-5,000 um, 5 mm is the upper default)
                 x1D, # Lower default size range (1 um)
                 x2M, x1M){
  CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a)) 
  return(CF)
}

# Create a data frame 'df' with relevant information
df <- data.frame(
  concentration_data = imputed_data,
  concentration_type = "length (um)",
  corrected_min = 1,
  corrected_max = 100000
)

# Clean incoming data types
imputed_data$imputed_standardized_data <- as.numeric(imputed_data$imputed_standardized_data)
imputed_data$filter_size <- as.numeric(imputed_data$filter_size)
imputed_data$top_particle <- as.numeric(imputed_data$top_particle)
imputed_data$study_media <- as.character(imputed_data$study_media)
imputed_data$standardized_concentration_units <- as.character(imputed_data$standardized_concentration_units)

# Create a data frame 'alpha_vals' for alpha values
study_media <- "freshwatersurface"
length <- 2.64
mass <- 1.65
volume <- 1.50
surface_area <- 2.00
specific_surface_area <- 2.71

alpha_vals <- data.frame(study_media=study_media,
                         length=length,
                         mass=mass,
                         volume=volume,
                         surface_area=surface_area,
                         specific_surface_area=specific_surface_area
)

# Merge alpha values if 'concentration_type' is "length (um)"
if (any(df$concentration_type == "length (um)")) {
  imputed_data <- merge(x = imputed_data, y = alpha_vals[, c("study_media", "length")], by = "study_media", all.x = TRUE)
  imputed_data <- imputed_data %>% rename("alpha" = "length")
}

# Add columns for 'correction_factor' and 'corrected_concentration'
imputed_data <- imputed_data %>%
  add_column(correction_factor = NA,
             corrected_concentration = NA)

# Initialize parameters for extrapolation
x1D_set = as.numeric(df$corrected_min) # Lower limit default extrapolated range is 1 um
x2D_set = as.numeric(df$corrected_max) # Upper limit default extrapolated range is 5 mm

imputed_data$correction_factor <- numeric(nrow(imputed_data))  # Initialize with zeros
imputed_data$corrected_concentration <- numeric(nrow(imputed_data))  # Initialize with zeros

# Loop through each row to calculate correction factor and corrected concentration
for(x in 1:nrow(imputed_data)) {
  x1M_set = as.numeric(imputed_data$filter_size[[x]])
  x2M_set = as.numeric(imputed_data$top_particle[[x]])
  alpha = as.numeric(imputed_data$alpha[[x]])
  
  # Calculate Correction Factor (CF) using the CFfnx function
  CF <- CFfnx(x1M = x1M_set,
              x2M = x2M_set,
              x1D = x1D_set,
              x2D = x2D_set,
              a = alpha
  )
  
  # Assign Correction Factor (CF) and calculate Corrected Concentration
  imputed_data$correction_factor[x] <- CF
  imputed_data$corrected_concentration[[x]] <- as.numeric(imputed_data$correction_factor[[x]]) * as.numeric(imputed_data$imputed_standardized_data[[x]])
}

# Define the formula for your model
formula <- corrected_concentration ~ .

# Create the Random Forest model with k-fold cross-validation (5 folds for example)
set.seed(211)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

# Train the model
rf_model <- train(
  formula,
  data = imputed_data %>%
    select(-imputed_standardized_data, -alpha, -correction_factor, -study_media, -standardized_concentration_units, -longitude, -latitude, -river_name),
  method = "rf",
  trControl = ctrl,
  ntree = 100
)

# View cross-validation results
print(rf_model)

# Calculate training metrics
train_predictions <- predict(rf_model, newdata = imputed_data)
train_rmse <- sqrt(mean((train_predictions - imputed_data$corrected_concentration)^2))
train_rsq <- 1 - sum((train_predictions - imputed_data$corrected_concentration)^2) / sum((imputed_data$corrected_concentration - mean(imputed_data$corrected_concentration))^2)
train_mae <- mean(abs(train_predictions - imputed_data$corrected_concentration))

# Print the training metrics
cat("Training RMSE:", train_rmse, "\n")
cat("Training Rsquared:", train_rsq, "\n")
cat("Training MAE:", train_mae, "\n")

# Function to evaluate the model
predictions <- predict(rf_model, imputed_data)

# Calculate prediction intervals (lower and upper bounds)
prediction_intervals <- quantile(predictions, c(0.05, 0.95))
lower_bounds <- prediction_intervals[1]
upper_bounds <- prediction_intervals[2]

# Calculate the minimum and maximum values of the actual target variable
min_actual_value <- min(imputed_data$corrected_concentration)
max_actual_value <- max(imputed_data$corrected_concentration)

# Print the minimum and maximum actual values
cat("Minimum Actual Value:", min_actual_value, "\n")
cat("Maximum Actual Value:", max_actual_value, "\n")

# Calculate RMSE
rmse <- sqrt(mean((predictions - imputed_data$corrected_concentration)^2))

# Calculate baseline prediction (mean or median)
baseline_prediction <- mean(imputed_data$corrected_concentration)

# Create a vector of baseline predictions for the full dataset
baseline_predictions <- rep(baseline_prediction, nrow(imputed_data))

# Calculate baseline RMSE
baseline_rmse <- sqrt(mean((baseline_predictions - imputed_data$corrected_concentration)^2))

# Calculate your model's RMSE
model_rmse <- rmse

# Calculate the accuracy improvement in percentage
accuracy_improvement_percent <- (baseline_rmse - model_rmse) / baseline_rmse * 100

# Importance Scores
importance_scores <- rf_model$finalModel$importance
print(importance_scores)

# Save model
# Save the trained Random Forest model to a file for future use
saveRDS(rf_model, file = "rf_model.rds")

# Save data frames
# Save the imputed data, excluding latitude, longitude, and river_name columns
saveRDS(imputed_data %>%
          select(-latitude, -longitude, -river_name), file = "imputed_data.rds")

# Save map-related data
# Save the data frame containing latitude, longitude, river_name, and corrected_concentration columns
saveRDS(imputed_data %>%
          select(longitude, latitude, river_name, corrected_concentration), file = "map_data.rds")
