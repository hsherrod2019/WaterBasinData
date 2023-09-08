# Install and load necessary packages
library(tidyverse)
library(randomForest)
library(janitor)
library(caret)
library(missForest)
library(NADA)
library(ggplot2)
library(data.table)
library(dplyr)


# Parameters
data_file <- "Copy of River_Plastics_Sample_Data - DATA.csv"
target_variable <- "corrected_concentration"
features <- c("bsldem30m", "lc01dev_lc11dev", "x50_percent_aep_flood", "deployment_method", "sample_size", "top_particle", "filter_size", "macro_or_micro", "imputed_standardized_data")

# Load and preprocess data
water_basin <- read.csv(data_file)
water_basin <- clean_names(water_basin)
cleaned_data <- water_basin %>%
  select(spatial_file_name, standardized_data_in_ppm3, all_of(features), precip, drnarea, standardized_concentration_units, study_media)
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
extracted_column2 <- imputed_mp_data$macro_or_micro
extracted_column3 <- imputed_mp_data$standardized_concentration_units
extracted_column4 <- imputed_mp_data$study_media
imputed_mp_data <- imputed_mp_data %>%
  select(-spatial_file_name, -censored, -deployment_method, -doi_part, -macro_or_micro, -standardized_concentration_units, -study_media)
imputed_data <- missForest(imputed_mp_data)
imputed_matrix <- imputed_data$ximp
imputed_dataframe <- as.data.frame(imputed_matrix)
imputed_mp_data[is.na(imputed_mp_data)] <- imputed_dataframe[is.na(imputed_mp_data)]
imputed_data <- imputed_mp_data %>%
  mutate(deployment_method = extracted_column1, 
         macro_or_micro = extracted_column2,
         standardized_concentration_units = extracted_column3,
         study_media = extracted_column4)

# Function to split data into training and testing sets
imputed_data <- imputed_data %>%
  select(-precip, - drnarea, -standardized_data_in_ppm3)

###create function to derive correction factor (CF) from Koelmans et al (equation 2)
CFfnx = function(a, #default alpha from Koelmans et al (2020)
                 x2D, #set default values to convert ranges to (1-5,000 um) #5 mm is the upper default
                 x1D, #1 um is the lower default size
                 x2M, x1M){
  CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a)) 
  return(CF)
}

df <- data.frame(
  concentration_data = imputed_data,
  concentration_type = "length (um)",
  corrected_min = 1,
  corrected_max = 5000
)


#clean incoming data
imputed_data$imputed_standardized_data <- as.numeric(imputed_data$imputed_standardized_data)
imputed_data$filter_size <- as.numeric(imputed_data$filter_size)
imputed_data$top_particle <- as.numeric(imputed_data$top_particle)
imputed_data$study_media <- as.character(imputed_data$study_media)
imputed_data$standardized_concentration_units <- as.character(imputed_data$standardized_concentration_units)

#Make df for alpha values
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


if (any(df$concentration_type == "length (um)")) {
  imputed_data <- merge(x = imputed_data, y = alpha_vals[, c("study_media", "length")], by = "study_media", all.x = TRUE)
  imputed_data <- imputed_data %>% rename("alpha" = "length")
}

imputed_data <- imputed_data %>%
  add_column(correction_factor = NA,
             corrected_concentration = NA)


#Extrapolated parameters
x1D_set = as.numeric(df$corrected_min) #lower limit default extrapolated range is 1 um
x2D_set = as.numeric(df$corrected_max) #upper limit default extrapolated range is 5 mm

imputed_data$correction_factor <- numeric(nrow(imputed_data))  # Initialize with zeros
imputed_data$corrected_concentration <- numeric(nrow(imputed_data))  # Initialize with zeros

for(x in 1:nrow(imputed_data)) {
  x1M_set = as.numeric(imputed_data$filter_size[[x]])
  x2M_set = as.numeric(imputed_data$top_particle[[x]])
  alpha = as.numeric(imputed_data$alpha[[x]])
  
  print(paste("x1M_set:", x1M_set))
  print(paste("x2M_set:", x2M_set))
  print(paste("alpha:", alpha))
  
  CF <- CFfnx(x1M = x1M_set,#lower measured length
              x2M = x2M_set, #upper measured length
              x1D = x1D_set, #default lower size range
              x2D = x2D_set,  #default upper size range
              a = alpha #alpha for count 
              
  )
  print(paste("CF (before formatting):", CF))
  
  imputed_data$correction_factor[x] <- CF
  print(paste("Correction Factor (CF) assigned:", imputed_data$correction_factor[x]))
  
  imputed_data$corrected_concentration[[x]] <- as.numeric(imputed_data$correction_factor[[x]]) * as.numeric(imputed_data$imputed_standardized_data[[x]])
  print(paste("Corrected Concentration:", imputed_data$corrected_concentration[[x]]))
}

######### Where to get rid of imputed_standardized_data, correction_factor, alpha, and study media?
#imputed_data <- imputed_data %>%
  #select(bsldem30m, lc01dev_lc11dev, x50_percent_aep_flood, sample_size, top_particle, filter_size, corrected_concentration, deployment_method, macro_or_micro)

######### Already made numeric right???
#imputed_data$bsldem30m <- as.numeric(imputed_data$bsldem30m)
#imputed_data$lc01dev_lc11dev <- as.numeric(imputed_data$lc01dev_lc11dev)
#imputed_data$x50_percent_aep_flood <- as.numeric(imputed_data$x50_percent_aep_flood)
#imputed_data$sample_size <- as.numeric(imputed_data$sample_size)
#imputed_data$top_particle <- as.numeric(imputed_data$top_particle)
#imputed_data$filter_size <- as.numeric(imputed_data$filter_size)
#imputed_data$corrected_concentration <- as.numeric(imputed_data$corrected_concentration)
#imputed_data$macro_or_micro <- factor(imputed_data$macro_or_micro, levels = c("Macroplastics", "Microplastics"))


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
    select(-standardized_concentration_units, -study_media),
  method = "rf",
  trControl = ctrl,
  ntree = 100
)

# View cross_validation results
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

rmse <- sqrt(mean((predictions - imputed_data$corrected_concentration)^2))

# Calculate baseline prediction (mean or median)
baseline_prediction <- mean(imputed_data$corrected_concentration)

# Create a vector of baseline predictions for the full dataset
baseline_predictions <- rep(baseline_prediction, nrow(imputed_data))

# Calculate baseline RMSE
baseline_rmse <- sqrt(mean((baseline_predictions - imputed_data$corrected_concentration)^2))

# Calculate your model's RMSE
model_rmse <- rmse
list(baseline_rmse = baseline_rmse, model_rmse = rmse)

# Accuracy 
(baseline_rmse - model_rmse) / baseline_rmse * 100

# Importance Scores
importance_scores <- rf_model$finalModel$importance
print(importance_scores)

# Save model
saveRDS(rf_model, file = "rf_model.rds")

saveRDS(imputed_data, file = "imputed_data.rds")

