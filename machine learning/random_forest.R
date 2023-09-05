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
target_variable <- "imputed_standardized_data"
features <- c("bsldem30m", "lc01dev_lc11dev", "x50_percent_aep_flood", "deployment_method", "sample_size", "top_particle", "filter_size", "macro_or_micro")

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
  select(-precip, - drnarea, -standardized_data_in_ppm3) %>%
  mutate(imputed_standardized_data = log10(imputed_standardized_data))

###create function to derive correction factor (CF) from Koelmans et al (equation 2)
CFfnx = function(a, #default alpha from Koelmans et al (2020)
                 x2D, #set default values to convert ranges to (1-5,000 um) #5 mm is the upper default
                 x1D, #1 um is the lower default size
                 x2M, x1M){
  CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a)) 
  return(CF)
}

#Build cleaning functions
cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}

removeslash <- function(x){
  gsub("/", " OR ", x)
}

cleanmaterials <- function(x) {
  x <- x[!names(x) %in% c("X1", "X2", "X3", "X4", "X5")]
  if(is.list(x)) lapply(x, cleanmaterials)
}

cleanitems <- function(x) {
  x <- x[!names(x) %in% c("X1", "X2", "X3", "X4", "X5", "X6")]
  if(is.list(x)) lapply(x, cleanitems)
}


df <- data.frame(
  concentration_data <- imputed_data,
  concentration_type <- "length (um)",
  corrected_min <- 1,
  corrected_max <- 5000
)


#clean incoming data
dataframe <- as.data.frame(imputed_data)
dataframe$imputed_standardized_data <- as.numeric(dataframe$imputed_standardized_data)
dataframe$filter_size <- as.numeric(dataframe$filter_size)
dataframe$top_particle <- as.numeric(dataframe$top_particle)
dataframe$study_media <- as.character(dataframe$study_media)
dataframe$standardized_concentration_units <- as.character(dataframe$standardized_concentration_units)
dataframeclean <- mutate_all(dataframe, cleantext) 

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


if(df$concentration_type == "length (um)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "length")], by = "study_media", all.x=TRUE)
dataframeclean <- dataframeclean %>%
  rename("alpha" = "length")}


dataframeclean <- dataframeclean %>%
  add_column(correction_factor = NA,
             corrected_concentration = NA)


#Extrapolated parameters
x1D_set = as.numeric(df$corrected_min) #lower limit default extrapolated range is 1 um
x2D_set = as.numeric(df$corrected_max) #upper limit default extrapolated range is 5 mm

for(x in 1:nrow(dataframeclean)) {
  x1M_set = as.numeric(dataframeclean$filter_size[[x]])
  x2M_set = as.numeric(dataframeclean$top_particle[[x]])
  alpha = as.numeric(dataframeclean$alpha[[x]])
  
  CF <- CFfnx(x1M = x1M_set,#lower measured length
              x2M = x2M_set, #upper measured length
              x1D = x1D_set, #default lower size range
              x2D = x2D_set,  #default upper size range
              a = alpha #alpha for count 
              
  )
  
  CF <- as.numeric(CF)
  
  CF <- format(round(CF, 2), nsmall = 2)
  
  
  dataframeclean$correction_factor[[x]] <- CF
  
  dataframeclean$corrected_concentration[[x]] <- as.numeric(dataframeclean$correction_factor[[x]]) * as.numeric(dataframeclean$imputed_standardized_data[[x]])
  
}

return(dataframeclean)

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
  data = dataframeclean %>%
    select(-standardized_concentration_units, -study_media),
  method = "rf",
  trControl = ctrl,
  ntree = 100
)

# View cross_validation results
print(rf_model)

# Calculate training metrics
train_predictions <- predict(rf_model, newdata = dataframeclean)
train_rmse <- sqrt(mean((train_predictions - dataframeclean$corrected_concentration)^2))
train_rsq <- 1 - sum((train_predictions - dataframeclean$corrected_concentration)^2) / sum((dataframeclean$corrected_concentration - mean(dataframeclean$corrected_concentration))^2)
train_mae <- mean(abs(train_predictions - dataframeclean$corrected_concentration))

# Print the training metrics
cat("Training RMSE:", train_rmse, "\n")
cat("Training Rsquared:", train_rsq, "\n")
cat("Training MAE:", train_mae, "\n")

# Function to evaluate the model
predictions <- predict(rf_model, dataframeclean)

rmse <- sqrt(mean((predictions - dataframeclean$corrected_concentration)^2))

# Calculate baseline prediction (mean or median)
baseline_prediction <- mean(dataframeclean$corrected_concentration)

# Create a vector of baseline predictions for the full dataset
baseline_predictions <- rep(baseline_prediction, nrow(imputed_data))

# Calculate baseline RMSE
baseline_rmse <- sqrt(mean((baseline_predictions - dataframeclean$corrected_concentration)^2))

# Calculate your model's RMSE
model_rmse <- rmse
list(baseline_rmse = baseline_rmse, model_rmse = rmse)

# Accuracy 
(baseline_rmse - model_rmse) / baseline_rmse * 100

# Importance Scores
importance_scores <- rf_model$finalModel$importance
print(importance_scores)

# Reverse log transform
imputed_data$imputed_standardized_data <- 10^dataframeclean$corrected_concentration

# Save model
saveRDS(rf_model, file = "rf_model.rds")

saveRDS(imputed_data, file = "imputed_data.rds")

