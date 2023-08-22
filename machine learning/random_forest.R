# Install and load necessary packages
library(tidyverse)
library(randomForest)
library(janitor)
library(caret)

# Load your dataset (replace "your_dataset.csv" with your actual file)
water_basin <- read.csv("Copy of River_Plastics_Sample_Data - DATA.csv")
water_basin <- clean_names(water_basin)

# Remove Unneccesary Data
water_basin <-select(water_basin, spatial_file_name, standardized_data_in_ppm3, bsldem30m, precip, drnarea, lc01dev_lc11dev, x50_percent_aep_flood)
cleaned_data <- na.omit(water_basin)

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(cleaned_data$standardized_data_in_ppm3, p = 0.7, list = FALSE)
train_data <- cleaned_data[train_indices, ]
test_data <- cleaned_data[-train_indices, ]

# Define the target variable and features
target_variable <- "standardized_data_in_ppm3"
features <- c("bsldem30m", "precip", "drnarea", "lc01dev_lc11dev", "x50_percent_aep_flood")  

# Create training and testing subsets with consistent columns
train_subset <- cleaned_data[, c(target_variable, features)]
test_subset <- cleaned_data[, c(target_variable, features)]

# Create the random forest model
rf_model <- randomForest(
  formula = as.formula(paste(target_variable, " ~ .", sep = "")),
  data = train_subset,
  ntree = 100
)

# Examine feature importance
importance_scores <- rf_model$importance
print(importance_scores)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_subset)

cat("Predicted values:", predictions, "\n")
cat("Actual values:", test_subset$mp_conc_ppm3, "\n")

###

# Evaluate the model (you can use various evaluation metrics)
accuracy <- mean(predictions == test_subset$standardized_data_in_ppm3)

# Calculate RMSE- regression task predicts continuous target variable, remove accuracy calculation
rmse <- sqrt(mean((predictions - test_subset$standardized_data_in_ppm3)^2))
cat("RMSE:", rmse, "\n")

# Calculate baseline prediction (mean or median)
baseline_prediction <- mean(train_data$standardized_data_in_ppm3)  # You can also use median if appropriate

# Create a vector of baseline predictions for the test set
baseline_predictions <- rep(baseline_prediction, nrow(test_data))

# Calculate baseline RMSE
baseline_rmse <- sqrt(mean((baseline_predictions - test_data$standardized_data_in_ppm3)^2))

# Calculate your model's RMSE (you've already calculated this)
model_rmse <- 1535.988  # Replace with your actual model's RMSE

# Compare the two RMSE values
cat("Baseline RMSE:", baseline_rmse, "\n")
cat("Model RMSE:", model_rmse, "\n")