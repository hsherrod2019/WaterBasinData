# Install and load necessary packages
library(tidyverse)
library(randomForest)
library(janitor)
library(caret)
library(missForest)

# Load your dataset (replace "your_dataset.csv" with your actual file)
water_basin <- read.csv("Copy of River_Plastics_Sample_Data - DATA.csv")
water_basin <- clean_names(water_basin)

# Remove Unneccesary Data
cleaned_data <-select(water_basin, spatial_file_name, standardized_data_in_ppm3, bsldem30m, lc01dev_lc11dev, x50_percent_aep_flood)

# Add a new column that checks for zeros
cleaned_data <- cleaned_data %>%
  mutate(censored = ifelse(standardized_data_in_ppm3 == 0, TRUE, FALSE))

fit <- cenros(cleaned_data$standardized_data_in_ppm3, cleaned_data$censored)


set.seed(211)

# Impute zero values using the cenros function from NADA package
fittedvalues <- sample(fit$modeled[fit$censored], length(fit$modeled[fit$censored]), replace = F)

#Update the data frame with imputed values
cleaned_data <- cleaned_data %>%
  mutate(imputed_standardized_data = ifelse(censored, fittedvalues, standardized_data_in_ppm3))

cleaned_data <-select(cleaned_data, -spatial_file_name, -censored)

# Impute missing values using missForest
imputed_data <- missForest(cleaned_data)

# The imputed_data object now contains the imputed values

# You can access the imputed data matrix using:
imputed_matrix <- imputed_data$ximp

# Convert the imputed matrix back to a dataframe
imputed_dataframe <- as.data.frame(imputed_matrix)

# Replace the NA values in the original dataframe with the imputed values
cleaned_data[is.na(cleaned_data)] <- imputed_dataframe[is.na(cleaned_data)]

# Now 'cleaned_data' contains the imputed values for missing NA values

# Split the data into training and testing sets
set.seed(211)
train_indices <- createDataPartition(cleaned_data$imputed_standardized_data, p = 0.7, list = FALSE)
train_data <- cleaned_data[train_indices, ]
test_data <- cleaned_data[-train_indices, ]

# Define the target variable and features
target_variable <- "imputed_standardized_data"
features <- c("bsldem30m", "lc01dev_lc11dev", "x50_percent_aep_flood")  

# Create training and testing subsets with consistent columns
train_subset <- cleaned_data[train_indices, c(target_variable, features)]
test_subset <- cleaned_data[-train_indices, c(target_variable, features)]

full_data <- cleaned_data[, c(target_variable, features)]
# Create the random forest model
rf_model <- randomForest(
  formula = as.formula(paste(target_variable, " ~ .", sep = "")),
  data = full_data,
  ntree = 100
)

# Examine feature importance
importance_scores <- rf_model$importance
print(importance_scores)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = full_data)

cat("Predicted values:", predictions, "\n")
cat("Actual values:", full_data$imputed_standardized_data, "\n")

###

# Evaluate the model (you can use various evaluation metrics)
accuracy <- mean(predictions == full_data$imputed_standardized_data)

# Calculate RMSE- regression task predicts continuous target variable, remove accuracy calculation
rmse <- sqrt(mean((predictions - full_data$imputed_standardized_data)^2))
cat("RMSE:", rmse, "\n")

# Calculate baseline prediction (mean or median)
baseline_prediction <- mean(full_data$imputed_standardized_data)  # You can also use median if appropriate

# Create a vector of baseline predictions for the test set
baseline_predictions <- rep(baseline_prediction, nrow(full_data))

# Calculate baseline RMSE
baseline_rmse <- sqrt(mean((baseline_predictions - full_data$imputed_standardized_data)^2))


# Calculate your model's RMSE (you've already calculated this)
model_rmse <- rmse  # Replace with your actual model's RMSE

# Compare the two RMSE values
cat("Baseline RMSE:", baseline_rmse, "\n")
cat("Model RMSE:", model_rmse, "\n")

### Density Plot to confirm similarity

ggplot(cleaned_data, aes(x = standardized_data_in_ppm3, fill = "Original")) +
  geom_density(alpha = 0.5) +
  geom_density(data = cleaned_data, aes(x = imputed_standardized_data, fill = "Imputed"), alpha = 0.5) +
  labs(title = "Density Plot: Original vs. Imputed",
       x = "original",
       y = "imputed") +
  scale_fill_manual(values = c("Original" = "blue", "Imputed" = "red")) +
  scale_x_log10()+
  theme_minimal()
