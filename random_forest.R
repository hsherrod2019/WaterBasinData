# Install and load necessary packages
library(tidyverse)
library(randomForest)
library(janitor)
library(caret)

# Load your dataset (replace "your_dataset.csv" with your actual file)
water_basin <- read.csv("Water Basin firstlast10merge data - Sheet1.csv")
water_basin <- clean_names(water_basin)

# Explore the dataset (optional)
head(water_basin)
summary(water_basin)

imputed_data <- water_basin %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(imputed_data$mp_conc_ppm3, p = 0.7, list = FALSE)
train_data <- imputed_data[train_indices, ]
test_data <- imputed_data[-train_indices, ]

# Define the target variable and features
target_variable <- "mp_conc_ppm3"
features <- c("drnarea_square_miles", "forest_percent", "precip_inches", "csl10_85_feet_per_mi", "lc11dev_percent", "lc11imp_percent")  # Add other relevant features

# Create training and testing subsets with consistent columns
train_subset <- train_data[, c(target_variable, features)]
test_subset <- test_data[, c(target_variable, features)]

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

# Evaluate the model (you can use various evaluation metrics)
accuracy <- mean(predictions == test_subset$mp_conc_ppm3)

# Print the evaluation result
cat("Accuracy:", accuracy, "\n")
