# Install and load necessary packages
install.packages(c("tidyverse", "randomForest"))
library(tidyverse)
library(randomForest)

# Load your dataset (replace "your_dataset.csv" with your actual file)
water_basin <- read.csv("Water Basin firstlast10merge data - Sheet1.csv")
water_basin <- clean_names(water_basin)

# Explore the dataset (optional)
head(water_basin)
summary(water_basin)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(nrow(water_basin), nrow(water_basin) *0.7)
train_data <- water_basin[train_indices, ]
test_data <- water_basin[-train_indices, ]

# Define the target variable and features
target_variable <- "mp_conc_ppm3"
features <- c("drnarea_square_miles", "forest_percent", "precip_inches", "csl10_85_feet_per_mi", "lc11dev_percent", "lc11imp_percent")  # Add other relevant features

# Create training and testing subsets with consistent columns
train_subset <- train_data[, c(target_variable, features)]
test_subset <- test_data[, c(target_variable, features)]

# Create the random forest model
rf_model <- randomForest(
  formula = as.formula(paste(target_variable, "~.", sep = "")),
  data = train_subset,
  ntree = 100,
  na.action = na.pass
)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_subset)

# Evaluate the model (you can use various evaluation metrics)
accuracy <- mean(predictions == test_subset$mp_conc_ppm3)

# Print the evaluation result
cat("Accuracy:", accuracy, "\n")