# Load necessary libraries
library(tidyverse)
library(janitor)
library(caret)
library(dplyr)
library(ggplot2)

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

# Create the multiple linear regression model
formula_str <- as.formula(paste(target_variable, "~", paste(features, collapse = "+")))
lm_model <- lm(formula_str, data = train_data)

# Summarize the model
summary(lm_model)

# Make predictions on the test set
predictions <- predict(lm_model, newdata = test_data)

# Calculate the RMSE
rmse <- sqrt(mean((predictions - test_data$standardized_data_in_ppm3)^2, na.rm = TRUE))

# Print the evaluation result
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Create a histogram for the variable mp_conc_ppm3
ggplot(water_basin, aes(x = mp_conc_ppm3)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  labs(title = "Histogram of mp_conc_ppm3")

# Create a density plot for the variable mp_conc_ppm3
ggplot(water_basin, aes(x = mp_conc_ppm3)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of mp_conc_ppm3")
