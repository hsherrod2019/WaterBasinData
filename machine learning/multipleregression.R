# Load necessary libraries
library(tidyverse)
library(janitor)
library(caret)
library(dplyr)
library(ggplot2)

# Load your dataset (replace "your_dataset.csv" with your actual file)
water_basin <- read.csv("Water Basin firstlast10merge data - Sheet1.csv")
water_basin <- clean_names(water_basin)

# Explore the dataset (optional)
head(water_basin)
summary(water_basin)

# Identify columns with missing values
missing_columns <- colnames(water_basin)[apply(is.na(water_basin), 2, any)]
print(missing_columns)

# Impute missing values with median for numeric columns
imputed_data <- water_basin %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(imputed_data$mp_conc_ppm3, p = 0.7, list = FALSE)
train_data <- imputed_data[train_indices, ]
test_data <- imputed_data[-train_indices, ]

# Define the target variable and predictor variables (features)
target_variable <- "mp_conc_ppm3"
features <- c("drnarea_square_miles", "forest_percent", "precip_inches", "csl10_85_feet_per_mi", "lc11dev_percent", "lc11imp_percent")  # Add more features

# Create the multiple linear regression model
lm_model <- lm(formula(paste(target_variable, "~", paste(features, collapse = "+"))),
               data = train_data)

# Summarize the model
summary(lm_model)

# Make predictions on the test set
predictions <- predict(lm_model, newdata = test_data)

# Calculate the RMSE
rmse <- sqrt(mean((predictions - test_data$mp_conc_ppm3)^2, na.rm = TRUE))

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
