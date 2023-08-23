# Assuming your data frame is named 'cleaned_data' and the columns for calculating fitted values are 'drnarea', 'precip', 'lc01dev_lc11dev', 'x50_percent_aep_flood', and 'bsldem30m'
# Also assuming that you've loaded the 'NADA' package using library(NADA)

library(dplyr)
library(NADA)

# Load your dataset (replace "your_dataset.csv" with your actual file)
water_basin <- read.csv("Copy of River_Plastics_Sample_Data - DATA.csv")
water_basin <- clean_names(water_basin)

# Remove Unneccesary Data
cleaned_data <-select(water_basin, spatial_file_name, standardized_data_in_ppm3, bsldem30m, precip, drnarea, lc01dev_lc11dev, x50_percent_aep_flood)

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




# Assuming your dataframe is named 'cleaned_data'
# Make sure to install and load the 'missForest' package using install.packages("missForest") and library(missForest)

library(missForest)

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

