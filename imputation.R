# Assuming your data frame is named 'cleaned_data' and the columns for calculating fitted values are 'drnarea', 'precip', 'lc01dev_lc11dev', 'x50_percent_aep_flood', and 'bsldem30m'
# Also assuming that you've loaded the 'NADA' package using library(NADA)

library(dplyr)
library(NADA)

# Load your dataset (replace "your_dataset.csv" with your actual file)
water_basin <- read.csv("Copy of River_Plastics_Sample_Data - DATA.csv")
water_basin <- clean_names(water_basin)

# Remove Unneccesary Data
water_basin <-select(water_basin, spatial_file_name, standardized_data_in_ppm3, bsldem30m, precip, drnarea, lc01dev_lc11dev, x50_percent_aep_flood)
cleaned_data <- na.omit(water_basin)

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

