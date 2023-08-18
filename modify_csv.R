library(dplyr)
library(janitor)
library(tidyr)

modify_csv <- function(pathname, start_row, end_row) {
  # Read CSV
  data <- read_csv(pathname)
  
  # Subset data based on row range
  basin_characteristics <- data %>%
    slice(start_row:end_row)
  
  # Extract first row as new column names
  new_colnames <- as.character(basin_characteristics[1, ])
  
  # Remove the existing column names
  colnames(basin_characteristics) <- NULL
  
  # Assign the first row values as the new column names
  colnames(basin_characteristics) <- new_colnames
  
  # Remove the first row (which is now the new column names)
  basin_characteristics <- basin_characteristics[-1, ]
  
  # Clean data
  basin_characteristics <- clean_names(basin_characteristics)
  
  # Merge columns 
  characteristics_merged <- basin_characteristics %>%
    mutate(parameter = paste(parameter_code, unit, sep = ", "))
  
  # Remove unnecessary columns
  basin_stats <- characteristics_merged %>%
    select(-parameter_code, -unit, -parameter_description, -na, -na_2)
  
  # Wide Format
  basin_stats_wide <- basin_stats %>%
    pivot_wider(names_from = parameter, values_from = value)
  
  # Add data name
  basin_stats_final <- basin_stats_wide %>%
    mutate(sample = c(pathname))
  
  return(basin_stats_final)
}

# data2 <- merge(data1, data2, by = intersect(names(data1), names(data2)), all = TRUE)