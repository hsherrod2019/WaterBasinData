library(dplyr)
library(stringr)

existing_data <- cleaned_data %>%
  mutate(
    doi_part = gsub('.*(doi\\.org/[^|]+).*', '\\1', spatial_file_name)
  )

