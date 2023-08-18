library(tidyr)
library(readr)
library(dplyr)

test_csv <- read.csv("Bacon_Rind_1A|doi.org:10.1016:j.watres.2018.10.013.csv")
test_csv

test_csv <- test_csv %>%
  mutate(RowIndex = row_number())

filtered_data <- test_csv %>%
  filter(RowIndex >= 9 & RowIndex <= 34)

wide_data <- filtered_data %>%
  pivot_wider(names_from = StreamStats.Output.Report,
              values_from = X.1)

wide_data <- wide_data %>%
  select(-RowIndex)

cleaned_data <- wide_data %>%
  select(-c(X, X.2, X.3, X.4))

reshaped_data <- cleaned_data %>%
  pivot_wider(
    names_from = StreamStats.Output.Report,
    values_from = X.1
  )
