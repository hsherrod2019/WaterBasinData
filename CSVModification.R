# Water Basin Data CSV Modification


modify_csv <- function(input_file, output_file) {
  data <- read.csv(input_file)
  
  # manipulate data structure
  
write.csv(data, output_file, row.names = FALSE)
}

# apply the function to multiple files
input_files <- c("file1.csv", "file2.csv", etc)
output_files <- c("file1_modified.csv", "file2_modified.csv", etc)

for (i in seq_along(input_files)) {
  modify_csv(input_files[i], output_files[i])
}


# ways to manipulate the data structure

#wide to long data
long_data <- pivot_longer(data, cols = c(col1, col2), names_to = "variable_name")

#transpose to make rows into columns
selected_data <- data[3:7, ]
selected_data <- t(selected_data)

