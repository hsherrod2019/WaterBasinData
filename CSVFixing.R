library(readr)
data <- read_csv("doi.org:10.1021:acs.est.1c03019|cc_1025.csv")

csv_content <- readLines("doi.org:10.1021:acs.est.1c03019|cc_1025.csv")
csv_content <- gsub("\"", "", csv_content)
csv_content <- paste(csv_content, collapse = "\n")

data2 <- read.table(text = csv_content, sep = ",", header = TRUE, fill = TRUE, quote = "", col.names = c("Parameter Code", "Parameter Description", "Value", "Unit"))
