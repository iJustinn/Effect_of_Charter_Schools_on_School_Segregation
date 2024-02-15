# load packages
library(tidyverse)
library(ggplot2)
library(janitor)
library(dplyr)
library(readr)
library(knitr)
library(kableExtra)
library(reshape2)

# read in data
file_path <- "inputs/data/raw_data.csv"
data <- read_csv(file_path, show_col_types = FALSE)

# Filter the data to drop NA values
data_filtered_1 <- data %>%
  filter(!is.na(Sc1_regret) & !is.na(sc1_socnorms1) & !is.na(sc1_socnorms2) & !is.na(sc1_combinednorms))

# Calculate the total counts and counts for Mr.Jones (1) and Mr.Smith (2)
total_counts_1 <- nrow(data_filtered_1)
mr_jones_counts <- sum(data_filtered_1$Sc1_regret == 1)
mr_smith_counts <- total_counts_1 - mr_jones_counts

# Calculate the counts and percentages for Mr.Jones
mr_jones_data <- data.frame(
  Group = "Mr.Jones(exception)",
  Regret = paste(mr_jones_counts, "(", round((mr_jones_counts / total_counts_1) * 100, 1), "%)", sep = ""),
  Injunctive = paste(sum(data_filtered_1$sc1_socnorms1 == 1), "(", round((sum(data_filtered_1$sc1_socnorms1 == 1) / total_counts_1) * 100, 1), "%)", sep = ""),
  Descriptive = paste(sum(data_filtered_1$sc1_socnorms2 == 1), "(", round((sum(data_filtered_1$sc1_socnorms2 == 1) / total_counts_1) * 100, 1), "%)", sep = ""),
  `Negative Effect` = paste(sum(data_filtered_1$sc1_combinednorms == 1), "(", round((sum(data_filtered_1$sc1_combinednorms == 1) / total_counts_1) * 100, 1), "%)", sep = "")
)

# Calculate the counts and percentages for Mr.Smith
mr_smith_data <- data.frame(
  Group = "Mr.Smith(routine)",
  Regret = paste(mr_smith_counts, "(", round((mr_smith_counts / total_counts_1) * 100, 1), "%)", sep = ""),
  Injunctive = paste(sum(data_filtered_1$sc1_socnorms1 == 2), "(", round((sum(data_filtered_1$sc1_socnorms1 == 2) / total_counts_1) * 100, 1), "%)", sep = ""),
  Descriptive = paste(sum(data_filtered_1$sc1_socnorms2 == 2), "(", round((sum(data_filtered_1$sc1_socnorms2 == 2) / total_counts_1) * 100, 1), "%)", sep = ""),
  `Negative Effect` = paste(sum(data_filtered_1$sc1_combinednorms == 2), "(", round((sum(data_filtered_1$sc1_combinednorms == 2) / total_counts_1) * 100, 1), "%)", sep = "")
)

# Combine the data for Mr. Jones and Mr. Smith into one dataframe
combined_data <- rbind(mr_jones_data, mr_smith_data)

# Now use kable to create the table with the combined data
kable_output <- kable(combined_data, "latex", booktabs = TRUE) %>%
  kable_styling(full_width = FALSE, font_size = 7)

kable_output
