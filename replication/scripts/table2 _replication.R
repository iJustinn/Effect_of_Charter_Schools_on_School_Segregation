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
data_filtered_2 <- data %>%
  filter(!is.na(Sc2_regret) & !is.na(Sc2_random_1) & !is.na(Sc2_random_2) & !is.na(Sc2_lucky))

# Calculate the total counts and counts for Mr.Adams (1) and Mr.White (2)
total_counts_2 <- nrow(data_filtered_2)
mr_adams_counts <- sum(data_filtered_2$Sc2_regret == 1)
mr_white_counts <- total_counts_2 - mr_adams_counts

# Calculate the counts and percentages for Mr.Adams
mr_adams_data <- data.frame(
  Group = "Mr.Adams(routine)",
  Regret = paste(mr_adams_counts, "(", round((mr_adams_counts / total_counts_2) * 100, 1), "%)", sep = ""),
  Luck = paste(sum(data_filtered_2$Sc2_lucky == 1), "(", round((sum(data_filtered_2$Sc2_lucky == 1) / total_counts_2) * 100, 1), "%)", sep = "")
)

# Calculate the counts and percentages for Mr. White
mr_white_data <- data.frame(
  Group = "Mr.White(exception)",
  Regret = paste(mr_white_counts, "(", round((mr_white_counts / total_counts_2) * 100, 1), "%)", sep = ""),
  Luck = paste(sum(data_filtered_2$Sc2_lucky == 2), "(", round((sum(data_filtered_2$Sc2_lucky == 2) / total_counts_2) * 100, 1), "%)", sep = "")
)

# Combine the data for Mr. Jones and Mr. Smith into one dataframe
combined_data <- rbind(mr_adams_data, mr_white_data)

# Now use kable to create the table with the combined data
kable_output <- kable(combined_data, "latex", booktabs = TRUE) %>%
  kable_styling(full_width = FALSE, font_size = 7)

kable_output