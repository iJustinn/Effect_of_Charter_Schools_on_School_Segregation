# filtereddata

example_data <- data %>%
  select(Sc1_regret, sc1_socnorms1, sc1_socnorms2, sc1_combinednorms, Sc2_regret, Sc2_lucky) %>%
  rename(
    "Experiment 1 regret" = Sc1_regret,
    "Injuctive norms" = sc1_socnorms1,
    "Descriptive norms" = sc1_socnorms2,
    "Negative effect" = sc1_combinednorms,
    "Experiment 2 regret" = Sc2_regret,
    "Luck" = Sc2_lucky
  ) %>%
  head(10)  # Selecting only the top 10 rows for the table

# Generating the table using kable and kableExtra for styling
example_kable <- kable(example_data, format = "latex", booktabs = TRUE, align='c') %>% # align the text in the middle
  kable_styling(full_width = FALSE, font_size = 7) 
example_kable



# experiment1

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



# experiment2

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
