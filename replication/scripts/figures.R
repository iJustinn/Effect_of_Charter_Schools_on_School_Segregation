# experiment1bar

smith_regret_p <- round((mr_smith_counts / total_counts_1) * 100, 1)
smith_injunctive_p <- round((sum(data_filtered_1$sc1_socnorms1 == 2) / total_counts_1) * 100, 1)
smith_descriptive_p <- round((sum(data_filtered_1$sc1_socnorms2 == 2) / total_counts_1) * 100, 1)
smith_negative_p <- round((sum(data_filtered_1$sc1_combinednorms == 2) / total_counts_1) * 100, 1)
jones_regret_p <- round((mr_jones_counts / total_counts_1) * 100, 1)
jones_injunctive_p <- round((sum(data_filtered_1$sc1_socnorms1 == 1) / total_counts_1) * 100, 1)
jones_descriptive_p <- round((sum(data_filtered_1$sc1_socnorms2 == 1) / total_counts_1) * 100, 1)
jones_negative_p <- round((sum(data_filtered_1$sc1_combinednorms == 1) / total_counts_1) * 100, 1)

# Create a data frame with the provided data
df <- data.frame(
  Category = c("Regret", "Injunctive Social Norm", "Descriptive Social Norm", "Negative Affect"),
  Smith_Routine = c(smith_regret_p, smith_injunctive_p, smith_descriptive_p, smith_negative_p),
  Jones_Exception = c(jones_regret_p, jones_injunctive_p, jones_descriptive_p, jones_negative_p)
)

# Convert the data from wide to long format
data_long <- melt(df, id.vars = "Category", variable.name = "Condition", value.name = "Percentage")

# Reorder the factor levels of Category so that 'Regret' comes first
data_long$Category <- factor(data_long$Category, levels = c("Regret", "Injunctive Social Norm", "Descriptive Social Norm", "Negative Affect"), 
                             labels = c("Regret", "Social Norm\nInjunctive", 
                                        "Social Norm\nDescriptive", "Negative\nAffect"))

# Create the bar plot with the categories reordered
ggplot(data_long, aes(x = Category, y = Percentage, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_manual(values = c("Smith_Routine" = "grey", "Jones_Exception" = "#505050"), 
                    labels = c("Mr.Smith(Routine)", "Mr.Jones(Exception)")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_minimal() +
  labs(x = "", y = "Percentage", fill = "Condition") +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, colour = "black",size = 9), 
        legend.position = "top", legend.title = element_blank())



# experiment2bar

adams_regret_p <- round((mr_adams_counts / total_counts_2) * 100, 1)
adams_luck_p <- round((sum(data_filtered_2$Sc2_lucky == 1) / total_counts_2) * 100, 1)
white_regret_p <- round((sum(data_filtered_2$Sc2_regret == 2) / total_counts_2) * 100, 1)
white_luck_p <- round((sum(data_filtered_2$Sc2_lucky == 2) / total_counts_2) * 100, 1)

# Create a data frame with the provided data
data <- data.frame(
  Category = c("Regret", "Luck"),
  Adams_Routine = c(adams_regret_p, adams_luck_p),
  White_Exception = c(white_regret_p, white_luck_p)
)

# Convert the data from wide to long format
data_long <- melt(data, id.vars = "Category", variable.name = "Condition", value.name = "Percentage")

# Reorder the factor levels of Category so that 'Regret' comes first
data_long$Category <- factor(data_long$Category, levels = c("Regret", "Luck"))

# Create the bar plot with the categories reordered
ggplot(data_long, aes(x = Category, y = Percentage, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_manual(values = c("Adams_Routine" = "grey", "White_Exception" = "#505050"), 
                    labels = c("Mr.Adams(Routine)", "Mr.White(Exception)")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + # label the y-axis every 20%
  theme_minimal() +
  labs(x = "", y = "Percentage", fill = "Condition") +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, colour = "black",size = 9), 
        legend.position = "top", legend.title = element_blank())
