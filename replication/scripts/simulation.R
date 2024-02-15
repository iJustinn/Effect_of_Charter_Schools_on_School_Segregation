library(tidyverse)
library(ggplot2)
library(janitor)
library(dplyr)
library(readr)
library(knitr)
library(kableExtra)
library(reshape2)

# experiment 1 simulation table
data <- data.frame(
  Category = c("Regret", "Negative_Affect"),
  Smith_Routine = c(10, 8),
  Jones_Exception = c(90, 92)
)

kable(data, caption = "Descriptive Statistics", align = c('l', 'c', 'c'))

# experiment 1 simulation bar plot

# Convert the data from wide to long format
data_long <- melt(data, id.vars = "Category", variable.name = "Condition", value.name = "Percentage")

# Create the bar plot
ggplot(data_long, aes(x = Category, y = Percentage, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("Smith_Routine" = "blue", "Jones_Exception" = "red")) +
  theme_minimal() +
  labs(x = "", y = "Percentage", fill = "Condition") +
  theme(legend.position = "bottom")

# experiment 2 simulation table
data_2 <- data.frame(
  Category = c("Regret", "Luck"),
  Adams_Routine = c(12, 30),
  White_Exception = c(88, 70)
)

kable(data_2, caption = "Descriptive Statistics", align = c('l', 'c', 'c'))

# experiment 2 simulation bar plot
# Convert the data from wide to long format
data_long <- melt(data_2, id.vars = "Category", variable.name = "Condition", value.name = "Percentage")

# Create the bar plot
ggplot(data_long, aes(x = Category, y = Percentage, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("Adams_Routine" = "blue", "White_Exception" = "red")) +
  theme_minimal() +
  labs(x = "", y = "Percentage", fill = "Condition") +
  theme(legend.position = "bottom")