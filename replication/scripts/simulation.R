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
  Category = c("Mr.Smith", "Mr.Jones"),
  Regret = c(10, 90),
  Luck = c(8, 92)
)

kable(data, caption = "Descriptive Statistics", align = c('l', 'c', 'c'))

# experiment 1 simulation bar plot

# Convert data from wide to long format for ggplot2
data_long <- reshape2::melt(data, id.vars = "Category", variable.name = "Outcome", value.name = "Percentage")

# Create the bar plot
ggplot(data_long, aes(x = Category, y = Percentage, fill = Outcome)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  labs(x = "Group", y = "Percentage", fill = "Outcome") +
  theme(legend.position = "bottom")

# experiment 2 simulation table
data_2 <- data.frame(
  Category = c("Mr.Adams", "Mr.White"),
  Regret = c(12, 88),
  Luck = c(30, 70)
)

kable(data_2, caption = "Descriptive Statistics", align = c('l', 'c', 'c'))

# experiment 2 simulation bar plot
# Convert data from wide to long format for ggplot2
data_long <- reshape2::melt(data_2, id.vars = "Category", variable.name = "Outcome", value.name = "Percentage")

# Create the bar plot
ggplot(data_long, aes(x = Category, y = Percentage, fill = Outcome)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  labs(x = "Group", y = "Percentage", fill = "Outcome") +
  theme(legend.position = "bottom")

# testing
# Check if 'regret' and 'Negative Affect'  contains only numeric values
is_integer_1 <- all(sapply(data$Smith_Routine, is.numeric))
is_integer_2 <- all(sapply(data$Jones_Exception, is.numeric))

is_integer

# check if there only two persons
nrow(data) == 2
nrow(data_2) == 2

# test the percentages are add up to 100
sum(data$Regret) == 100
sum(data$Luck) == 100

# simulation of dataset
set.seed(123) # Set a seed for reproducibility
num_rows <- 10

# Create a data frame with specified columns and random values of 1 or 2
df <- data.frame(
  "Experiment 1 regret" = sample(1:2, num_rows, replace = TRUE),
  "Injunctive norms" = sample(1:2, num_rows, replace = TRUE),
  "Descriptive norms" = sample(1:2, num_rows, replace = TRUE),
  "Negative effect" = sample(1:2, num_rows, replace = TRUE),
  "Experiment 2 regret" = sample(1:2, num_rows, replace = TRUE),
  "Luck" = sample(1:2, num_rows, replace = TRUE)
)

df

# Check if selected columns contain only the values 1 and 2
selected_data <- data %>%
  select(Sc1_regret, sc1_socnorms1, sc1_socnorms2, sc1_combinednorms, Sc2_regret, Sc2_lucky) %>%
  rename(
    "Experiment 1 regret" = Sc1_regret,
    "Injuctive norms" = sc1_socnorms1,
    "Descriptive norms" = sc1_socnorms2,
    "Negative effect" = sc1_combinednorms,
    "Experiment 2 regret" = Sc2_regret,
    "Luck" = Sc2_lucky
  )

selected_data <- na.omit(selected_data)

columns_to_check <- c("Experiment 1 regret", "Injuctive norms", "Descriptive norms", "Negative effect", "Experiment 2 regret", "Luck")

results <- sapply(selected_data[columns_to_check], function(x) all(x %in% c(1, 2)))
print(results)

# Check if selected columns contains only numeric values
# Define the columns to check
columns_to_check <- c("Experiment 1 regret", "Injuctive norms", "Descriptive norms", "Negative effect", "Experiment 2 regret", "Luck")

# Check if the specified columns contain only numeric values
are_columns_numeric <- sapply(selected_data[columns_to_check], is.numeric)

# Print the results
print(are_columns_numeric)

# check if there 342 participants
nrow(selected_data) == 342

# check there are in total of 6 variables
ncol(selected_data) == 6

