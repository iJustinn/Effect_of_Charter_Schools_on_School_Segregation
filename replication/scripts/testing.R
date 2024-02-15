# Load the dataset into R
file_path <- "inputs/data/raw_data.csv"
data <- read_csv(file_path, show_col_types = FALSE)

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