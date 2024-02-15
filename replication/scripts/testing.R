# Load the dataset into R
file_path <- "../../inputs/data/raw_data.csv"
my_data <- read_csv(file_path, show_col_types = FALSE)

# Check if 'age' contains only integer values
is_age_integer <- all(my_data$age == floor(my_data$age))

# Check if 'gender' contains only the values 1 and 2
valid_genders <- unique(my_data$gender)
is_gender_valid <- all(valid_genders %in% c(1, 2))

# Output the results
print(paste("Age column contains only integers:", is_age_integer))
print(paste("Gender column contains only 1 and 2:", is_gender_valid))

# Optionally, identify rows with problematic 'age' data
if(!is_age_integer) {
  problematic_age <- my_data[my_data$age != floor(my_data$age), ]
  print("Rows with problematic 'age' data:")
  print(problematic_age)
}

# Optionally, identify rows with problematic 'gender' data
if(!is_gender_valid) {
  problematic_gender <- my_data[!my_data$gender %in% c(1, 2), ]
  print("Rows with problematic 'gender' data:")
  print(problematic_gender)
}
