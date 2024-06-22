install.packages("tidyverse")
install.packages('ggplot2')

# Load necessary libraries
library(dplyr)
library(ggplot2)



# Load the dataset
data <- read.csv(('C:/Users/anjal/Downloads/diabetes.csv'))

View((data))

# Print the structure of your dataset
print("Structure of the dataset:")
str(data)

# List the variables in your dataset
print("Variables in the dataset:")
print(colnames(data))

# Print the top 15 rows of your dataset
print("Top 15 rows of the dataset:")
print(head(data, 15))

# User-defined function using the 'BMI' variable
calculate_bmi_ratio <- function(BMI) {
  average_bmi <- mean(BMI, na.rm = TRUE)  # Calculate average BMI
  return(BMI / average_bmi)  # Return the ratio of BMI to average BMI
}

# Apply the function to the 'BMI' variable in the dataset and print the results
print("BMI ratio compared to the average BMI:")
bmi_ratio <- calculate_bmi_ratio(data$BMI)
print(bmi_ratio)



# Calculate the average BMI
average_bmi <- mean(data$BMI, na.rm = TRUE)

# Filter rows where BMI is greater than the average BMI
filtered_data <- data %>% filter(BMI > average_bmi)

# Display the filtered data
print("Rows where BMI is greater than the average BMI:")
print(filtered_data)





# Extracting the relevant variables
dependent_var <- data$Outcome
independent_vars <- data[, c("Pregnancies", "Glucose", "BloodPressure", 
                             "SkinThickness", "Insulin", "BMI", 
                             "DiabetesPedigreeFunction", "Age")]

# Creating a new data frame by joining dependent and independent variables
new_data <- cbind(dependent_var, independent_vars)

# Printing the first few rows of the new data frame
head(new_data)





data[data == 0] <- NA

# Remove rows with any NA values
cleaned_data <- na.omit(data)

# Check the dimensions of the cleaned data
dim(cleaned_data)
dim(data)



duplicated_rows <- duplicated(data)

# View which rows are duplicated
duplicated_indices <- which(duplicated_rows)

# Remove duplicates and create a new cleaned dataset
cleaned_data <- unique(data)







# Convert Glucose column to numeric
data$Glucose <- as.numeric(data$Glucose)

# Reorder rows based on Glucose in descending order
ordered_data <- data[order(-data$Glucose), ]

# Print reordered dataset
print(ordered_data)



colnames(data) <- c("Pregnancies", "GlucoseLevel", "BloodPressure", "SkinThickness", 
                    "InsulinLevel", "BMI", "DiabetesPedigree", "Age", "DiabetesOutcome")

# Print the updated column names
print(colnames(data))




# Example: Multiply 'GlucoseLevel' by 2 and add as a new variable 'DoubleGlucose'
data <- data %>%
  mutate(DoubleGlucose = GlucoseLevel * 2)

# View the updated data frame
head(data)





# Number of rows in your dataset
num_rows <- nrow(data)

# Number of rows for training set (e.g., 80% of the data)
train_size <- 0.8 * num_rows

# Generate indices for training set
train_indices <- sample(num_rows, train_size, replace = FALSE)

# Create training set
train_data <- data[train_indices, ]

# View the first few rows of the training set
head(train_data)





# Print summary statistics of the dataset
summary(data)





# Remove rows with NA values in GlucoseLevel
data <- na.omit(data)

# Mean calculation
mean_glucose <- mean(data$GlucoseLevel, na.rm = TRUE)

# Median calculation
median_glucose <- median(data$GlucoseLevel, na.rm = TRUE)

# Mode calculation (custom function)
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_glucose <- get_mode(data$GlucoseLevel)

# Range calculation
range_glucose <- range(data$GlucoseLevel)

# Print the results
cat("Mean Glucose Level:", mean_glucose, "\n")
cat("Median Glucose Level:", median_glucose, "\n")
cat("Mode Glucose Level:", mode_glucose, "\n")
cat("Range of Glucose Level:", range_glucose[1], "to", range_glucose[2], "\n")



# Scatter plot using ggplot2
ggplot(data, aes(x = GlucoseLevel, y = BMI)) +
  geom_point() +  # Scatter points
  labs(x = "Glucose Level", y = "BMI", title = "Scatter Plot of Glucose Level vs. BMI") +
  theme_minimal()







# Create a bar plot for GlucoseLevel and BloodPressure
ggplot(data, aes(x = GlucoseLevel, y = BloodPressure)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +  # stat = "identity" for raw values
  labs(x = "Glucose Level", y = "Blood Pressure", title = "Bar Plot of Glucose Level vs Blood Pressure") +
  theme_minimal()





# Calculate Pearson correlation between GlucoseLevel and BloodPressure
correlation <- cor(data$GlucoseLevel, data$BloodPressure, method = "pearson")

# Print the correlation coefficient
cat("Pearson correlation coefficient between GlucoseLevel and BloodPressure:", correlation, "\n")