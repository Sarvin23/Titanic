# Function to detect variables with metric values
detect_metric_variables <- function(data) {
  # Initialize an empty array to store the variable names
  metric_vars <- c()

  # Loop through each column in the data
  for (col in names(data)) {
    # Check if the column contains metric values
    if (is.numeric(data[[col]]) && any(grepl("\\.", as.character(data[[col]])))) {
      metric_vars <- c(metric_vars, col)  # Add the variable name to the array data[[col]]
    }
  }

  # Return the array of variable names with metric values
  return(metric_vars)
}

# Example usage
data <- read.csv("Preprocessed.csv")  # Replace "your_data.csv" with the actual file name
metric_vars <-  detect_metric_variables(data)
print(metric_vars)


detect_cat_variables <- function(data, exclude_column = "X") {
  # Initialize an empty array to store the variable names
  cat_vars <- c()

  # Loop through each column in the data
  for (col in names(data)) {
    # Skip the exclude_column
    if (col == exclude_column) {
      next
    }

    # Check if the column contains metric values
    if (!(is.numeric(data[[col]]) && any(grepl("\\.", as.character(data[[col]]))))) {
      cat_vars <- c(cat_vars, col)  # Add the variable name to the array data[[col]]
    }
  }


  # Return the array of variable names with metric values
  return(cat_vars)
}

# Example usage
data <- read.csv("Preprocessed.csv")  # Replace "your_data.csv" with the actual file name
cat_vars <-  detect_cat_variables(data)
print(cat_vars)


# Internal function to remove missing values from a vector
# Remove missing values from a vector

# This function removes missing values from a vector.

# input: x A vector
# output: A vector without missing values


remove_missing_values <- function(x) {
  return(x[!is.na(x)])
}

