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
data <- read.csv("Preprocessed.csv")
cat_vars <-  detect_cat_variables(data)
print(cat_vars)



detect_cat_variable_combinations <- function(data, exclude_column = "X") {
  # Initialize an empty list to store combinations
  cat_var_combinations <- list()

  # Get categorical variables
  cat_vars <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]

  # Generate combinations of 2 categorical variables
  combinations <- combn(cat_vars, 2)

  # Loop through each combination
  for (i in 1:ncol(combinations)) {
    cat_var_combinations[[i]] <- combinations[, i]
  }

  return(cat_var_combinations)
}





# Funktion zur Berechnung geeigneter deskriptiver bivariater Statistiken 
# fuer den Zusammenhang zwischen einer metrischen und einer dichotomen Variablen
# Berechnet und gibt geeignete deskriptive bivariate Statistiken fuer den 
# Zusammenhang zwischen einer metrischen und einer dichotomen Variablen aus

# Berechnet geeignete deskriptive bivariate Statistiken fuer den Zusammenhang 
# zwischen einer metrischen und einer dichotomen Variablen.

# input: metric_var Die metrische Variable.
#        dichotomous_var Die dichotome Variable.
# otput:Eine Zusammenfassung der berechneten deskriptiven bivariaten Statistiken

calculate_bivariate_stats_metric_dichotomous <- function(metric_var, 
                                                         dichotomous_var) {
  # Mittelwert und Standardabweichung fuer jede Kategorie der dichotomen
  # Variable berechnen.
  bivariate_stats <- tapply(metric_var, dichotomous_var, 
                            function(x) c(Mittelwert = mean(x), SD = sd(x)))
  
  # Zusammenfassung der Ergebnisse
  return(bivariate_stats)
}




# Funktion zur Erstellung einer geeigneten Visualisierung von drei oder
# vier kategorialen Variablen
# Erstellt und gibt eine geeignete Visualisierung von drei oder vier 
# kategorialen Variablen aus

# Erstellt eine geeignete Visualisierung von drei oder vier 
# kategorialen Variablen.

# input: var1 Die erste kategoriale Variable.
#        var2 Die zweite kategoriale Variable.
#        var3 Die dritte kategoriale Variable.
#        var4 (Optional) Die vierte kategoriale Variable.
# output: Eine visuelle Darstellung der kategorialen Variablen

visualize_categorical_variables <- function(var1, var2, var3, var4 = NULL) {
  if(is.null(var4)) {
    mosaicplot(table(var1, var2, var3), main = "Mosaikdiagramm")
  } else {
    mosaicplot(table(var1, var2, var3, var4), main = "Mosaikdiagramm")
  }
}





# Internal function to remove missing values from a vector
# Remove missing values from a vector
# This function removes missing values from a vector.
# input: x A vector
# output: A vector without missing values

remove_missing_values <- function(x) {
  return(x[!is.na(x)])
}

