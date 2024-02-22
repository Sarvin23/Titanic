summary_stats_continuous <- function(data) {
  metric_vars <- detect_metric_variables(data)

  for (var in metric_vars) {

    # Check if there are non-missing values in the variable
    if (all(is.na(data[[var]]))) {
      cat("All values in the variable are missing:", var, "\n")
      next
    }

    # calculate summary statistics for a continuous variable
    mean_var <- mean(data[[var]], na.rm = TRUE)
    median_var <- median(data[[var]], na.rm = TRUE)
    sd_var <- sd(data[[var]], na.rm = TRUE)
    iqr_var <- IQR(data[[var]], na.rm = TRUE)
    min_var <- min(data[[var]], na.rm = TRUE)
    max_var <- max(data[[var]], na.rm = TRUE)

    # create a summary table
    summary_table <- data.frame(
      "Mean" = mean_var,
      "Median" = median_var,
      "Standard Deviation" = sd_var,
      "Interquartile Range" = iqr_var,
      "Minimum" = min_var,
      "Maximum" = max_var
    )

    # print the summary table
    cat("\nSummary Statistics for", var, "\n")
    print(summary_table)
  }
}

data<-read.csv("Preprocessed.csv")
summary_stats_continuous(data)

summary_stats_categorical <- function(data) {
  cat_vars <- detect_cat_variables(data)

    for (var in cat_vars) {
      if (all(is.na(data[[var]]))) {
      cat("All values in the variable", var,"are not available", "\n")
      next
      }
    # calculate summary statistics for a categorical variable
    counts <- table(data[[var]], useNA = "ifany")
    prop <- prop.table(counts)

    # create a summary table without Proportions.Var1
    summary_table <- data.frame(
      "Counts" = as.numeric(counts),
      "Proportions" = prop
    )

    # print the summary table
    cat("\nSummary Statistics for", var, "\n")
    print(summary_table)
  }
}

data<-read.csv("Preprocessed.csv")
summary_stats_categorical(data)

# Funktion zur Berechnung geeigneter deskriptiver bivariater Statistiken für
# den Zusammenhang zwischen zwei kategorialen Variablen.
# Berechnet und gibt geeignete deskriptive bivariate Statistiken
# fuer den Zusammenhang zwischen zwei kategorialen Variablen aus.

# Berechnet geeignete deskriptive bivariate Statistiken für den Zusammenhang
# zwischen zwei kategorialen Variablen.

# input: var1 Die erste kategoriale Variable.
#        var2 Die zweite kategoriale Variable.
# output: Eine Zusammenfassung der berechneten deskriptiven bivariaten
#         Statistiken.

calculate_bivariate_stats_categorical <- function(var1, var2) {
  # Create contingency table
  contingency_table <- table(var1, var2)

  # Fisher's Exact Test
  fisher_test_result <- fisher.test(contingency_table,simulate.p.value=TRUE)

  # Summary of results
  result_summary <- list(
    "Fisher's Exact Test" = fisher_test_result,
    "Cramer's V" = cramer_v
  )

  return(result_summary)
}

data<-read.csv("Preprocessed.csv")
variable_combinations <- detect_cat_variable_combinations(data)
# Initialize an empty list
results_list <- list()

# Iterate over each combination
for (comb in variable_combinations) {
  # Calculate bivariate statistics for the combination
  result <- calculate_bivariate_stats_categorical(data[[comb[1]]], data[[comb[2]]])

  # Extract relevant information from the result
  chi_sq_test <- result$"Fisher's Exact Test"
  p_value <- chi_sq_test$p.value
  cramer_v <- result$"Cramer's V"

  # Store the results in a data frame
  result_df <- data.frame(
    Variable1 = comb[1],
    Variable2 = comb[2],
    P_Value = p_value
  )

  # Append the data frame to the list
  results_list[[length(results_list) + 1]] <- result_df
}

# Combine all data frames into a single data frame
result_table <- do.call(rbind, results_list)

# Print or return the result table
print(result_table)

