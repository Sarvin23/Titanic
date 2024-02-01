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