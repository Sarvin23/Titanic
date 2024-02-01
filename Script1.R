load("Preprocessed.csv",verbose = TRUE)
summary_stats_continuous <- function(data, var) {
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

summary_stats_continuous(Preprocessed.csv,)