# Die summary_stats_continuous Funktion berechnet Zusammenfassungsstatistiken für kontinuierliche Variablen in einem Datensatz.
  #
  # Input:
  #   data: Ein Datenrahmen oder eine Datenmatrix, der/die die zu analysierenden Variablen enthält.
  #
  # Output:
  #   Diese Funktion gibt Zusammenfassungsstatistiken für kontinuierliche Variablen im Datenrahmen aus.
  #   Für jede kontinuierliche Variable werden der Mittelwert, Median, Standardabweichung, Interquartilsbereich,
  #   Minimum und Maximum berechnet und in einer Tabelle zusammengefasst.
  #   Wenn eine Variable nur fehlende Werte enthält, wird eine Meldung ausgegeben, dass alle Werte fehlen.


summary_stats_continuous <- function(data) {
  metric_vars <- detect_metric_variables(data)

  for (var in metric_vars) {

    # Überprüfe, ob nicht fehlende Werte in der Variable vorhanden sind
    if (all(is.na(data[[var]]))) {
      cat("Alle Werte in der Variable fehlen:", var, "\n")
      next
    }

    # Berechne Zusammenfassungsstatistiken für eine kontinuierliche Variable
    mean_var <- mean(data[[var]], na.rm = TRUE)
    median_var <- median(data[[var]], na.rm = TRUE)
    sd_var <- sd(data[[var]], na.rm = TRUE)
    iqr_var <- IQR(data[[var]], na.rm = TRUE)
    min_var <- min(data[[var]], na.rm = TRUE)
    max_var <- max(data[[var]], na.rm = TRUE)

    # Erstelle eine Zusammenfassungstabelle
    summary_table <- data.frame(
      "Mittelwert" = mean_var,
      "Median" = median_var,
      "Standardabweichung" = sd_var,
      "Interquartilsbereich" = iqr_var,
      "Minimum" = min_var,
      "Maximum" = max_var
    )

    # Gib die Zusammenfassungstabelle aus
    cat("\nZusammenfassungsstatistiken für", var, "\n")
    print(summary_table)
  }
}


data<-read.csv("Preprocessed.csv")
summary_stats_continuous(data)

# Diese Funktion berechnet Zusammenfassungsstatistiken für kategoriale Variablen in einem Datenrahmen.
  #
  # Argumente:
  #   data: Ein Datenrahmen, der die Daten enthält.
  #
  # Rückgabe:
  #   Diese Funktion gibt keine explizite Rückgabe aus, sondern druckt Zusammenfassungsstatistiken für jede kategoriale Variable im Datenrahmen.
  #

summary_stats_categorical <- function(data) {
  # Kategoriale Variablen im Datenrahmen identifizieren
  cat_vars <- detect_cat_variables(data)

    # Für jede kategoriale Variable Zusammenfassungsstatistiken berechnen
    for (var in cat_vars) {
      # Überprüfen, ob alle Werte der Variablen fehlend sind
      if (all(is.na(data[[var]]))) {
      cat("All values in the variable", var,"are not available", "\n")
      next
      }
    # Häufigkeiten und Anteile berechnen
    counts <- table(data[[var]], useNA = "ifany")
    prop <- prop.table(counts)

    # Zusammenfassungstabelle erstellen
    summary_table <- data.frame(
      "Counts" = as.numeric(counts),
      "Proportions" = prop
    )

    # Zusammenfassungsstatistiken drucken
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
    "Fisher's Exact Test" = fisher_test_result
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


# Funktion zur Berechnung geeigneter deskriptiver bivariater Statistiken
# fuer den Zusammenhang zwischen einer metrischen und einer dichotomen Variablen


# Berechnet geeignete deskriptive bivariate Statistiken fuer den Zusammenhang
# zwischen einer metrischen und einer dichotomen Variablen.

# input: metric_var Die metrische Variable.
#        dichotomous_var Die dichotome Variable.
#        data Die Dataset
# output:Eine Zusammenfassung der berechneten deskriptiven bivariaten Statistiken

calculate_bivariate_stats_correlation <- function(metric_var, dichotomous_var, data) {
  # Überprüfen, ob Nicht-Missing-Werte in den Variablen vorhanden sind
  if (any(is.na(data[[metric_var]])) || any(is.na(data[[dichotomous_var]]))) {
    cat("Missing values present in the variables. Unable to calculate correlation.\n")
    return(NULL)
  }

  # Dichotome Variable in numerischen Wert umwandeln
  dichotomous_numeric <- as.numeric(data[[dichotomous_var]])

  # Korrelation berechnen
  correlation <- cor(data[[metric_var]], dichotomous_numeric, method = "pearson")

  # Zusammenfassungstabelle erstellen
  summary_table <- data.frame(
    "Correlation" = correlation
  )

  # Zusammenfassungstabelle drucken
  cat("\nBivariate Correlation Stats for", metric_var, "and", dichotomous_var, "\n")
  print(summary_table)
}
calculate_bivariate_stats_correlation("Fare", "Survived", data)
calculate_bivariate_stats_correlation("Age", "Survived", data)


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