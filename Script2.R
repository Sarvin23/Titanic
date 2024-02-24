#Die Funktion detect_metric_variables dient dazu, Variablen in einem Datenrahmen zu identifizieren,
# die metrische Werte enthalten. Metrische Werte sind numerische Werte, die in der Regel kontinuierlich sind
# und Messungen darstellen, wie z.B. Größen, Gewichte oder Temperatur.
#
#  input: data, Ein Datenrahmen, in dem nach Variablen mit metrischen Werten gesucht werden soll.
#  output : in Vektor mit den Namen der Variablen, die metrische Werte enthalten.


detect_metric_variables <- function(data) {
  # Initialisiere ein leeres Array, um die Variablennamen zu speichern
  metric_vars <- c()

  # Iteriere durch jede Spalte in den Daten
  for (col in names(data)) {
    # Überprüfe, ob die Spalte metrische Werte enthält
    if (is.numeric(data[[col]]) && any(grepl("\\.", as.character(data[[col]])))) {
      metric_vars <- c(metric_vars, col)  # Füge den Variablennamen zum Array hinzu data[[col]]
    }
  }

  # Gib das Array der Variablennamen mit metrischen Werten zurück
  return(metric_vars)
}


# Example usage
data <- read.csv("Preprocessed.csv")  # Replace "your_data.csv" with the actual file name
metric_vars <-  detect_metric_variables(data)
print(metric_vars)


# Funktion zur Erkennung kategorischer Variablen im Datensatz.
# Parameter:
#   - data: Die Daten, in denen kategorische Variablen erkannt werden sollen.
#   - exclude_column: Die Spalte, die ausgeschlossen werden soll. Standardmäßig ist "X" ausgeschlossen.
detect_cat_variables <- function(data, exclude_column = "X") {
  # Initialisiere ein leeres Array zum Speichern der Variablennamen
  cat_vars <- c()

  # Iteriere durch jede Spalte in den Daten
  for (col in names(data)) {
    # Überspringe die exclude_column
    if (col == exclude_column) {
      next
    }

    # Überprüfe, ob die Spalte metrische Werte enthält
    if (!(is.numeric(data[[col]]) && any(grepl("\\.", as.character(data[[col]]))))) {
      cat_vars <- c(cat_vars, col)  # Füge den Variablennamen zum Array hinzu data[[col]]
    }
  }

  # Gib das Array der Variablennamen mit kategorischen Werten zurück
  return(cat_vars)
}


# Example usage
data <- read.csv("Preprocessed.csv")
cat_vars <-  detect_cat_variables(data)
print(cat_vars)


# Funktion zur Erkennung dichotomer Variablen im Datensatz.
# Parameter:
#   - data: Die Daten, in denen dichotome Variablen erkannt werden sollen.
detect_dichotomous_variables <- function(data) {
  dichotomous_vars <- character(0)  # Initialisiere ein leeres Array zum Speichern der dichotomen Variablennamen.

  for (col in names(data)) {  # Iteriere über jede Spalte im Datensatz.
    unique_values <- unique(data[[col]])  # Bestimme die eindeutigen Werte in der aktuellen Spalte.

    # Überprüfe, ob die Spalte genau zwei eindeutige Werte enthält, die 0 und 1 sind.
    if (length(unique_values) == 2 && all(unique_values %in% c(0, 1))) {
      dichotomous_vars <- c(dichotomous_vars, col)  # Füge den Namen der dichotomen Variablen zum Array hinzu.
    }
  }

  return(dichotomous_vars)  # Gib das Array der dichotomen Variablennamen zurück.
}

data <- read.csv("Preprocessed.csv")
dic_vars <-  detect_dichotomous_variables(data)
print(dic_vars)



# Funktion zur Erstellung von Kombinationen kategorischer Variablen im Datensatz.
# Parameter:
#   - data: Die Daten, aus denen Kombinationen kategorischer Variablen erstellt werden sollen.
#   - exclude_column: Die Spalte, die ausgeschlossen werden soll. Standardmäßig ist "X" ausgeschlossen.
detect_cat_variable_combinations <- function(data, exclude_column = "X") {
  # Initialisiere eine leere Liste zum Speichern von Kombinationen
  cat_var_combinations <- list()

  # Erhalte kategorische Variablen
  cat_vars <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]

  # Generiere Kombinationen von 2 kategorischen Variablen
  combinations <- combn(cat_vars, 2)

  # Iteriere durch jede Kombination
  for (i in 1:ncol(combinations)) {
    cat_var_combinations[[i]] <- combinations[, i]
  }

  return(cat_var_combinations)
}



# Interne Funktion zum Entfernen fehlender Werte aus einem Vektor
# Entferne fehlende Werte aus einem Vektor
# Diese Funktion entfernt fehlende Werte aus einem Vektor.
# Eingabe: x Ein Vektor
# Ausgabe: Ein Vektor ohne fehlende Werte

remove_missing_values <- function(x) {
  return(x[!is.na(x)])
}





