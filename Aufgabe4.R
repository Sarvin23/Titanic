# Laden des Datensatzes
titanic_data <- read.csv("Preprocessed.csv")


# Deskriptive Statistiken fuer kontinuierliche Variablen berechnen
# und visualisieren
summary_stats_continuous(titanic_data)

# Deskriptive Statistiken fuer kategoriale Variablen berechnen und visualisieren
summary_stats_categorical(titanic_data)

# Bestimmung der dichotomen Variablen fuer den Zusammenhang mit 
# der Ueberlebensrate
dichotomous_vars <- c("Sex", "Pclass", "Embarked")

# Bivariate deskriptive Statistiken fuer den Zusammenhang zwischen 
# Ueberlebensrate und dichotomen Variablen berechnen und visualisieren
for (var in dichotomous_vars) {
  calculate_bivariate_stats_correlation("Survived", var, titanic_data)
}

# Untersuchung des Zusammenhangs zwischen Ticketpreis und Ueberlebensrate
calculate_bivariate_stats_correlation("Fare", "Survived", titanic_data)

# Visualisierung kategorialer Variablen
visualize_categorical_variables(titanic_data$Sex, titanic_data$Pclass
                                titanic_data$Embarked, titanic_data$Survived)

# Visualisierung des Zusammenhangs zwischen kontinuierlichen Variablen und
# Ueberlebensrate
plot(titanic_data$Age, titanic_data$Survived,
     main = "Ueberlebensrate in Abhaengigkeit vom Alter", xlab = "Alter",
     ylab = "Ueberlebt (1) oder nicht (0)",
     col = ifelse(titanic_data$Survived == 1, "blue", "red"))
plot(titanic_data$Fare, titanic_data$Survived,
     main = "Ueberlebensrate in Abhaengigkeit vom Ticketpreis", 
     xlab = "Ticketpreis", ylab = "Ueberlebt (1) oder nicht (0)", 
     col = ifelse(titanic_data$Survived == 1, "blue", "red"))

# Ueberlebensrate nach Geschlecht und Klasse visualisieren
barplot(table(titanic_data$Survived, titanic_data$Sex, titanic_data$Pclass),
        beside = TRUE,
        legend.text = TRUE,
        main = "Ueberlebensrate nach Geschlecht und Klasse",
        xlab = "Ueberlebt (1) oder nicht (0)",
        ylab = "Anzahl der Passagiere")

# Altersverteilung der Passagiere visualisieren
hist(titanic_data$Age,
     main = "Altersverteilung der Passagiere",
     xlab = "Alter",
     ylab = "Anzahl der Passagiere",
     col = "lightblue")

# Ueberlebensrate nach Zustiegshafen visualisieren
barplot(table(titanic_data$Survived, titanic_data$Embarked),
        beside = TRUE,
        legend.text = TRUE,
        main = "Ueberlebensrate nach Zustiegshafen",
        xlab = "Ueberlebt (1) oder nicht (0)",
        ylab = "Anzahl der Passagiere")