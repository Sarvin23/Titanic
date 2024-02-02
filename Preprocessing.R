# Function to read the dataset
read_dataset <- function(file_path) {
  return(read.csv(file_path, stringsAsFactors = FALSE))
}

# Function to extract titles from the Name variable
extract_titles <- function(dataset) {
  dataset$Title <- gsub('(.*, )|(\\..*)', '', dataset$Name)
  dataset$Title <- gsub('(Mlle|Mme|Ms|Miss|Lady|the Countess|Dona)', 'Miss', dataset$Title)
  dataset$Title <- gsub('(Mrs)', 'Mrs', dataset$Title)
  dataset$Title <- gsub('(Sir|Rev|Major|Col|Capt|Don|Jonkheer|Dr|Master)', 'Mr', dataset$Title)
  return(dataset)
}

# Function to factorize specified variables
factorize_variables <- function(dataset) {
  dataset$Survived <- factor(dataset$Survived)
  dataset$Sex <- factor(dataset$Sex)
  dataset$Embarked <- factor(dataset$Embarked)
  dataset$Pclass <- factor(dataset$Pclass, ordered = TRUE)
  return(dataset)
}

# Function to impute age based on title groups
impute_age <- function(dataset) {
  dataset$Age <- ave(dataset$Age, dataset$Title, FUN = function(x) replace(x, is.na(x), median(x, na.rm = TRUE)))
  return(dataset)
}

# Function to extract information from Cabin variable
extract_cabin_info <- function(dataset) {
  dataset$Side <- ifelse(grepl("\\.", dataset$Cabin), ifelse(as.integer(gsub("[^0-9]", "", dataset$Cabin)) %% 2 == 0, "Backbord", "Steuerbord"), NA)
  dataset$Deck <- substr(dataset$Cabin, 1, 1)
  dataset$Deck[dataset$Deck == ","] <- NA
  return(dataset)
}

# Function to remove specified variables
remove_variables <- function(dataset, variables_to_remove) {
  dataset <- dataset[!(names(dataset) %in% variables_to_remove)]
  return(dataset)
}

# Function to save the preprocessed dataset
save_preprocessed <- function(dataset, output_file) {
  write.csv(dataset, file = output_file)
}


file_path <- "titanic.csv"
output_file <- "Preprocessed.csv"

dataset <- read_dataset(file_path)
dataset <- extract_titles(dataset)
dataset <- factorize_variables(dataset)
dataset <- impute_age(dataset)
dataset <- extract_cabin_info(dataset)
variables_to_remove <- c("PassengerId", "Name", "Ticket", "Cabin")
dataset <- remove_variables(dataset, variables_to_remove)
save_preprocessed(dataset, output_file)
