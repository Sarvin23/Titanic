# Read the dataset dataset
dataset <- read.csv("titanic.csv", stringsAsFactors = FALSE)

# Extract title from the Name variable
dataset$Title <- gsub('(.*, )|(\\..*)', '', dataset$Name)
dataset$Title <- gsub('(Mlle|Mme|Ms|Miss|Lady|Countess|Dona)', 'Miss', dataset$Title)
dataset$Title <- gsub('(Mrs)', 'Mrs', dataset$Title)
dataset$Title <- gsub('(Master|Sir|Rev|Major|Col|Capt|Don|Jonkheer|Dr|)', 'Mr', dataset$Title)

# Removing titles from the Name and save back to the variable Name
dataset$Name <- gsub("(Mlle.|Mme.|Ms.|Miss.|Lady.|Countess.|Dona.|Mrs.|Master.|Sir.|Rev.|Major.|Col.|Capt.|Don.|Jonkheer.|Dr.|Mr.)","",dataset$Name)


# Convert variables to factors
dataset$Pclass <- factor(dataset$Pclass, ordered = TRUE)

# Extract information from Cabin variable
dataset$Side <- ifelse(grepl("\\.", dataset$Cabin), ifelse(as.integer(gsub("[^0-9]", "", dataset$Cabin)) %% 2 == 0, "Backbord", "Steuerbord"), NA)
dataset$Deck <- substr(dataset$Cabin, 1, 1)
dataset$Deck[dataset$Deck == ","] <- NA

# Save as preprocessed.R file
write.csv(dataset, "https://github.com/Sarvin23/Titanic/titanic_preprocessed.csv", row.names = FALSE)