# Read the dataset dataset
dataset <- read.csv("titanic.csv", stringsAsFactors = FALSE)

# Extract title from the Name variable.
# In total there will be only 4 Titles.
dataset$Title <- gsub('(.*, )|(\\..*)', '', dataset$Name)
# Update different meaning of unmarried women to Miss
dataset$Title <- gsub('(Mlle|Mme|Ms|Miss|Lady|the Countess|Dona)', 'Miss', dataset$Title)
dataset$Title <- gsub('(Mrs)', 'Mrs', dataset$Title)
dataset$Title <- gsub('(Sir|Rev|Major|Col|Capt|Don|Jonkheer|Dr|Master)', 'Mr', dataset$Title)

# Factorize the variables of "Survived", "Sex" and "Embarked" since all has maximum of 3 different values
dataset$Survived<- factor(dataset$Survived)
dataset$Sex<- factor(dataset$Sex)
dataset$Embarked<- factor(dataset$Embarked)

# Impute the age of N.A values in Age variable using median age of each title group.
dataset$Age<- ave(dataset$Age, dataset$Title, FUN = function (x) replace(x, is.na(x), median(x, na.rm = TRUE)))



# Convert variables to factors
dataset$Pclass <- factor(dataset$Pclass, ordered = TRUE)

# Extract information from Cabin variable
dataset$Side <- ifelse(grepl("\\.", dataset$Cabin), ifelse(as.integer(gsub("[^0-9]", "", dataset$Cabin)) %% 2 == 0, "Backbord", "Steuerbord"), NA)
dataset$Deck <- substr(dataset$Cabin, 1, 1)
dataset$Deck[dataset$Deck == ","] <- NA

# Remove the Variables "PassengerId", "Name", "Ticket" and "Cabin"
del<-c("PassengerId", "Name", "Ticket", "Cabin")
dataset=dataset[!(names(dataset) %in% del)]

# Save as preprocessed.csv file
write.csv(dataset,file = "Preprocessed.csv")