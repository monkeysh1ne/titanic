# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.survived)


# Need to clean up column names so all uniformly lowercase.
# install and load Janitor package
install.packages("janitor")
library(janitor)

# As Janitor breaks if run on existing df, copy df to be cleaned
# to NEW (temporary) df with clean_names func.  The default clean_names() arg
# is snake case
mydata <- janitor::clean_names(data.combined)

# Remove old table
rm(data.combined)

# Create new table and assign contents of mydata to it...
data.combined <- mydata

# ...then nuke temp table
rm(mydata)


# A bit about R data types (e.g., factors)
str(data.combined)

# Machine learning algorithms do not like CHR or STRING
data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)



# Take a look at survival rates
table(data.combined$survived)


# Distribution across classes
table(data.combined$pclass)


# Load up ggplot2 package to use for visualisations
library(ggplot2)

# Tidy up train df so all column names lcase
mydata <- janitor::clean_names(train)
rm(train)
train <- mydata
rm(mydata)



# Hypothesis - Rich folks survived at a higher rate
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived)), position = position_stack(reverse = FALSE)) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


# Examine the first few names in the training data set
head(as.character(train$name))


# How many unique names are there across both train & test?
length(unique(as.character(data.combined$name)))


# Two duplicate names, take a closer look (i.e., 1309 objs in data.combined
# but length returned 1307 objs - why?)
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])


# Next, take a look at the records in the combined data set
data.combined[which(data.combined$name %in% dup.names),]


# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

# Any correlation with other variables (e.g., sib_sp)?
# Grab every single name in the combined dataset WHERE 'Miss.' is in the name.
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")),]

# Show first 5 rows and *default* all of the columns
misses[1:5,]

# Noteworthy of results returned - 4 out of 5 results survived=1, also 4 of them were in 3rd class (i.e, pclass=3)



# Hypothesis - name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")),]
mrses[1:5,]


# Check out males to see if pattern continues
males <- data.combined[which(train$sex == "male"),]
males[1:5,]



# Expand upon the relationship between "survived" ansd "pclass" by adding the new 'Title' variable
# to the data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with Title extraction
extractTitle<- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

# Expand upon the relationship between "survived" ansd "pclass" by adding the new 'Title' variable
# to the data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with Title extraction
extractTitle<- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL # ensure var value initialised as NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles) # add 'titles' as a factor var

# Since we only have survived labels for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

