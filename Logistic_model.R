require(pracma)

setwd("~/Documents/GitHub/Kaggle-Titanic/")

########## Training Data ##############
train <- read.csv("train.csv", header=TRUE, stringsAsFactors=FALSE)

head(train)
colnames(train)


# keepin' an eye on things...
sapply(train,class)
sapply(train, isempty)

# I want to check to see if there are NA values in any columns.
# Need to write a quick function because I can't get anything else to work in sapply()
nacheck <- function(x)
{
  any(is.na(x))
}
sapply(train, nacheck)  # There are NAs in the Age column

# Find the number of NAs in Age
sum(is.na(train$Age))  # there are 177


# Going to use the average age of each title to fill in the blank Ages
# Need to clean the data first:

temp <- sub("[[:alpha:]]+[[:punct:]][[:space:]]([0-9A-Za-z]+).+",replacement="\\1",train$Name)
table(temp)
temp <- gsub("[[:alpha:]]+[[:punct:]]{1}","",temp)
temp <- sub("[[:alpha:]]+[[:space:]]{1}","",temp)
temp <- sub("[[:alpha:]]{1}[[:space:]]{1}","",temp)
temp <- sub("Mlle", "Miss", temp)
temp <- sub("Mme", "Mrs", temp)
table(temp)

grep("the", temp) #row 760
grep("deMr", temp) #row 171
grep("Ms", temp) #row 444

# Look for actual titles in original dataset
train$Name[grep("the", temp)] # Countess
train$Name[grep("deMr", temp)] # Mr

temp[760] <- "Countess"
temp[171] <- "Mr"
temp[444] <- "Miss"

table(temp)

#Put the titles back into the original data set:
train$title <- temp

# Now find the average of each title:
temp <- data.frame(aggregate(Age ~ title, train, mean))
colnames(temp)[2] <- "avg.age"
merge.df <- merge(temp, train, by='title')

for(i in 1:nrow(merge.df))
{
  if(is.na(merge.df[i,8]==TRUE))
  {
    merge.df[i,8] <- merge.df[i,2]
  }
}


logit.model <- glm(Survived ~ title + Pclass + Sex + Age +SibSp + Parch + Fare + Embarked,
                  data=merge.df, family=gaussian)



######### Test data ###########

test <- read.csv("test.csv", header=TRUE, stringsAsFactors=FALSE)

sapply(test,class)
sapply(test, isempty)
sapply(test, nacheck)  # There are NAs in the Age column

# Find the number of NAs in Age
sum(is.na(test$Age))  # there are 86


# Going through the same process as before...

temp <- sub("[[:alpha:]]+[[:punct:]][[:space:]]([0-9A-Za-z]+).+",replacement="\\1",test$Name)
table(temp)
temp <- gsub("[[:alpha:]]+[[:punct:]]{1}","",temp)
temp <- sub("[[:alpha:]]+[[:space:]]{1}","",temp)
temp <- sub("[[:alpha:]]{1}[[:space:]]{1}","",temp)
table(temp)

grep("Dona",temp) # This does not exist in the training set, need to change -- equiv to Lady
temp[grep("Dona",temp)] <- "Lady"

grep("Ms",temp) 
temp[grep("Ms",temp)] <- "Miss"

#Put the titles back into the original data set:
test$title <- temp

# Now find the average of each title:
temp <- data.frame(aggregate(Age ~ title, test, mean))
colnames(temp)[2] <- "avg.age"
merge.test.df <- merge(temp, test, by='title')


for(i in 1:nrow(merge.test.df))
{
  if(is.na(merge.test.df[i,7]==TRUE))
  {
    merge.test.df[i,7] <- merge.test.df[i,2]
  }
}


# Create the prediction and write to csv for submission

logit.prediction <- predict.glm(logit.model,merge.test.df)
logit.prediction[is.na(logit.prediction)] <- 0   # some may have resulted in an undefined value
logit.prediction[logit.prediction >= .5] <- 1
logit.prediction[logit.prediction < .5] <- 0

logit.submission <- data.frame(PassengerId=merge.test.df$PassengerId, Survived=logit.prediction)
write.csv(logit.submission, file="logit.submission.csv", row.names=FALSE)



