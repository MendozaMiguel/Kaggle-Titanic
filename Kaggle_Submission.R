# Required packages
require(randomForest)

#Load training/test set
setwd("/Users/michaelpiccirilli/Documents/GitHub/Kaggle-Titanic")
train <- read.csv("train.csv",header=T,sep=",", stringsAsFactors = FALSE)
test <- read.csv("test.csv",header=T,sep=",", stringsAsFactors = FALSE)
sapply(train,class)


# Index the Sex
train$sex.index <- as.numeric(as.factor(train$Sex))
test$sex.index <- as.numeric(as.factor(test$Sex))


# Remove NA from AGE column
# Will need to replace later? 
train$Age[is.na(train$Age)] <- 0
test$Age[is.na(test$Age)] <- 0


# Index and reclassify the embarked letters to numbers
train$embarked.index <- as.numeric(as.factor(train$Embarked))
test$embarked.index <- as.numeric(as.factor(test$Embarked))


# Strip out the titles of each person:
title <- grep("\\.",value=TRUE,train$Name)
train$title <- grep("[[:alpha:]]{2,6}",value=TRUE,train$Name)
head(train$title)

titleonly <- gsub("[[:alpha:]]+[[:punct:]]{1}.+",replacement="\\1",train$Name[title])
titleonly <- data.frame(titleonly)
head(titleonly)

head(ttrain$title)
ttrain$title <- NULL

test <- grep("http",df$url)

testinfo <- gsub("[[:alpha:]]{4}[[:punct:]]{3}[[:alpha:]]{3}[[:punct:]]{1}([0-9,A-Z,a-z]+)[[:punct:]]{1}.+",replacement="\\1",df$url[test])
testinfo <- as.data.frame(testinfo)




mrtitle <- grep("Mr.",fixed=TRUE,train$Name)
mrtitle <- as.data.frame(mrtitle)
mrstitle <- grep("Mrs.",fixed=TRUE,train$Name)
mstitle <- grep("Miss.",fixed=TRUE,ttrain$Name)
msttitle <- grep("Master.",fixed=TRUE,ttrain$Name)
mmetitle <- grep("Mme.",fixed=TRUE,ttrain$Name)
majortitle <- grep("Major.",fixed=TRUE,ttrain$Name)




reducedtitanic <- ttrain[,c(2,3,15,13,14,7,8,10)]
sapply(reducedtitanic,class)


rf <- randomForest(Survived ~., data=reducedtitanic, mtry=2, ntree=3000, importance=TRUE, do.trace=100)
rf2 <- randomForest(Survived ~., data=reducedtitanic, mtry=2, ntree=5000, importance=TRUE, do.trace=100)
rf3 <- randomForest(Survived ~., data=reducedtitanic, mtry=2, ntree=5000, importance=TRUE, do.trace=100)
rf3 <- randomForest(Survived ~., data=reducedtitanic, mtry=2, nPerm=4, ntree=5000, importance=TRUE, do.trace=100)
print(rf)

detach(ttrain)
colnames(ttrain)
attach(ttrain)
model <- glm(Survived ~ Pclass + sex_index + age_lm + SibSp + Parch + Fare + embarked_index,family=binomial(link="logit"))
summary(model)

predict(model,newdata=ttrain, type="response")

p <- predict(model,newdata=ttrain, type="response")
p <- as.data.frame(p)
p.survive <- round(p)

attach(ttrain)
require(caret)
confusionMatrix(p.survive$p,ttrain$Survived)




prediction1 <- predict(rf, ttest)
prediction2 <- predict(rf2, ttest)
prediction3 <- predict(rf3, ttest)
prediction1 <- as.data.frame(prediction1)
prediction2 <- as.data.frame(prediction2)
prediction3 <- as.data.frame(prediction3)
#check for NAs
any(is.na(prediction1$prediction1))
any(is.na(prediction2$prediction2))
any(is.na(prediction3$prediction3))
#change NAs to zero
prediction1$prediction1[is.na(prediction1$prediction1)] <- 0
prediction2$prediction2[is.na(prediction2$prediction2)] <- 0
prediction3$prediction3[is.na(prediction3$prediction3)] <- 0


prediction <- (prediction1$prediction1 + prediction2$prediction2 + prediction3$prediction3)/3
prediction <- as.data.frame(prediction)
class(prediction$prediction)
any(is.na(prediction$prediction))

prediction$prediction <- round(prediction$prediction)

#confusion matrix
prediction1 <- predict(rf, ttrain)
prediction1$prediction1 <- round(prediction1$prediction1)
confusionMatrix(prediction1$prediction1,ttrain$Survived)

colnames(prediction) <- c("Survived")

submissiondf <- ttest$PassengerId
submissiondf <- as.data.frame(submissiondf)
colnames(submissiondf) <- c("PassengerId")
submissiondf$Survived <- prediction$Survived

any(is.na(submissiondf$PassengerId))
any(is.na(submissiondf$Survived))


write.table(submissiondf,file="submission3.csv",sep=",",row.names=F)


table(ttrain$Name)
title <- grep("\\.",value=TRUE,ttrain$Name)
title <- as.data.frame(title)

mrtitle <- grep("Mr.",fixed=TRUE,ttrain$Name)
mrtitle <- as.data.frame(mrtitle)
mrstitle <- grep("Mrs.",fixed=TRUE,ttrain$Name)
mstitle <- grep("Miss.",fixed=TRUE,ttrain$Name)
msttitle <- grep("Master.",fixed=TRUE,ttrain$Name)
mmetitle <- grep("Mme.",fixed=TRUE,ttrain$Name)
majortitle <- grep("Major.",fixed=TRUE,ttrain$Name)

testinfo <- gsub("[[:alpha:]]+[[:punct:]]{1}([[:alpha:]]+[[:punct:]]{1})[[:alpha:]].+",replacement="\\1",ttrain$Name[title])
testinfo <- as.data.frame(testinfo)
testinfo <- gsub("[[:alpha:]]{4}[[:punct:]]{3}([0-9,A-Z,a-z]+)[[:punct:]]{1}.+",replacement="\\1",testinfo$testinfo[test])
testinfo <- as.data.frame(testinfo)

#put the http link info back into the kaggle dataset
#the use the aggregate function
df$website <- testinfo$testinfo
