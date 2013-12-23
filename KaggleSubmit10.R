rm(list=ls(all=TRUE))
setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Kaggle_Competition")
train <- read.table("train.tsv",header=T,sep="\t", stringsAsFactors = FALSE)
test <- read.table("test.tsv",header=T,sep="\t", stringsAsFactors = FALSE)
require(plyr)
require(randomForest)
require(ggplot2)
require(rjson)
require(RTextTools)
require(useful)

jsonDataTrain <- sapply(train$boilerplate, fromJSON)
train$title <- sapply(1:nrow(train), function(i, jsonDataTrain) unlist(jsonDataTrain[[i]])["title"], jsonDataTrain)
train$body <- sapply(1:nrow(train), function(i, jsonDataTrain) unlist(jsonDataTrain[[i]])["body"], jsonDataTrain)
train$bp_url <- sapply(1:nrow(train), function(i, jsonDataTrain) unlist(jsonDataTrain[[i]])["url"], jsonDataTrain)

jsonDataTest <- sapply(train$boilerplate, fromJSON)
test$title <- sapply(1:nrow(test), function(i, jsonDataTest) unlist(jsonDataTest[[i]])["title"], jsonDataTest)
test$body <- sapply(1:nrow(test), function(i, jsonDataTest) unlist(jsonDataTest[[i]])["body"], jsonDataTest)
test$bp_url <- sapply(1:nrow(test), function(i, jsonDataTest) unlist(jsonDataTest[[i]])["url"], jsonDataTest)


title_matrix <- create_matrix(train$title,language = "english",removeNumbers=TRUE, removeStopwords = TRUE, stemWords=TRUE)
title_text <- as.matrix(title_matrix)
title.nb <- naiveBayes(x=title_text, y=train$label)
head(title.nb$table)
title.predict <- predict(title.nb,test$title)



summary(HouseVotes84)
data(HouseVotes84, package = "mlbench")
model <- naiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,])

pred <- predict(model, HouseVotes84)
table(pred, HouseVotes84$Class)




data.flu <- data.frame(chills = c(1,1,1,0,0,0,0,1), runnyNose = c(0,1,0,1,0,1,1,1), headache = c("M", "N", "S", "M", "N", "S", "S", "M"), fever = c(1,0,1,1,0,1,0,1), flu = c(0,1,1,1,0,1,0,1) )
patient <- data.frame(chills = c(1), runnyNose = c(0), headache = c("M"), fever = c(1))
model.flu <- naiveBayes(as.factor(flu)~., data=data.flu)
predict.flu <- predict(model.flu, patient)
predict.flu


sapply(train,class)
alchemy.index <- as.numeric(as.factor(c(train$alchemy_category, test$alchemy_category)))
identityMatrix <- diag(max(alchemy.index))
categories <- t(sapply(alchemy.index, anonFun <- function(x, identityMatrix){identityMatrix[x, ]}, identityMatrix))




bp <- train$boilerplate
bp <- as.data.frame(bp)


bptitle <- grep("title",bp$bp)
rm(extraction)
extraction <- gsub("[[:punct:]]+[[:alpha:]]+[[:punct:]]+([[:print:]]+)[[:punct:]]+[[:alpha:]]+[[:punct:]]+[[:alpha:]].+",replacement="\\1",bp$bp[bptitle])
extraction <- as.data.frame(extraction)
testinfo <- gsub("([0-9,A-Z,a-z]+[[:punct:]]{1})[[:punct:]]{3}([0-9,A-Z,a-z]+[[:punct:]]{1}).+",replacement="\\1",testinfo$testinfo[test])
testinfo <- as.data.frame(testinfo)

#put the http link info back into the kaggle dataset
#the use the aggregate function
df$website <- testinfo$testinfo


split <- bp

require(stringr)
colnames(split)
class(split$bp)
split <- as.character(split)
x <- strsplit(split$bp, ",")
x <- as.data.frame(x)
length(x)

head(x)

df <- data.frame(matrix(unlist(x), nrow=7395, byrow=T))






#rename the imported data to prevent from making errors
dftrain <- train
dftest <- test

#take out the question marks in columns
dftrain$alchemy_category <- gsub("\\?+","0",dftrain$alchemy_category)
dftrain$alchemy_category_score <- gsub("\\?+",".4",dftrain$alchemy_category_score)
dftrain$news_front_page <- gsub("\\?+",".5",dftrain$news_front_page)
dftrain$is_news <- gsub("\\?+","0",dftrain$is_news)
dftest$alchemy_category <- gsub("\\?+","0",dftest$alchemy_category)
dftest$alchemy_category_score <- gsub("\\?+",".4",dftest$alchemy_category_score)
dftest$news_front_page <- gsub("\\?+",".5",dftest$news_front_page)
dftest$is_news <- gsub("\\?+","0",dftest$is_news)


#recategorize the names of categories to numbers so as to be able to include them in the regression
dftrain$alchemy_category <- gsub("arts_entertainment","1",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("business","2",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("computer_internet","3",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("culture_politics","4",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("gaming","5",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("health","6",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("law_crime","7",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("recreation","8",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("science_technology","9",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("sports","10",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("unknown","0",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("weather","11",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("religion","12",dftrain$alchemy_category)

#reclassify the test set as well
table(dftest$alchemy_category)
dftest$alchemy_category <- gsub("arts_entertainment","1",dftest$alchemy_category)
dftest$alchemy_category <- gsub("business","2",dftest$alchemy_category)
dftest$alchemy_category <- gsub("computer_internet","3",dftest$alchemy_category)
dftest$alchemy_category <- gsub("culture_politics","4",dftest$alchemy_category)
dftest$alchemy_category <- gsub("gaming","5",dftest$alchemy_category)
dftest$alchemy_category <- gsub("health","6",dftest$alchemy_category)
dftest$alchemy_category <- gsub("law_crime","7",dftest$alchemy_category)
dftest$alchemy_category <- gsub("recreation","8",dftest$alchemy_category)
dftest$alchemy_category <- gsub("science_technology","9",dftest$alchemy_category)
dftest$alchemy_category <- gsub("sports","10",dftest$alchemy_category)
dftest$alchemy_category <- gsub("religion","12",dftest$alchemy_category)
dftest$alchemy_category <- gsub("unknown","0",dftest$alchemy_category)

#change columns from factor or character into numeric for training set
dftrain$alchemy_category <- as.numeric(dftrain$alchemy_category)
dftrain$alchemy_category_score <- as.numeric(dftrain$alchemy_category_score)
dftrain$is_news <- as.numeric(dftrain$is_news)
dftrain$news_front_page <- as.numeric(dftrain$news_front_page)
#change the columns from factor or charater to numeric for test set
dftest$alchemy_category <- as.numeric(dftest$alchemy_category)
dftest$alchemy_category_score <- as.numeric(dftest$alchemy_category_score)
dftest$is_news <- as.numeric(dftest$is_news)
dftest$news_front_page <- as.numeric(dftest$news_front_page)

#break the training and test sets down into subsets based on category
#look at unknown data as well as good categories
trainsubq <- subset(dftrain, alchemy_category==0)
trainnoq <- subset(dftrain, alchemy_category!=0)
testsubq <- subset(dftest,alchemy_category==0)
testnoq <- subset(dftest,alchemy_category!=0)

colnames(trainsubq)
#create a subset for the random forest of the data for the ? category
question <- trainsubq[,c(4:5)]
noquestion <- trainnoq[,c(4:5)]


#runing a randomforest for each dataset
rfq <- randomForest(label ~., data=question, mtry=6, ntree=3000, do.trace=100)
rfnoq <- randomForest(label ~., data=noquestion, mtry=6, ntree=3000, do.trace=100)
print(trainr7)

predictionq <- predict(rfq, testsubq)
predictionnoq <- predict(rfnoq, testnoq)
predictionq <- as.data.frame(predictionq)
predictionnoq <- as.data.frame(predictionnoq)
colnames(predictionq) <- c("label")
colnames(predictionnoq) <- c("label")


#create a df for the urlid, then insert the prediction column
questionprediction <- testsubq$urlid
questionprediction <- as.data.frame(questionprediction)
colnames(questionprediction) <- c("urlid")
questionprediction$label <- predictionq$label


noquestionprediction <- testnoq$urlid
noquestionprediction <- as.data.frame(noquestionprediction)
colnames(noquestionprediction) <- c("urlid")
noquestionprediction$label <- predictionnoq$label

totalprediction <- rbind(questionprediction,noquestionprediction)

#to correct for prediction error, take 1-prediction
totalprediction$label <- 1-totalprediction$label

totalprediction$label[totalprediction$label < .5] <- 0
totalprediction$label[totalprediction$label >= .5] <- 1

setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Kaggle_Competition")
write.table(totalprediction,file="kaggle_submit_9.csv",sep=",",row.names=F)
