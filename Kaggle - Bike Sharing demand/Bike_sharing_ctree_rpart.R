train <- read.csv("D:/My Folder/R/Kaggle - Bike Sharing demand/train.csv", stringsAsFactors=FALSE)

test <- read.csv("D:/My Folder/R/Kaggle - Bike Sharing demand/test.csv", stringsAsFactors=FALSE)


#Feature engineering from Bike_Sharing_ctree.R

train$time <- factor(substring(train$datetime,12,20))
test$time <- factor(substring(test$datetime, 12,20))

train$day <- factor(weekdays(as.Date(train$datetime)))
test$day <- factor(weekdays(as.Date(test$datetime)))


#If sunday or not. As there are less people renting bikes on Sunday
train$sunday <- factor(ifelse(train$day=="Sunday", 1, 0))
test$sunday <- factor(ifelse(test$day=="Sunday", 1, 0))

train$hour <- as.numeric(substring(train$time, 1, 2))
test$hour <- as.numeric(substring(test$time, 1, 2))


train$daypart <- "4"
test$daypart <- "4"

train$daypart[(train$hour < 11) & (train$hour > 7)] <- 1
test$daypart[(test$hour < 11) & (test$hour > 7)] <- 1

train$daypart[(train$hour < 18) & (train$hour > 11)] <- 2
test$daypart[(test$hour < 18) & (test$hour > 11)] <- 2

train$daypart[(train$hour < 22) & (train$hour > 18)] <- 3
test$daypart[(test$hour < 22) & (test$hour > 18)] <- 3

#train$daypart <- factor(train$daypart)
#test$daypart <- factor(test$daypart)

#train$hour <- factor(train$hour)
#test$hour <- factor(test$hour)


train$daypart <- as.integer(train$daypart)
test$daypart <- as.integer(test$daypart)

train$hour <- as.integer(train$hour)
test$hour <- as.integer(test$hour)

train$sunday <- as.integer(train$sunday)
test$sunday <- as.integer(test$sunday)

train$temp <- as.integer(train$temp)
test$temp <- as.integer(test$temp)

train$atemp <- as.integer(train$atemp)
test$atemp <- as.integer(test$atemp)

train$windspeed <- as.integer(train$windspeed)
test$windspeed <- as.integer(test$windspeed)

library(neuralnet)

test <- test[-11]
test <- test[-10]

train <- train[-14]
train <- train[-13]
train <- train[-11]
train <- train[-10]

library(party)

model1 <- ctree(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + daypart + sunday, data = train)
result1 <-round(predict(model1, test))

summary(result1)

#  Min.   :  3.0000  
#1st Qu.: 49.0000  
#Median :166.0000  
#Mean   :189.4318  
#3rd Qu.:287.0000  
#Max.   :655.0000  

sampleSubmission <- cbind(sampleSubmission, result1)


sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction10.csv", sep = ",", quote = TRUE, row.names = FALSE)



library(rpart)

model1 <- rpart(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + daypart + sunday, data = train)
result1 <-round(predict(model1, test))

summary(result1)

#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 33.0000  33.0000 158.0000 191.9486 268.0000 615.0000 

sampleSubmission <- cbind(sampleSubmission, result1)


sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction11.csv", sep = ",", quote = TRUE, row.names = FALSE)



train$daypart <- as.factor(train$daypart)
test$daypart <- as.factor(test$daypart)

train$hour <- as.factor(train$hour)
test$hour <- as.factor(test$hour)

train$sunday <- as.factor(train$sunday)
test$sunday <- as.factor(test$sunday)

train$season <- as.factor(train$season)
test$season <- as.factor(test$season)

train$holiday <- as.factor(train$holiday)
test$holiday <- as.factor(test$holiday)

train$workingday <- as.factor(train$workingday)
test$workingday <- as.factor(test$workingday)

train$weather <- as.factor(train$weather)
test$weather <- as.factor(test$weather)


model1 <- ctree(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + daypart + sunday, data = train)
result1 <-round(predict(model1, test))

summary(result1)

#Min.   :  2.0000  
#1st Qu.: 49.0000  
#Median :155.0000  
#Mean   :189.1617  
#3rd Qu.:272.0000  
#Max.   :635.0000 

sampleSubmission <- cbind(sampleSubmission, result1)


sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction12.csv", sep = ",", quote = TRUE, row.names = FALSE)



library(randomForest)

model1 <- randomForest(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + daypart + sunday, data = train)
result1 <-round(predict(model1, test))

summary(result1)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 4.0000  58.0000 162.0000 189.8469 269.0000 776.0000 

sampleSubmission <- cbind(sampleSubmission, result1)


sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction13.csv", sep = ",", quote = TRUE, row.names = FALSE)


model1 <- ctree(count ~ season + holiday + workingday + weather + humidity + windspeed + hour + daypart + sunday, data = train)
result1 <-round(predict(model1, test))

summary(result1)

#Min.   :  3.0000  
#1st Qu.: 49.0000  
#Median :160.0000  
#Mean   :191.7094  
#3rd Qu.:272.0000  
#Max.   :676.0000 

sampleSubmission <- cbind(sampleSubmission, result1)


sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction14.csv", sep = ",", quote = TRUE, row.names = FALSE)
