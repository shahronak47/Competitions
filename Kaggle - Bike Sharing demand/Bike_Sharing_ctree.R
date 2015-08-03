install.packages("randomForest")
library(randomForest)

summary(train)
library(party)

str(train)

model <- ctree(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed, data = train)

str(test)

result <- round(predict(model, test))

summary(result)

# Min.   : 28.0000  
#1st Qu.:104.0000  
#Median :150.0000  
#Mean   :185.9436  
#3rd Qu.:252.0000  
#Max.   :518.0000 

sampleSubmission <- cbind(sampleSubmission, result)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction4.csv", sep = ",", quote = TRUE, row.names = FALSE)

#trying some feature engineering

#Seperating the time (00:00:00)
train$time <- factor(substring(train$datetime,12,20))
test$time <- factor(substring(test$datetime, 12,20))

#Converting the dates to day 01-01-2011 as Saturday
train$day <- factor(weekdays(as.Date(train$datetime)))
test$day <- factor(weekdays(as.Date(test$datetime)))

#If sunday or not. As there are less people renting bikes on Sunday
train$sunday <- factor(ifelse(train$day=="Sunday", 1, 0))
test$sunday <- factor(ifelse(test$day=="Sunday", 1, 0))

#Converting 00:00:00 as 0, 01:00:00 as 1 ..
train$hour <- as.numeric(substring(train$time, 1, 2))
test$hour <- as.numeric(substring(test$time, 1, 2))

#As the time of the day affects the sales of the bikes, hence dividing the 24-hour day 
# into 4 parts 4AM-10AM - 1 10AM-3PM etc
library(foreach)

train$daypart <- "4"
test$daypart <- "4"

train$daypart[(train$hour < 11) & (train$hour > 7)] <- 1
test$daypart[(test$hour < 11) & (test$hour > 7)] <- 1

train$daypart[(train$hour < 18) & (train$hour > 11)] <- 2
test$daypart[(test$hour < 18) & (test$hour > 11)] <- 2

train$daypart[(train$hour < 22) & (train$hour > 18)] <- 3
test$daypart[(test$hour < 22) & (test$hour > 18)] <- 3

train$daypart <- factor(train$daypart)
test$daypart <- factor(test$daypart)

train$hour <- factor(train$hour)
test$hour <- factor(test$hour)

model1 <- ctree(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + daypart + sunday, data = train)
result1 <- round(predict(model1, test))

sampleSubmission <- cbind(sampleSubmission, result1)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction5.csv", sep = ",", quote = TRUE, row.names = FALSE)

#Lets make the factors back to integers just to check what difference it makes to the final result

str(train)

#  season    : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
#$ holiday   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#$ workingday: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#$ weather   : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 2 1 1 1 1 ...
#$ temp      : num  9.84 9.02 9.02 9.84 9.84 ...
#$ atemp     : num  14.4 13.6 13.6 14.4 14.4 ...
#$ humidity  : int  81 80 80 75 75 75 80 86 75 76 ...
#$ windspeed : num  0 0 0 0 0 ...
#$ casual    : int  3 8 5 3 0 0 2 1 1 8 ...
#$ registered: int  13 32 27 10 1 1 0 2 7 6 ...
#$ count     : int  16 40 32 13 1 1 2 3 8 14 ...
#$ time      : Factor w/ 24 levels "00:00:00","01:00:00",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ day       : Factor w/ 7 levels "Friday","Monday",..: 3 3 3 3 3 3 3 3 3 3 ...
#$ sunday    : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#$ hour      : Factor w/ 24 levels "0","1","2","3",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ daypart   : Factor w/ 4 levels "1","2","3","4": 4 4 4 4 4 4 4 4 1 1 ...

str(test)
#$ season    : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
#$ holiday   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#$ workingday: Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
#$ weather   : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 2 ...
#$ temp      : num  10.7 10.7 10.7 10.7 10.7 ...
#$ atemp     : num  11.4 13.6 13.6 12.9 12.9 ...
#$ humidity  : int  56 56 56 56 56 60 60 55 55 52 ...
#$ windspeed : num  26 0 0 11 11 ...
#$ time      : Factor w/ 24 levels "00:00:00","01:00:00",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ day       : Factor w/ 7 levels "Friday","Monday",..: 5 5 5 5 5 5 5 5 5 5 ...
#$ sunday    : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#$ hour      : Factor w/ 24 levels "0","1","2","3",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ daypart   : Factor w/ 4 levels "1","2","3","4": 4 4 4 4 4 4 4 4 1 1 ...

train$season <- as.integer(train$season)
train$holiday <- as.integer(train$holiday)
train$workingday <- as.integer(train$workingday)
train$weather <- as.integer(train$weather)
train$time <- as.integer(train$time)
train$day <- as.integer(train$day)
train$sunday <- as.integer(train$sunday)
train$hour <- as.integer(train$hour)
train$daypart <- as.integer(train$daypart)


test$season <- as.integer(test$season)
test$holiday <- as.integer(test$holiday)
test$workingday <- as.integer(test$workingday)
test$weather <- as.integer(test$weather)
test$time <- as.integer(test$time)
test$day <- as.integer(test$day)
test$sunday <- as.integer(test$sunday)
test$hour <- as.integer(test$hour)
test$daypart <- as.integer(test$daypart)

model2 <- ctree(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + daypart + sunday, data = train)
result2 <- round(predict(model2, test))

sampleSubmission <- cbind(sampleSubmission, result2)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction6.csv", sep = ",", quote = TRUE, row.names = FALSE)

count <- 0

foreach(i=1:nrow(result1)) %do%
{
  if(result1[i] == result2[i])
    count <- count + 1
}

count
# 401 
#So there were only 401 out of 6493 same records as result1 in result2
#Also the result was quite less than what it was previous
#So we can conlcude from here that it matters if the features are declared as variables or factors

