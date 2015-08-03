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

train$daypart <- factor(train$daypart)
test$daypart <- factor(test$daypart)

train$hour <- factor(train$hour)
test$hour <- factor(test$hour)





#Till this it has been copied
#Lets take feature engineering further and see if it helps

train$weather <- factor(train$weather)
test$weather <- factor(test$weather)

train$season <- factor(train$season)
test$season <- factor(test$season)

train$holiday <- factor(train$holiday)
test$holiday <- factor(test$holiday)

train$workingday <- factor(train$workingday)
test$workingday <- factor(test$workingday)

model1 <- ctree(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + daypart + sunday, data = train)
result1 <- round(predict(model1, test))

sampleSubmission <- cbind(sampleSubmission, result1)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction7.csv", sep = ",", quote = TRUE, row.names = FALSE)

#The score still remains the same. No improvement or decrease

#diving the datset now on the basis of temperature
#As it is assumed that people would travel less in less suitable temperature
#Considering 15-35 as suitable temperature
#We'll take into consideration atemp and not temp

summary(train$atemp)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.76   16.66   24.24   23.66   31.06   45.46 

train$suit_temp <- 0
test$suit_temp <- 0

train$suit_temp[(train$atemp >= 15.0) & (train$atemp <= 35)] <- 1
test$suit_temp[(test$atemp >= 15) & (test$atemp <= 35)] <- 1

train$suit_temp <- factor(train$suit_temp)
test$suit_temp <- factor(test$suit_temp)

model1 <- ctree(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + daypart + sunday + suit_temp, data = train)
result1 <- round(predict(model1, test))

sampleSubmission <- cbind(sampleSubmission, result1)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction8.csv", sep = ",", quote = TRUE, row.names = FALSE)

# Improved the score by 0.00133 and up by 5 positions


#Lets apply the lm algo to this

model1 <- lm(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + daypart + sunday + suit_temp, data = train)
result1 <- round(predict(model1, test))

#as there are some negative values generated changing the sign of those values
i<-0

foreach(i= 1:6493) %do%
{
  if(result1[i] < 0)
    result1[i] <- -(result1[i])
}

sampleSubmission <- cbind(sampleSubmission, result1)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction9.csv", sep = ",", quote = TRUE, row.names = FALSE)

#Doesn't change. Infact worse than the previous one


