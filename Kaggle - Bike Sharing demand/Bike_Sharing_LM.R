train <- read.csv("D:/My Folder/R/Kaggle - Bike Sharing demand/train.csv", stringsAsFactors=FALSE)

test <- read.csv("D:/My Folder/R/Kaggle - Bike Sharing demand/test.csv", stringsAsFactors=FALSE)

str(train)
#Lets ignore casual and registered features 
#Season, Holiday, WorkingDay, Whether are categorical values so lets convert it into factors


train$season <- factor(train$season)
train$holiday <- factor(train$holiday)
train$workingday <- factor(train$workingday)
train$weather <- factor(train$weather)

summary(train)
library(stats)

model <- lm(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed, data = train)

str(test)
test$season <- factor(test$season)
test$holiday <- factor(test$holiday)
test$workingday <- factor(test$workingday)
test$weather <- factor(test$weather)

result <- round(predict.lm(model, test))
summary(result)

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-74.0000 117.0000 186.0000 187.2195 258.0000 460.0000 

summary(result > 0)
# Mode   FALSE    TRUE    NA's 
# logical     197    6296       0 

mean(result)
# 187.2194671

foreach( i = 1:6493) %do% 
{
  result[i] <- ifelse(result[i] < 0.0, 187,result[i])
}

summary(result > 0)

#Mode   FALSE    TRUE    NA's 
#logical      11    6482       0 

summary(result== 0)

#Mode   FALSE    TRUE    NA's 
#logical    6482      11       0 

sampleSubmission <- cbind(sampleSubmission, result)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction1.csv", sep = ",", quote = TRUE, row.names = FALSE)

which (train$count == max(train$count))
# [1] 9346

train[9346,]
# datetime season holiday workingday weather  temp atemp humidity windspeed
# 12-09-12 18:00      3       0          1       1 27.06 31.06       44   16.9979
# casual registered count
#  91        886   977


model1 <- lm(count ~ season + weather + temp + atemp + humidity + windspeed, data = train)
result1 <- round(predict.lm(model1, test))
summary(result1)

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -76.0000 118.0000 186.0000 187.2292 257.0000 458.0000 

summary(result1 > 0)
# Mode   FALSE    TRUE    NA's 
# logical     188    6305       0 

mean(result1)
# 187.2291699

foreach( i = 1:6493) %do% 
{
  result1[i] <- ifelse(result1[i] < 0.0, 187,result1[i])
}

summary(result1 > 0)

# Mode   FALSE    TRUE    NA's 
#logical       5    6488       0 

summary(result1== 0)

# Mode   FALSE    TRUE    NA's 
#logical    6488       5       0 

sampleSubmission <- cbind(sampleSubmission, result1)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction2.csv", sep = ",", quote = TRUE, row.names = FALSE)



model2 <- lm(count ~ season + weather + temp + humidity, data = train)
result2 <- round(predict.lm(model2, test))
summary(result2)

#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -76.0000 118.0000 186.0000 187.3678 257.0000 469.0000

summary(result2 > 0)
# Mode   FALSE    TRUE    NA's 
# logical  185    6308       0 

mean(result2)
# 187.3677807

foreach( i = 1:6493) %do% 
{
  result2[i] <- ifelse(result2[i] < 0.0, 187,result2[i])
}

summary(result2 > 0)

# Mode   FALSE    TRUE    NA's 
#logical   4    6489       0 

summary(result2== 0)

# Mode   FALSE    TRUE    NA's 
#logical  6489       4       0

sampleSubmission <- cbind(sampleSubmission, result2)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "count"

write.table (sampleSubmission, file = "prediction3.csv", sep = ",", quote = TRUE, row.names = FALSE)
