tree_pred <- rpart(type ~ ., data =train)
result <- round(predict(tree_pred, test))

table(result)

# 1      2      4      5      6      7 
#4584 332808 115424  79213  14356  19507  


train$soil_type <- as.integer(train$soil_type)
test$soil_type <- as.integer(test$soil_type)

train$wilderness_type <- as.integer(train$wilderness_type)
test$wilderness_type <- as.integer(test$wilderness_type)

tree_pred <- neuralnet(type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology
+ Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am 
+ Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + soil_type + wilderness_type, data =train)

result <- compute(tree_pred, test)
table(result$net.result)

# 2.11260619075209  3.9999895855812 4.00000033881339 
#         1                1           565890 

# Overall NN seems preety useless everytime 

train$soil_type <- as.factor(train$soil_type)
test$soil_type <- as.factor(test$soil_type)

train$wilderness_type <- as.factor(train$wilderness_type)
test$wilderness_type <- as.factor(test$wilderness_type)


tree_pred <- randomForest(type ~ ., data =train)
result <- round(predict(tree_pred, test))

table(result)

# result
# 1      2      3      4      5      6      7 
# 105080 265942  83203  56203  29208  12012  14244 

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, result)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction17.csv", sep = ",", quote = TRUE, row.names = FALSE)

#Normalizing and then applying KNN

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}  

train_n <- as.data.frame(lapply(train, normalize))
test_n <- as.data.frame(lapply(test, normalize))


test_pred <- knn(train = train_n, test = test_n, cl=type, k =1)

table(test_pred)

# 1      2      3      4      5      6      7 
#197618 217739  33097   4554  49989  26181  36714 

#Not impressed with the result hence, not submitting it


test_pred <- knn(train = train_n, test = test_n, cl=type, k =10)

table(test_pred)

# 1      2      3      4      5      6      7 
#179533 197653  31344   6655  74504  29444  46759 

# so normalization not useful

#log(Elevation)

for(i in 1:nrow(train))
{
  train$logElevation[i] <-  log10(train$Elevation[i])
}

for(i in 1:nrow(test))
{
  test$logElevation[i] <-  log10(test$Elevation[i])
}

for(i in 1:nrow(train))
{
   if(train$Vertical_Distance_To_Hydrology[i] == 0 )
     train$ratio[i] <- 0
   else
    train$ratio[i] <-  (train$Horizontal_Distance_To_Hydrology[i]/train$Vertical_Distance_To_Hydrology[i])
}

for(i in 1:nrow(test))
{
  if(test$Vertical_Distance_To_Hydrology[i] == 0 )
    test$ratio[i] <- 0
  else
    test$ratio[i] <-  (test$Horizontal_Distance_To_Hydrology[i]/test$Vertical_Distance_To_Hydrology[i])
}

#as knn  has been always a top performer so now concentrating only on knn
#Note that almost 50% of the right answer is 2 and then 37% is 1 and 4 is 0.1%

tree_pred <- knn(train = train, test = test, cl=type, k =1)

table(tree_pred)

# tree_pred
# 1      2      3      4      5      6      7 
# 206327 218014  40089   2791  36718  25121  36832

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, tree_pred)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction18.csv", sep = ",", quote = TRUE, row.names = FALSE)


tree_pred <- knn(train = train, test = test, cl=type, k =10)

table(tree_pred)

# tree_pred
# 1      2      3      4      5      6      7 
# 178898 177662  40688   5101  71347  31437  60759 


tree_pred <- knn(train = train, test = test, cl=type, k =120)

table(tree_pred)

# tree_pred
# 1      2      3      4      5      6      7 
# 157705 113565  33303   9287 119943  29575 102514 
 

tree_pred <- rpart(type ~ ., data =train)
result <- round(predict(tree_pred, test))

table(result)

# result
# 1      2      3      4      5      6      7 
#4584 312647  41883 158409  14506  14356  19507 


#Relacing 1 with 4 and vice versa
for(i in 1:565892)
{
  if(result[i] == 1)
    result[i] <- 4
  else
  if(result[i] == 4)
    result[i] <- 1
}
# result
# 1      2      3      4      5      6      7 
# 158409 312647  41883   4584  14506  14356  19507 

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, result)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction19.csv", sep = ",", quote = TRUE, row.names = FALSE)

library(randomForest)

tree_pred <- randomForest(type ~ ., data =train)
result <- round(predict(tree_pred, test))


table(result)

# result
#1      2      3      4      5      6      7 
#108875 255351  86772  58837  29949  12520  13588 


sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, result)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction20.csv", sep = ",", quote = TRUE, row.names = FALSE)


for(i in 1:nrow(train))
{
  train$logAspect[i] <-  log10(train$Aspect[i])
}

for(i in 1:nrow(test))
{
  test$logAspect[i] <-  log10(test$Aspect[i])
}


for(i in 1:nrow(train))
{
  train$logSlope[i] <-  log10(train$Slope[i])
}

for(i in 1:nrow(test))
{
  test$logSlope[i] <-  log10(test$Slope[i])
}

for(i in 1:nrow(train))
{
  train$diff1[i] <-  abs(train$Hillshade_Noon[i] - train$Hillshade_9am[i])
  train$diff2[i] <- abs(train$Hillshade_Noon[i] - train$Hillshade_3pm[i])
}

for(i in 1:nrow(test))
{
  test$diff1[i] <-  abs(test$Hillshade_Noon[i] - test$Hillshade_9am[i])
  test$diff2[i] <- abs(test$Hillshade_Noon[i] - test$Hillshade_3pm[i])
}

#there are some infinite values generated
library(foreach)
foreach(i= 1:nrow(train)) %do%
{
  if(train$logAspect[i] == -Inf)
    train$logAspect[i] <- 0
}

foreach(i= 1:nrow(test)) %do%
{
  if(test$logAspect[i] == -Inf)
    test$logAspect[i] <- 0
}

for(i in 1:nrow(train))
   {
    if(train$logSlope[i] == -Inf)
       train$logSlope[i] <- 0
   }



for(i in 1:nrow(test))
{
  if(test$logSlope[i] == -Inf)
    test$logSlope[i] <- 0
}


library(class)
tree_pred <- knn(train = train, test = test, cl=type, k =1)

table(tree_pred)

# tree_pred
# 1      2      3      4      5      6      7 
# 206581 218277  40034   2787  36407  25150  36656 

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, tree_pred)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction21.csv", sep = ",", quote = TRUE, row.names = FALSE)


tree_pred <- knn(train = train, test = test, cl=type, k =10)

table(tree_pred)

# tree_pred
# 1      2      3      4      5      6      7 
# 179212 177947  40536   4949  70846  31658  60744 

train_k <- train[-c(1, 2,3,7,8,9)] 
test_k <- test[-c(1,2,3,7,8,9)]

tree_pred <- knn(train = train_k, test = test_k, cl=type, k =1)
table(tree_pred)

# tree_pred
# 1      2      3      4      5      6      7 
# 171039 173525  54282  11717  45251  44012  66066  


for(i in 1:nrow(train))
{
  train$logHoriz[i] <-  log10(train$Horizontal_Distance_To_Hydrology[i])
}

for(i in 1:nrow(test))
{
  test$logHoriz[i] <-  log10(test$Horizontal_Distance_To_Hydrology[i])
}


for(i in 1:nrow(train))
{
  if(train$Vertical_Distance_To_Hydrology [i] < 0)
    train$logVer[i] <- 0
  else
  train$logVer[i] <-  log10(train$Vertical_Distance_To_Hydrology[i])
}

for(i in 1:nrow(test))
{
  if(test$Vertical_Distance_To_Hydrology [i] < 0)
    test$logVer[i] <- 0
  else
  test$logVer[i] <-  log10(test$Vertical_Distance_To_Hydrology[i])
}


for(i in 1:nrow(train))
{
  train$logRoad[i] <-  log10(train$Horizontal_Distance_To_Roadways[i])
}

for(i in 1:nrow(test))
{
  test$logRoad[i] <-  log10(test$Horizontal_Distance_To_Roadways[i])
}

# 

for(i in 1:nrow(train))
{
  train$logFire[i] <-  log10(train$Horizontal_Distance_To_Fire_Points[i])
}

for(i in 1:nrow(test))
{
  test$logFire[i] <-  log10(test$Horizontal_Distance_To_Fire_Points[i])
}


library(class)

for(i in 1:nrow(train))
{
  if(train$logHoriz[i] == -Inf)
    train$logHoriz[i] <- 0
}



for(i in 1:nrow(test))
{
  if(test$logHoriz[i] == -Inf)
    test$logHoriz[i] <- 0
}



for(i in 1:nrow(train))
{
  if(train$logVer[i] == -Inf)
    train$logVer[i] <- 0
}



for(i in 1:nrow(test))
{
  if(test$logVer[i] == -Inf)
    test$logVer[i] <- 0
}

for(i in 1:nrow(train))
{
  if(train$logRoad[i] == -Inf)
    train$logRoad[i] <- 0
}

for(i in 1:nrow(test))
   {
     if(test$logFire[i] == -Inf)
       test$logFire[i] <- 0
   }



for(i in 1:nrow(train))
{
  if(train$logFire[i] == -Inf)
    train$logFire[i] <- 0
}

for(i in 1:nrow(test))
{
  if(test$logFire[i] == -Inf)
    test$logFire[i] <- 0
}


tree_pred <- knn(train = train, test = test, cl=type, k =1)

table(tree_pred)

# tree_pred
# 1      2      3      4      5      6      7 
# 206580 218277  40035   2786  36408  25150  36656 
#No much improvement in the result

tree_pred <- knn(train = train, test = test, cl=type, k =10)


table(tree_pred)

# tree_pred
# 1      2      3      4      5      6      7 
#179156 177945  40563   4994  70916  31609  60709 
#No improvement in the result
#library(rpart)
#library(party)
#library(tree)
#library(neuralnet)

#test_pred <- neuralnet(type ~ ., data = train)
#result <- round(predict(test_pred, test))
#table(result)


#sampleSubmission <- sampleSubmission[-2]
#sampleSubmission <- cbind(sampleSubmission, tree_pred)
#colnames(sampleSubmission)[2] <- "Cover_Type"

#write.table (sampleSubmission, file = "prediction21.csv", sep = ",", quote = TRUE, row.names = FALSE)



