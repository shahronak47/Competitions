train <- read.csv("D:/My Folder/R/Kaggle - Titanic/train.csv", stringsAsFactors=FALSE)

i<- 0

library(foreach)

foreach(i = 1:nrow(train)) %do%
{
  if(is.na(train$Age[i]))
  {
    train$Age[i] <- 30
  }
}

test <- read.csv("D:/My Folder/R/Kaggle - Titanic/test.csv", stringsAsFactors=FALSE)

foreach(i = 1:nrow(test)) %do%
{
  if(is.na(test$Age[i]))
  {
    test$Age[i] <- 30
  }
}

foreach(i = 1: nrow(train)) %do%
{
  train$Sex[i] <- ifelse(train$Sex[i] == "male" , 1, 0)
  
}

foreach(i = 1: nrow(test)) %do%
{
  test$Sex[i] <- ifelse(test$Sex[i] == "male" , 1, 0)
  
}

survived <- train$Survived


foreach(i = 1:nrow(train)) %do%
{
  train$survival_chance <- ifelse(train$Age > 16 & train$Age < 45 , 1, 0)
}


foreach(i = 1:nrow(test)) %do%
{
  test$survival_chance <- ifelse(test$Age > 16 & test$Age < 45 , 1, 0)
}

train$Sex <- as.integer(train$Sex)
test$Sex <- as.integer(test$Sex)


foreach(i= 1:nrow(train)) %do%
{
  train$Single <- ifelse(train$SibSp == 0 & train$Parch == 0, 1, 0)
}


foreach(i= 1:nrow(test)) %do%
{
  test$Single <- ifelse(test$SibSp == 0 & test$Parch == 0, 1, 0)
}


model <- randomForest(survived ~ Pclass + Sex + Age + SibSp + Parch + survival_chance + Single ,  data=train)
tree_pred = predict(model, test)

summary(tree_pred)
library(foreach)

foreach( j = 1:418) %do% 
{
  result[j] <- ifelse(tree_pred[j] > 0.5, 1,0)
  
}


table(result)

# result
#  0   1 
# 280 138

gendermodel <- cbind(gendermodel, result)
gendermodel <- gendermodel[-2]
colnames(gendermodel) [2] <- "Survived"


write.table (gendermodel, file = "prediction12.csv", sep = ",", quote = TRUE, row.names = FALSE)




foreach( j = 1:418) %do% 
{
  result[j] <- 0
  
}


table(result)

# result
#  0  
# 418

gendermodel <- cbind(gendermodel, result)
gendermodel <- gendermodel[-2]
colnames(gendermodel) [2] <- "Survived"


write.table (gendermodel, file = "prediction13.csv", sep = ",", quote = TRUE, row.names = FALSE)


#All the 0's give the result as 62.6 which means out of the total values 62.6% of the 
#values are 0 i.e 418 * 0.62 = 260 of them should be zero approx

model <- rpart(survived ~ Pclass + Sex + Age + survival_chance + Single   , data=train)
tree_pred = predict(model, test)

table(tree_pred)
sort(tree_pred)[260]
# 0.3583333333

#tree_pred <- order(tree_pred)
summary(tree_pred)
#  Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# 0.08333333 0.11547340 0.35833330 0.38721050 0.53787880 0.94705880 


foreach(i = 1:418) %do%
{
  result[i] <- ifelse(tree_pred[i] > 0.36  , 1, 0)
  
}

table(result)

# result
# 0   1 
# 261 157 

gendermodel <- cbind(gendermodel, result)
gendermodel <- gendermodel[-2]
colnames(gendermodel) [2] <- "Survived"


write.table (gendermodel, file = "prediction14.csv", sep = ",", quote = TRUE, row.names = FALSE)




library(neuralnet)

model <- neuralnet(survived ~ Pclass + Sex + Age + survival_chance + Single   , data=train)
tree_pred <- compute(model, test)

# Error in neurons[[i]] %*% weights[[i]] : 
# requires numeric/complex matrix/vector arguments
