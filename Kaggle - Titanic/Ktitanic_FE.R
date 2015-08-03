train <- read.csv("D:/My Folder/R/Kaggle - Titanic/train.csv", stringsAsFactors=FALSE)

summary(train$Age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.42   20.12   28.00   29.70   38.00   80.00     177 

#Replacing Na with 30
i<- 0

foreach(i = 1:nrow(train)) %do%
{
  if(is.na(train$Age[i]))
  {
    train$Age[i] <- 30
  }
}

summary(train$Age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.42   22.00   30.00   29.76   35.00   80.00 

test <- read.csv("D:/My Folder/R/Kaggle - Titanic/test.csv", stringsAsFactors=FALSE)

summary(test$Age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.17   21.00   27.00   30.27   39.00   76.00      86 

foreach(i = 1:nrow(test)) %do%
{
  if(is.na(test$Age[i]))
  {
    test$Age[i] <- 30
  }
}

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.17   23.00   30.00   30.22   35.75   76.00 

foreach(i = 1: nrow(train)) %do%
{
  train$Sex[i] <- ifelse(train$Sex[i] == "male" , 1, 0)
  
}

foreach(i = 1: nrow(test)) %do%
{
  test$Sex[i] <- ifelse(test$Sex[i] == "male" , 1, 0)
  
}

survived <- train$Survived
train <- train[-2]

# Let us consider people in the age group 16-45 as active and create a new feature 
# 1 - if in the group 16-45 0 - if not


table(train$Age > 16 & train$Age <45)

# FALSE  TRUE 
# 215   676 

train$survival_chance <- 0

foreach(i = 1:nrow(train)) %do%
{
  train$survival_chance <- ifelse(train$Age > 16 & train$Age < 45 , 1, 0)
}

table(train$survival_chance)
#  0   1 
# 215 676 

foreach(i = 1:nrow(test)) %do%
{
  test$survival_chance <- ifelse(test$Age > 16 & test$Age < 45 , 1, 0)
}

train$Sex <- as.integer(train$Sex)
test$Sex <- as.integer(test$Sex)

library(rpart)
model <- rpart(survived ~ Pclass + Sex + Age + SibSp + Parch + survival_chance , method="class", data=train)
tree_pred = predict(model, test)

summary(tree_pred)
head(tree_pred)

result <- c(0,3)

foreach(i = 1: nrow(tree_pred)) %do%
{
  result[i] <- ifelse(tree_pred[i] >= 0.5  , 1, 0)
  
}

table(result)
# result
#  0   1 
# 266 152 

gendermodel <- read.csv("D:/My Folder/R/Kaggle - Titanic/gendermodel.csv", stringsAsFactors=FALSE)

gendermodel <- gendermodel[-2]
gendermodel[,2] <- NA


gendermodel <- cbind(gendermodel, result)
colnames(gendermodel) [2] <- "Survived"


write.table (gendermodel, file = "prediction7.csv", sep = ",", quote = TRUE, row.names = FALSE)
# Gets 75.598% accuracy. Not the best one

table(train$SibSp == 0)
# FALSE  TRUE 
#  283   608

table(train$Parch == 0)
# FALSE  TRUE 
# 213   678  

table(test$SibSp == 0)
# FALSE  TRUE 
# 135   283 

table(test$Parch == 0)
# FALSE  TRUE 
# 94   324 

#there is a possibility that people with 0 relatives on baord havea better chance to survive
#Lets find people whose SibSp as well Parch is 0

foreach(i= 1:nrow(train)) %do%
{
  train$Single <- ifelse(train$SibSp == 0 & train$Parch == 0, 1, 0)
}

table(train$Single)
#  0   1 
# 354 537 

foreach(i= 1:nrow(test)) %do%
{
  test$Single <- ifelse(test$SibSp == 0 & test$Parch == 0, 1, 0)
}

table(test$Single)

#  0   1 
# 165 253 

model <- rpart(survived ~ Pclass + Sex + Age + survival_chance + Single   , method="class", data=train)
tree_pred = predict(model, test)


foreach(i = 1: nrow(tree_pred)) %do%
{
  result[i] <- ifelse(tree_pred[i] >= 0.5  , 0, 1)
  
}
table(result)

result
#  0   1 
# 286 132 

gendermodel <- gendermodel[-2]
gendermodel[,2] <- NA


gendermodel <- cbind(gendermodel, result)
colnames(gendermodel) [2] <- "Survived"


write.table (gendermodel, file = "prediction8.csv", sep = ",", quote = TRUE, row.names = FALSE)


model <- rpart(survived ~ Pclass + Sex + survival_chance + Single   , method="class", data=train)
tree_pred = predict(model, test)


foreach(i = 1: nrow(tree_pred)) %do%
{
  result[i] <- ifelse(tree_pred[i] >= 0.5  , 0, 1)
  
}

table(result)

# result
#  0   1 
# 298 120 

gendermodel <- gendermodel[-2]

gendermodel <- cbind(gendermodel, result)
colnames(gendermodel) [2] <- "Survived"


write.table (gendermodel, file = "prediction9.csv", sep = ",", quote = TRUE, row.names = FALSE)

#Lets try with the ctree function

library(party)

model <- ctree(survived ~ Pclass + Sex + survival_chance + Single , data=train)
tree_pred = predict(model, test)

table(tree_pred > 0.5)
# FALSE  TRUE 
# 333    85 


foreach(i = 1: nrow(tree_pred)) %do%
{
  result[i] <- ifelse(tree_pred[i] >= 0.5  , 1, 0)
  
}

table(result)

# result
#  0   1 
# 261 157 

gendermodel <- gendermodel[-2]

gendermodel <- cbind(gendermodel, result)
colnames(gendermodel) [2] <- "Survived"


write.table (gendermodel, file = "prediction11.csv", sep = ",", quote = TRUE, row.names = FALSE)
