train <- read.csv("D:/My Folder/R/Kaggle - Titanic/train.csv", stringsAsFactors=FALSE)

library(rpart)


foreach(i = 1: nrow(train)) %do%
{
  train$Sex[i] <- ifelse(train$Sex[i] == "male" , 1, 0)
  
}

train$Sex <- as.integer(train$Sex)

model <- rpart(train$Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare , method="class", data=train)

plotcp(model)

#printcp(model)

test <- read.csv("D:/My Folder/R/Kaggle - Titanic/test.csv", stringsAsFactors=FALSE)


foreach(i = 1: nrow(test)) %do%
{
  test$Sex[i] <- ifelse(test$Sex[i] == "male" , 1, 0)
  
}

test$Sex <- as.integer(test$Sex)

tree_pred = predict(model, test)

head(tree_pred)

#     0         1
#1 0.8318264 0.1681736
#2 0.2972973 0.7027027
#3 0.8318264 0.1681736
#4 0.8318264 0.1681736
#5 0.6969697 0.3030303
#6 0.8318264 0.1681736

my <- as.array(10)

foreach(i = 1: nrow(tree_pred)) %do%
{
  my[i] <- ifelse(tree_pred[i] >= 0.5  , 0, 1)
  
}

head(my)

# [1] 0 1 0 0 0 0

gendermodel <- gendermodel[-2]
gendermodel[,2] <- NA
colnames(gendermodel) [2] <- "Survived"

gendermodel <- cbind(gendermodel, my)

write.table (gendermodel, file = "prediction6.csv", sep = ",", quote = TRUE, row.names = FALSE)

printcp(model)

#    CP       nsplit rel error  xerror  xstd
#1 0.444444      0   1.00000 1.00000 0.042446
#2 0.030702      1   0.55556 0.55556 0.035750
#3 0.023392      3   0.49415 0.51170 0.034675
#4 0.020468      4   0.47076 0.50877 0.034599
#5 0.012671      5   0.45029 0.50292 0.034448
#6 0.010000      8   0.41228 0.48830 0.034061

# Minimum xerror is taken into consideration and the respective 
# cp is noted


model2 = prune(model, cp = 0.01)
plotcp(model2)

tree_pred2 = predict(model2, test)

count = 0;

foreach(i = 1: nrow(tree_pred)) %do%
{
  count <- ifelse(tree_pred[i] == tree_pred2[i], count + 1, count)
    
}
count
#[1] 418

#So there is no change in the values, lets take some other cp value and check

model3 = prune(model, cp = 0.02)
plotcp(model3)

tree_pred3 = predict(model3, test)

count = 0;

foreach(i = 1: nrow(tree_pred)) %do%
{
  count <- ifelse(tree_pred[i] == tree_pred3[i], count + 1, count)
  
}

  count
# [1] 352

head(tree_pred3)

#     0         1
#1 0.8318264 0.1681736
#2 0.4102564 0.5897436
#3 0.8318264 0.1681736
#4 0.8318264 0.1681736
#5 0.4102564 0.5897436
#6 0.8318264 0.1681736

foreach(i = 1: nrow(tree_pred3)) %do%
{
  my[i] <- ifelse(tree_pred3[i] >= 0.5  , 0, 1)
  
}

gendermodel <- cbind(gendermodel, my)
gendermodel <- gendermodel[-2]
colnames(gendermodel) [2] <- "Survived"

write.table (gendermodel, file = "prediction3.csv", sep = ",", quote = TRUE, row.names = FALSE)
# Prunning does not improve the performance here
