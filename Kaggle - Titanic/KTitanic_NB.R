library(e1071)

foreach(i= 1:nrow(train)) %do% 
{
  if(is.na(train$Age[i]))
    train$Age[i] <- 30
}

foreach(i= 1:nrow(test)) %do% 
{
  if(is.na(test$Age[i]))
    test$Age[i] <- 30
}

label <- train$Survived

train <- train[-2]


for (i in 1:ncol(train))
{
  train[, i] <- as.factor(train[,i])
  test[, i] <- as.factor(test[,i])
}

install.packages("klaR")
library(klaR)
library(MASS)

result <- naiveBayes(train, label)
result_pred <- predict(result,test)

#count = 0
#foreach(i =1 : ncol(train)) %do%
#{
#  if(colnames(train)[i] == colnames(test)[i])
#  {
#    count <- count + 1
#  }
#}
