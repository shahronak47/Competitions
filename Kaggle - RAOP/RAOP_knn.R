#Trying Knn so we have to slash all the features which are non numeric

str(raop_train)


raop_train <- raop_train[-(1:4)]
raop_train <- raop_train[-8]
raop_train <- raop_train[-10]
raop_train <- raop_train[-12]


str(raop_test)
raop_test <- raop_test[-(1:4)]
raop_test <- raop_test[-8]
raop_test <- raop_test[-10]

library(class)
test_pred <- knn(train = raop_train, test = raop_test, cl = rcvd, k=10)

table(test_pred)
# FALSE  TRUE 
# 1494   137

test_pred <- as.logical(test_pred)

foreach(i = 1 : 1631) %do% 
{
  test_pred[i] <- ifelse(test_pred[i] == TRUE, 1, 0 )  
}

sampleSubmission <- cbind(sampleSubmission, test_pred)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission) [2] <- "requester_received_pizza"

write.table (sampleSubmission, file = "Final8.csv", sep = ",", quote = TRUE, row.names = FALSE)


#Lets try with k=1

test_pred <- knn(train = raop_train, test = raop_test, cl = rcvd, k=1)

table(test_pred)
# FALSE  TRUE 
#  1227   404 

test_pred <- as.logical(test_pred)

foreach(i = 1 : 1631) %do% 
{
  test_pred[i] <- ifelse(test_pred[i] == TRUE, 1, 0 )  
}

sampleSubmission <- cbind(sampleSubmission, test_pred)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission) [2] <- "requester_received_pizza"

write.table (sampleSubmission, file = "Final9.csv", sep = ",", quote = TRUE, row.names = FALSE)

#K = square root of training examples

test_pred <- knn(train = raop_train, test = raop_test, cl = rcvd, k=64)

table(test_pred)
# FALSE  TRUE 
#  1626     5 

test_pred <- as.logical(test_pred)

foreach(i = 1 : 1631) %do% 
{
  test_pred[i] <- ifelse(test_pred[i] == TRUE, 1, 0 )  
}

sampleSubmission <- cbind(sampleSubmission, test_pred)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission) [2] <- "requester_received_pizza"

write.table (sampleSubmission, file = "Final10.csv", sep = ",", quote = TRUE, row.names = FALSE)


#Normalizing the features

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}  

norm_train <- as.data.frame(lapply(raop_train, normalize))
norm_test <- as.data.frame(lapply(raop_test, normalize))


test_pred <- knn(train = norm_train, test = norm_test, cl = rcvd, k=10)

table(test_pred)
# FALSE  TRUE 
#   1523   108

test_pred <- as.logical(test_pred)

foreach(i = 1 : 1631) %do% 
{
  test_pred[i] <- ifelse(test_pred[i] == TRUE, 1, 0 )  
}

sampleSubmission <- cbind(sampleSubmission, test_pred)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission) [2] <- "requester_received_pizza"

write.table (sampleSubmission, file = "Final11.csv", sep = ",", quote = TRUE, row.names = FALSE)



#K = 1

test_pred <- knn(train = norm_train, test = norm_test, cl = rcvd, k=1)

table(test_pred)
# FALSE  TRUE 
#  1272   359 

test_pred <- as.logical(test_pred)

foreach(i = 1 : 1631) %do% 
{
  test_pred[i] <- ifelse(test_pred[i] == TRUE, 1, 0 )  
}

sampleSubmission <- cbind(sampleSubmission, test_pred)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission) [2] <- "requester_received_pizza"

write.table (sampleSubmission, file = "Final12.csv", sep = ",", quote = TRUE, row.names = FALSE)
