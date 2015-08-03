bids <- read.csv("D:/My Folder/R/Kaggle - Facebook recruiting/bids.csv/bids.csv", stringsAsFactors=FALSE)

test <- read.csv("D:/My Folder/R/Kaggle - Facebook recruiting/test.csv/test.csv", stringsAsFactors=FALSE)

train <- read.csv("D:/My Folder/R/Kaggle - Facebook recruiting/train.csv/train.csv", stringsAsFactors=FALSE)


bids_train <- bids[which(bids$bidder_id %in% train$bidder_id),]

bids_test <- bids[which(bids$bidder_id %in% test$bidder_id),]

fraud_bidder_id <- train[which(train$outcome == 1), ]$bidder_id
bids_train$outcome <- ifelse(bids_train$bidder_id %in% fraud_bidder_id, 1, 0)

unique(bids_test$bidder_id)
# [4630]

unique(bids_train$bidder_id)
#1984

bids_train$merchandise_number <- as.numeric(as.factor(bids_train$merchandise))
bids_train$device_number <- as.numeric(as.factor(bids_train$device))
bids_train$country_number <- as.numeric(as.factor(bids_train$country))
bids_train$auction_number <- as.numeric(as.factor(bids_train$auction))


bids_test$merchandise_number <- as.numeric(as.factor(bids_test$merchandise))
bids_test$device_number <- as.numeric(as.factor(bids_test$device))
bids_test$country_number <- as.numeric(as.factor(bids_test$country))
bids_test$auction_number <- as.numeric(as.factor(bids_test$auction))

library(class)
new_train <- bids_train[,-c(1,2,3,4,5,7,8,9,10)]
new_test <- bids_test[,-c(1,2,3,4,5, 7,8, 9)]
result <- knn(train = new_train, test = new_test, cl = bids_train$outcome, k=1)

table(result)
#    0       1 
# 3981762  603348 


result_table <- data.frame(cbind(bids_test$bidder_id,result$result))

colnames(result_table)[1] <- "bidder_id"
colnames(result_table)[2] <- "result"

table(result_table$result)

#  1       2 
#3981762  603348 

library(dplyr)
res <- aggregate(result ~ bidder_id, result_table, FUN=function(x){which.max(c(sum(x==1), sum(x==2)))})

table(res$result)
#    1    2 
# 4486  144 

#Replacing 1 to 0 and 2 to 1
res$result[res$result == 1] <- 0
res$result[res$result == 2] <- 1


outcome <- res$result
outcome[4631:4700] <- 0
sampleSubmission <- cbind(sampleSubmission, outcome)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "prediction"
write.table (sampleSubmission, file = "prediction1.csv", sep = ",", quote = TRUE, row.names = FALSE)
