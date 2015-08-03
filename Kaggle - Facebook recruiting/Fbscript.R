bids <- read.csv("D:/My Folder/R/Kaggle - Facebook recruiting/bids.csv/bids.csv", stringsAsFactors=FALSE)

test <- read.csv("D:/My Folder/R/Kaggle - Facebook recruiting/test.csv/test.csv", stringsAsFactors=FALSE)

train <- read.csv("D:/My Folder/R/Kaggle - Facebook recruiting/train.csv/train.csv", stringsAsFactors=FALSE)


table(train$outcome)
#   0    1 
# 1910  103 

table(train$bidder_id %in% bids$bidder_id)
# FALSE  TRUE 
# 29  1984

table(bids$bidder_id %in% train$bidder_id)

# FALSE    TRUE 
# 4585110 3071224 


table(test$bidder_id %in% bids$bidder_id)
# FALSE  TRUE 
# 70  4630

table(bids$bidder_id %in% test$bidder_id)

#  FALSE    TRUE 
# 3071224 4585110 


#if you add the TRUE columns of each you'll realise that they addup to be the no.of rows 
# in bids table

unique(bids$device)
# [7351] "phone7498"

unique(bids$bidder_id)
# 6614

unique(bids_train$ip)

#seperating the bids which are present in train and test columns

bids_train <- bids[which(bids$bidder_id %in% train$bidder_id),]

bids_test <- bids[which(bids$bidder_id %in% test$bidder_id),]


fraud_bidder_id <- train[which(train$outcome == 1), ]$bidder_id


table(bids_train$bidder_id %in% fraud_bidder_id)
#FALSE    TRUE 
#2658808  412416 

bids_train$outcome <- ifelse(bids_train$bidder_id %in% fraud_bidder_id, 1, 0)
table(bids_train$outcome)
# 0       1 
# 2658808  412416 

unique(bids_train$merchandise)
unique(bids_train$country)
table(bids_train$auction)

bids_train$merchandise_number <- as.numeric(as.factor(bids_train$merchandise))
bids_train$device_number <- as.numeric(as.factor(bids_train$device))
bids_train$country_number <- as.numeric(as.factor(bids_train$country))
bids_train$auction_number <- as.numeric(as.factor(bids_train$auction))


bids_test$merchandise_number <- as.numeric(as.factor(bids_test$merchandise))
bids_test$device_number <- as.numeric(as.factor(bids_test$device))
bids_test$country_number <- as.numeric(as.factor(bids_test$country))
bids_test$auction_number <- as.numeric(as.factor(bids_test$auction))


#Let us first apply for KNN

library(class)
new_train <- bids_train[,-c(1,2,3,4,5,7,8,9,10)]
new_test <- bids_test[,-c(1,2,3,4,5, 7,8, 9)]
result <- knn(train = new_train, test = new_test, cl = bids_train$outcome, k=175)



  