train <- read.csv("D:/My Folder/R/Kaggle - Forest Cover Type Prediction/train.csv", stringsAsFactors=FALSE)

test <- read.csv("D:/My Folder/R/Kaggle - Forest Cover Type Prediction/test.csv", stringsAsFactors=FALSE)

sampleSubmission <- read.csv("D:/My Folder/R/Kaggle - Forest Cover Type Prediction/sampleSubmission.csv", stringsAsFactors=FALSE)

table(sampleSubmission$Cover_Type)
#    1 
# 565892 

# Lets submit this all 1's 

#Submitting this gives 37.05 % accuracy which means 37% of all the enteries are 1

# Lets submit what all 0's just to cross verify

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, 0)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction.csv", sep = ",", quote = TRUE, row.names = FALSE)
# The cover_type varies from 1 to 7 and 0 is not included

table(train$Cover_Type)
# 1    2    3    4    5    6    7 
# 2160 2160 2160 2160 2160 2160 2160 


#So it has an equal ratio in the training set

#So we know that 37.05 % of the training set is 1. Lets do it for all the cover_type
# i.e 209663 rows should be 1

#For 2

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, 2)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction2.csv", sep = ",", quote = TRUE, row.names = FALSE)
# 49.68 % accuracy i.e 281135 rows should be 2

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, 3)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction3.csv", sep = ",", quote = TRUE, row.names = FALSE)
# 5.9%


sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, 4)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction4.csv", sep = ",", quote = TRUE, row.names = FALSE)
# 0.1%


sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, 5)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction5.csv", sep = ",", quote = TRUE, row.names = FALSE)
# 1.3%

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, 6)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction6.csv", sep = ",", quote = TRUE, row.names = FALSE)
# 2.7%  


sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, 7)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction7.csv", sep = ",", quote = TRUE, row.names = FALSE)
# 3.2 %

#So we now have all the individual percentage of each forest type
str(train)
str(test)
#All values are int

#KNN

type <- train$Cover_Type
train <- train[-56]

library(class)

test_pred <- knn(train = train, test = test, cl = type , k = 1)

table(test_pred)

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, test_pred)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction8.csv", sep = ",", quote = TRUE, row.names = FALSE)
#accuracy of 7%


train <- train[-1]
test <- test[-1]

# k =10 

test_pred <- knn(train = train, test = test, cl = type , k = 10)

table(test_pred)
#      1      2      3      4      5      6      7 
# 178882 178157  40658   5083  71054  31436  60622 

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, test_pred)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction9.csv", sep = ",", quote = TRUE, row.names = FALSE)

# k = 120 sqrt(train)
test_pred <- knn(train = train, test = test, cl = type , k = 120)

table(test_pred)
#      1      2      3      4      5      6      7 
# 157605 113993  33414   9277 119514  29536 102553 

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, test_pred)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction10.csv", sep = ",", quote = TRUE, row.names = FALSE)
