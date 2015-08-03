train <- read.csv("D:/My Folder/R/Kaggle - Tradeshift Text Classification/train.csv")
#Read the file

install.packages("rpart")
library(rpart)

attach(train)

#Taking random variable x1 and splitting the dataset as it is too big

x <- split(train, train$x1) #Splitting according to yes no and blank

summary(x[1])

write.table (x[1], file = "split1.csv", sep = ",", quote = TRUE, row.names = FALSE)
#Creating a new csv file for every split
#248190 rows for blank


write.table (x[2], file = "split2.csv", sep = ",", quote = TRUE, row.names = FALSE)
#1306048 rows for NO

write.table (x[3], file = "split3.csv", sep = ",", quote = TRUE, row.names = FALSE)
#145762 rows for YES

table(train$x1)

#             NO     YES 
# 248190 1306048  145762 = 1700000

split3 <- read.csv("D:/My Folder/R/Kaggle - Tradeshift Text Classification/split3.csv", stringsAsFactors=FALSE)

trainLabels <- read.csv("D:/My Folder/R/Kaggle - Tradeshift Text Classification/trainLabels.csv")

label3 <- as.data.frame(split3$YES.id)
#taking out the id's of split3

colnames (label3)[1] <- "id"
#Renaming the column name to id so that we could compare it with trainLabels data frame

label3 <- merge(label3,trainLabels,by="id")
#All the columns in label 3 and trainLabels is merged with a common term id.

attach(split3)
library(rpart)

#model3 <- rpart(label3$y1 ~ ., method="class", data=split3)

library(tree)


#Adding the target variable to the training table

split3<-data.frame(split3, label3$y1)


model3 <- tree(label3.y1 ~ ., method="class", data=split3)
#printcp(model3) # display the results 
#plotcp(model3)


#plot(model3)
#text(model3) Get an error - Cannot plot a single node tree

test <- read.csv("D:/My Folder/R/Kaggle - Tradeshift Text Classification/test.csv", stringsAsFactors=FALSE)

tree_pred = predict(model3, test)
summary(tree_pred)

#  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0002333 0.0002333 0.0002333 0.0002333 0.0002333 0.0002333 

table(trainLabels$y1) #     0 -   1689631     1 - 10369
table(trainLabels$y2) #     0 -   1698871     1  - 1129
table(trainLabels$y3) #     0 -   1664400     1 - 35600
table(trainLabels$y4) #     0 -   1677704     1 - 22296
table(trainLabels$y5) #     0 -   1699855     1 - 145
      
    
    
    
