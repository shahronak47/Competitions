train <- read.csv("D:/My Folder/R/Kaggle - Forest Cover Type Prediction/train.csv", stringsAsFactors=FALSE)

test <- read.csv("D:/My Folder/R/Kaggle - Forest Cover Type Prediction/test.csv", stringsAsFactors=FALSE)

sampleSubmission <- read.csv("D:/My Folder/R/Kaggle - Forest Cover Type Prediction/sampleSubmission.csv", stringsAsFactors=FALSE)

type <- train$Cover_Type
train <- train[-56]

train <- train[-1]
test <- test[-1]

table(type)
#1    2    3    4    5    6    7 
#2160 2160 2160 2160 2160 2160 2160 

library(party)
library(rpart)
test_pred <- rpart(type ~ ., data = train)
result <- round(predict(test_pred, test))
table(result)

#     2      3      4      5      6 
# 324187  41883 152305   5205  42312 

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, result)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction11.csv", sep = ",", quote = TRUE, row.names = FALSE)

#Ctree


test_pred <- ctree(type ~ ., data = train)
result <- round(predict(test_pred, test))
table(result)

#     1      2      3      4      5      6      7 
#  45856 298242  61014  98481  24961  14901  22437 

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, result)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction12.csv", sep = ",", quote = TRUE, row.names = FALSE)


#tree

library(tree)
test_pred <- tree(type ~ ., data = train)
result <- round(predict(test_pred, test))
table(result)

#     2      3      4      5      6 
# 324187  41883 152305   5205  42312 

#Same as rpart hence, not uploading it

#randomforest

library(randomForest)

test_pred <- randomForest(type ~ ., data = train)
result <- round(predict(test_pred, test))
table(result)

#     1      2      3      4      5      6      7 
#  106570 264566  82703  55354  30433  12624  13642 

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, result)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction13.csv", sep = ",", quote = TRUE, row.names = FALSE)



#neuralnet

library(neuralnet)

#train <- cbind(train, type)
#train <- train[-55]
#columns_features <- paste(colnames(train), collapse = "+ ")
#formulas <- as.formula("type ~ columns_features")
#columns_features <- colnames(train)


# Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Wilderness_Area1 + Wilderness_Area2 + Wilderness_Area3 + Wilderness_Area4
#+ Soil_Type1 + Soil_Type2 + Soil_Type3 + Soil_Type4 + Soil_Type5 + Soil_Type6 + Soil_Type7+ Soil_Type8 + Soil_Type9 + Soil_Type10 + Soil_Type11 + Soil_Type12 + Soil_Type13 + Soil_Type14 + Soil_Type15 + Soil_Type16 + Soil_Type17 + Soil_Type18 + Soil_Type19 + Soil_Type20 + Soil_Type21 + Soil_Type22 + Soil_Type23 + Soil_Type24 + Soil_Type25 
#+ Soil_Type26 + Soil_Type27 + Soil_Type28 + Soil_Type29 + Soil_Type30 + Soil_Type31 + Soil_Type32 + Soil_Type33 + Soil_Type34 + Soil_Type35 + Soil_Type36 + Soil_Type37 + Soil_Type38 + Soil_Type39 + Soil_Type40



test_pred <- neuralnet(type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology + Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways + Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points + Wilderness_Area1 + Wilderness_Area2 + Wilderness_Area3 + Wilderness_Area4
                       + Soil_Type1 + Soil_Type2 + Soil_Type3 + Soil_Type4 + Soil_Type5 + Soil_Type6 + Soil_Type7+ Soil_Type8 + Soil_Type9 + Soil_Type10 + Soil_Type11 + Soil_Type12 + Soil_Type13 + Soil_Type14 + Soil_Type15 + Soil_Type16 + Soil_Type17 + Soil_Type18 + Soil_Type19 + Soil_Type20 + Soil_Type21 + Soil_Type22 + Soil_Type23 + Soil_Type24 + Soil_Type25 
                       + Soil_Type26 + Soil_Type27 + Soil_Type28 + Soil_Type29 + Soil_Type30 + Soil_Type31 + Soil_Type32 + Soil_Type33 + Soil_Type34 + Soil_Type35 + Soil_Type36 + Soil_Type37 + Soil_Type38 + Soil_Type39 + Soil_Type40, data = train)



result <- compute(test_pred, test)
table(result$net.result)
# 3.99999976332137 
# 565892 

#Checking if soil type is 1 

count <- 0

for(i in 1:15120)
{
  for(j in 15:54) 
{
  if(train[i, colnames(train)[j]] == 1)
    count <- count + 1
}

}
# count
# [1] 15120 

count <- 0

for(i in 1:565892)
{
  for(j in 15:54) 
  {
    if(test[i, colnames(test)[j]] == 1)
      count <- count + 1
  }
  
}
# count 
# [1] 565892

#I was so right ;) 

#we can collapse the 40 rows and keep just the one row as soil_type and give the 
#numbers from 1 to 40 according to the soil type


for(i in 1:565892)
{
  for(j in 15:54) 
  {
    if(test[i, colnames(test)[j]] == 1)
      test$soil_type[i] <- (j-14)
  }
}


for(i in 1:15120)
{
  for(j in 15:54) 
  {
    if(train[i, colnames(train)[j]] == 1)
      train$soil_type[i] <- (j-14)
  }
}

table(train$soil_type)
#  1    2    3    4    5    6    8    9   10   11   12   13   14   16   17   18   19 
# 355  623  962  843  165  650    1   10 2142  406  227  476  169  114  612   60   46 
# 20   21   22   23   24   25   26   27   28   29   30   31   32   33   34   35   36 
# 139   16  345  757  257    1   54   15    9 1291  725  332  690  616   22  102   10 
# 37   38   39   40 
# 34  728  657  459 

table(test$soil_type)

#1      2      3      4      5      6      7      8      9     10     11     12 
#2676   6902   3861  11553   1432   5925    105    178   1137  30492  12004  29744 
#13     14     15     16     17     18     19     20     21     22     23     24 
#16955    430      3   2731   2810   1839   3975   9120    822  33028  56995  21021 
#25     26     27     28     29     30     31     32     33     34     35     36 
#473   2535   1071    937 113956  29445  25334  51829  44538   1589   1789    109 
#37     38     39     40 
#264  14845  13149   8291 


table(train$Soil_Type1)
#   0     1 
# 14765   355 

train <- train[-(15:54)]
test <- test[-(15:54)]


#Lets check for the wilderness area as well
for(i in 1:15120)
{
  for(j in 11:14) 
  {
    if(train[i, colnames(train)[j]] == 1)
      count <- count + 1
  }
}



for(i in 1:565892)
{
  for(j in 11:14) 
  {
    if(test[i, colnames(test)[j]] == 1)
      count <- count + 1
  }
}


for(i in 1:565892)
{
  for(j in 11:14) 
  {
    if(test[i, colnames(test)[j]] == 1)
      test$wilderness_type[i] <- (j-10)
  }
}


for(i in 1:15120)
{
  for(j in 11:14) 
  {
    if(train[i, colnames(train)[j]] == 1)
      train$wilderness_type[i] <- (j-10)
  }
}

table(train$wilderness_type)
#  1    2    3    4 
# 3597  499 6349 4675 

table(test$wilderness_type)
#  1      2      3      4 
# 257199  29385 247015  32293 

table(train$Wilderness_Area1)
#  0     1 
# 11523  3597 

table(test$Wilderness_Area1)
#   0      1 
# 308693 257199 

train <- train[-(11:14)]
test <- test[-(11:14)]

train$wilderness_type <- as.factor(train$wilderness_type)
test$wilderness_type <- as.factor(test$wilderness_type)

train$soil_type <- as.factor(train$soil_type)
test$soil_type <- as.factor(test$soil_type)



train$wilderness_type <- as.integer(train$wilderness_type)
test$wilderness_type <- as.integer(test$wilderness_type)

train$soil_type <- as.integer(train$soil_type)
test$soil_type <- as.integer(test$soil_type)

train$soil_type[500] <- 7
train$soil_type[501] <- 15

tree_pred <- ctree(type ~ ., data =train)
result <- round(predict(tree_pred, test))

table(result)
#  1      2      3      4      5      6      7 
# 125957 245940  47389  58871  47740  16157  23838 


sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, result)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction14.csv", sep = ",", quote = TRUE, row.names = FALSE)


#Knn with k =10 

test_pred <- knn(train = train, test = test, cl=type, k =10)
table(test_pred)
#   1      2      3      4      5      6      7 
# 178949 178165  40590   5095  71081  31383  60629 

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, test_pred)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction15.csv", sep = ",", quote = TRUE, row.names = FALSE)



test_pred <- knn(train = train, test = test, cl=type, k =1)
table(test_pred)

# 1      2      3      4      5      6      7 
#206431 218552  39986   2775  36499  25067  36582 

sampleSubmission <- sampleSubmission[-2]
sampleSubmission <- cbind(sampleSubmission, test_pred)
colnames(sampleSubmission)[2] <- "Cover_Type"

write.table (sampleSubmission, file = "prediction16.csv", sep = ",", quote = TRUE, row.names = FALSE)


test_pred <- knn(train = train, test = test, cl=type, k =120)
table(test_pred)

# 1      2      3      4      5      6      7 
#157813 114027  33384   9260 119368  29582 102458 

# Its pretty obvious from the data that it is not going to be an improvement on the score

