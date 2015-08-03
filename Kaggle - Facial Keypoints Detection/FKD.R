training <- read.csv("D:/My Folder/R/Kaggle - Facial Keypoints Detection/training.csv", stringsAsFactors=FALSE)

test <- read.csv("D:/My Folder/R/Kaggle - Facial Keypoints Detection/test.csv", stringsAsFactors=FALSE)

#counting the number of pixels in the image id

sapply(gregexpr("\\W+", training$Image[1]), length) + 1
#[1] 9216
sapply(gregexpr("\\W+", training$Image[4]), length) + 1
#[1] 9216
sapply(gregexpr("\\W+", training$Image[18]), length) + 1
#[1] 9216
sapply(gregexpr("\\W+", training$Image[99]), length) + 1
#[1] 9216

#So it is 96 * 96 = 9216 value at each pixel

summary(training)
#Too many NA values
#Replacing it with mean

library(foreach)

foreach(i = 1:nrow(training)) %do%
{
  foreach(j = 1:ncol(training)) %do%
  {
    if(is.na(training[i, j]))
    {
      training[i,j] <- mean(training[, j], na.rm = T)
    }
  }
}


summary(training)
#Wow!! I'm, awesome! This has done the trick. No NA values



sapply(gregexpr("\\W+", test$Image[1]), length) + 1
#[1] 9216
sapply(gregexpr("\\W+", test$Image[4]), length) + 1
#[1] 9216
sapply(gregexpr("\\W+", test$Image[18]), length) + 1
#[1] 9216
sapply(gregexpr("\\W+", test$Image[99]), length) + 1
#[1] 9216


library(stringr)

word <- str_split(training$Image[1], '\\s+') 
words = as.integer(unlist(word))
px <- as.data.frame(matrix(words, 1, 9216))
pixel <- px

for(i in  2:7049)
{
  word <- str_split(training$Image[i], '\\s+') 
  words = as.integer(unlist(word))
  px <- as.data.frame(matrix(words, 1, 9216))
  pixel <- rbind(pixel, px)
}

#data <- data.frame(0, nrow = 1, ncol = 9216)

#Renaming the columns
library(foreach)

foreach( j= 1:9216) %do% 
{
  colnames(pixel)[j] <- paste("l_", j, sep="")
}

#Now lets do the same for test data


word <- str_split(test$Image[1], '\\s+') 
words = as.integer(unlist(word))
px <- as.data.frame(matrix(words, 1, 9216))
pixel_test <- px

for(i in  2:1783)
{
  word <- str_split(test$Image[i], '\\s+') 
  words = as.integer(unlist(word))
  px <- as.data.frame(matrix(words, 1, 9216))
  pixel_test <- rbind(pixel_test, px)
}

#Renaming the columns


foreach( j= 1:9216) %do% 
{
  colnames(pixel_test)[j] <- paste("l_", j, sep="")
}


training <- training[-31]

train <- cbind(training, pixel)

# tree_pred <- rpart(train[, 1:30] ~ train[, 31:9216], data = train) 
#This doesn't work

#tree_pred <- rpart(left_eye_center_x + left_eye_center_y  ~ l_1 + l_2, data = train)
#This works

#tree_pred <- rpart(train[, 1] ~ l_1 + l_2, data = train)
#This works too :O

#tree_pred <- rpart(train[, 1] ~ train[,2:3], data = train)
#This doesn't work

#tree_pred <- rpart(train[, 1:30] ~ train[,2:3], data = train)
#This doesn't work


count <- 0

for(i in 1:nrow(pixel))
{
  for(j in 1:ncol(pixel))
  {
    if(is.na(pixel[i,j]))
      count <- count + 1
  }
}

#count remains as 0, hence confirming that there are non NA values

#columns_target <- paste(colnames(train)[1:30], collapse = "+")
columns_features <- paste(colnames(train)[31:50], collapse = "+ ")
# columns_features <- print.data.frame(data.frame(columns_features), quote = FALSE)
#gsub("["]", " ", columns_features)

formulas <- as.formula(train$left_eye_center_x ~ columns_features )

formulas <- as.formula(paste("train$left_eye_center_x ~", paste(colnames(train)[31:9246], collapse = "+")))

library(party)
library(rpart)
library(tree)

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
# [1] 1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "left_eye_center_x")
  {
    count <- count + 1
  }
    
}

#  count
# [1] 1782


i <- 1
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}



formulas <- as.formula(paste("train$left_eye_center_y ~", paste(colnames(train)[31:9246], collapse = "+")))



tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "left_eye_center_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 1782


i <- 2
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}



formulas <- as.formula(paste("train$right_eye_center_x ~", paste(colnames(train)[31:9246], collapse = "+")))



tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "right_eye_center_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 1782


i <- 3
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}



formulas <- as.formula(paste("train$right_eye_center_y ~", paste(colnames(train)[31:9246], collapse = "+")))



tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "right_eye_center_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 1782


i <- 4
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}




formulas <- as.formula(paste("train$left_eye_inner_corner_x ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "left_eye_inner_corner_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 588


i <- 5
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}



formulas <- as.formula(paste("train$left_eye_inner_corner_y ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "left_eye_inner_corner_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 588


i <- 6
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


formulas <- as.formula(paste("train$left_eye_outer_corner_x ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "left_eye_outer_corner_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 589


i <- 7
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}



formulas <- as.formula(paste("train$left_eye_outer_corner_y ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "left_eye_outer_corner_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 589


i <- 8
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


formulas <- as.formula(paste("train$right_eye_inner_corner_x ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "right_eye_inner_corner_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 589


i <- 9
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


formulas <- as.formula(paste("train$right_eye_inner_corner_y ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "right_eye_inner_corner_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 589


i <- 10
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}




formulas <- as.formula(paste("train$right_eye_outer_corner_x ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "right_eye_outer_corner_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 588


i <- 11
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


formulas <- as.formula(paste("train$right_eye_outer_corner_y ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "right_eye_outer_corner_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 588


i <- 12
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}




formulas <- as.formula(paste("train$left_eyebrow_inner_end_x ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "left_eyebrow_inner_end_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 585


i <- 13
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}



formulas <- as.formula(paste("train$left_eyebrow_inner_end_y ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "left_eyebrow_inner_end_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 585


i <- 14
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


formulas <- as.formula(paste("train$left_eyebrow_outer_end_x ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "left_eyebrow_outer_end_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 574


i <- 15
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


formulas <- as.formula(paste("train$left_eyebrow_outer_end_y ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "left_eyebrow_outer_end_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 574


i <- 16
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}




formulas <- as.formula(paste("train$right_eyebrow_inner_end_x  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "right_eyebrow_inner_end_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 585


i <- 17
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


formulas <- as.formula(paste("train$right_eyebrow_inner_end_y  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "right_eyebrow_inner_end_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 585


i <- 18
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


formulas <- as.formula(paste("train$right_eyebrow_outer_end_x  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "right_eyebrow_outer_end_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 572


i <- 19
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}




formulas <- as.formula(paste("train$right_eyebrow_outer_end_y  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "right_eyebrow_outer_end_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 572


i <- 20
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}

formulas <- as.formula(paste("train$nose_tip_x  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "nose_tip_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 1783


i <- 21
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}

formulas <- as.formula(paste("train$nose_tip_y  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "nose_tip_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 1783


i <- 22
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


formulas <- as.formula(paste("train$mouth_left_corner_x  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "mouth_left_corner_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 1783


i <- 23
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


formulas <- as.formula(paste("train$mouth_left_corner_y  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "mouth_left_corner_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 590


i <- 24
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}



formulas <- as.formula(paste("train$mouth_right_corner_x  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "mouth_right_corner_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 587


i <- 25
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


formulas <- as.formula(paste("train$mouth_right_corner_y  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "mouth_right_corner_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 587


i <- 26
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}

formulas <- as.formula(paste("train$mouth_center_top_lip_x  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "mouth_center_top_lip_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 590


i <- 27
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}




formulas <- as.formula(paste("train$mouth_center_top_lip_y  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "mouth_center_top_lip_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 590


i <- 28
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}






formulas <- as.formula(paste("train$mouth_center_bottom_lip_x  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "mouth_center_bottom_lip_x")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 1778


i <- 29
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}



formulas <- as.formula(paste("train$mouth_center_bottom_lip_y  ~", paste(colnames(train)[31:9246], collapse = "+")))

tree_pred <- rpart(formulas , data = train)
result <- predict(tree_pred, pixel_test)

sum(table(result))
#1783

count <- 0
for(i in  1:27124)
{
  if(IdLookupTable$FeatureName[i] == "mouth_center_bottom_lip_y")
  {
    count <- count + 1
  }
  
}

#  count
# [1] 1778


i <- 30
j <- 1

while(i < 27124)
{
  SampleSubmission$Location[i] <- result[j]
  i<- i + 30
  j<- j+1
}


write.table (SampleSubmission, file = "submit.csv", sep = ",", quote = TRUE, row.names = FALSE)
