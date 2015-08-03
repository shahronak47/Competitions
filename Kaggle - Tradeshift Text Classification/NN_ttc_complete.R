train <- read.csv("D:/My Folder/R/Kaggle - Tradeshift Text Classification/train.csv", stringsAsFactors=FALSE)

library(rpart)

attach(train)

#Taking random variable x1 and splitting the dataset as it is too big

x <- split(train, train$x1) #Splitting according to yes no and blank
summary(x[1])

write.table (x[3], file = "split3.csv", sep = ",", quote = TRUE, row.names = FALSE)
#145762 rows for YES

table(train$x1)

split3 <- read.csv("D:/My Folder/R/Kaggle - Tradeshift Text Classification/split3.csv", stringsAsFactors=FALSE)

trainLabels <- read.csv("D:/My Folder/R/Kaggle - Tradeshift Text Classification/trainLabels.csv")

attach(split3)

library(grid)
library(MASS)
library(neuralnet)

label3 <- as.data.frame(split3$YES.id)
#taking out the id's of split3

colnames (label3)[1] <- "id"
#Renaming the column name to id so that we could compare it with trainLabels data frame

label3 <- merge(label3,trainLabels,by="id")
#All the columns in label 3 and trainLabels is merged with a common term id.

rm(trainLabels)

#PREDICTING FOR Y1

split3<-data.frame(split3, label3$y1)
colnames(split3)
colnames(split3)[147] <- "y1"
colnames(split3)

set.seed(123)
model3 <- neuralnet(formula = y1 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

test <- read.csv("D:/My Folder/R/Kaggle - Tradeshift Text Classification/test.csv", stringsAsFactors=FALSE)


model3_results <- compute(model3, test[6:10])
class(model3_results)

#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y1)
#   0      1 
# 145728     34 
# 34/145762 = 0.000233256
#So out of total records only 0.000233256 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.000233256 * 545082 = 127 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 127 rows

summary(predicted_values3)

predicted_values3[127,] #0.001239143713

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 127th row is 0.001239143713 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.001239143713, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final <- as.data.frame(convert_counts(predicted_values3))
colnames(final)[1] <- "y1"
table(final)

#   final
#     0      1 
#   544956    126 
#----------------------------------------------------------------------------------
#PREDICTING FOR Y2

split3<-data.frame(split3, label3$y2)
colnames(split3)
colnames(split3)[148] <- "y2"
colnames(split3)

set.seed(124)
model3 <- neuralnet(formula = y2 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y2)
#   0      1 
# 145753  9 
# 9/145762 = 0.0000617444
#So out of total records only 0.0000617444 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.0000617444 * 545082 = 34 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 34 rows

summary(predicted_values3)

predicted_values3[34,] #0.0006665782187

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 34th row is 0.0006665782187 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.0006665782187, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y2"
table(final1)

# final1
#   0      1 
#   545048     34 

final <- cbind(final, final1)

#----------------------------------------------------------------------------------
#PREDICTING FOR Y4

split3<-data.frame(split3, label3$y4)
colnames(split3)
colnames(split3)[150] <- "y4"
colnames(split3)

set.seed(126)
model3 <- neuralnet(formula = y4 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y4)
#    0      1 
# 145649    113
# 113/145649 = 0.0007752363
#So out of total records only 0.0007752363 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.0007752363 * 545082 = 423 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 423 rows

summary(predicted_values3)

predicted_values3[423,] #0.003304613344

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 423th row is 0.003304613344 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.003304613344, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y4"
table(final1)

# final1
#   0      1 
#  544666    416

#As was getting an error for y3 hence, making it as complete 0 here
#the error was R neuralnet does not converge within stepmax for time series
#After a bit of googling it seems that there is a problem with the data

final <- cbind(final, 0)
colnames (final)[3] <- "y3"
final <- cbind(final, final1)

#----------------------------------------------------------------------------------
#PREDICTING FOR Y5

split3<-data.frame(split3, label3$y5)
colnames(split3)
colnames(split3)[151] <- "y5"
colnames(split3)

set.seed(127)
model3 <- neuralnet(formula = y5 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y5)
#      0      1 
#  145750     12 
# 12/145762 = 0.00008232598
#So out of total records only 0.00008232598 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00008232598 * 545082 = 45 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 45 rows

summary(predicted_values3)

predicted_values3[45,] #0.0004588578461

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 45th row is 0.0004588578461 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.0004588578461, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y5"
table(final1)

# final1
#   0      1 
#  545038     44 

final <- cbind(final, final1)

#----------------------------------------------------------------------------------
#PREDICTING FOR Y8

split3<-data.frame(split3, label3$y8)
colnames(split3)
colnames(split3)[154] <- "y8"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y8 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y8)
#      0      1 
#  145642    120 
# 120/145762 = 0.00082325983
#So out of total records only 0.00082325983 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00082325983 * 545082 = 449 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 45 rows

summary(predicted_values3)

predicted_values3[449,] #0.00290836296

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 449th row is 0.00290836296 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.00290836296, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y8"
table(final1)

# final1
#   0         1 
#  544635    447

#As was getting an error for y6, y7 hence, making it as complete 0 here

final <- cbind(final, 0)
colnames (final)[6] <- "y6"
final <- cbind(final, 0)
colnames (final)[7] <- "y7"
final <- cbind(final, final1)


#----------------------------------------------------------------------------------
#PREDICTING FOR Y9

split3<-data.frame(split3, label3$y9)
colnames(split3)
colnames(split3)[155] <- "y9"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y9 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y9)
#      0      1 
#  138403   7359 
# 7359/145762 = 0.05048640935
#So out of total records only 0.05048640935 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.05048640935 * 545082 = 27519 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 27519 rows

summary(predicted_values3)

predicted_values3[27519,] #0.1367836515

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 27519th row is 0.1367836515 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.1367836515, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y9"
table(final1)

# final1
#   0         1 
#  319363 225719 


final <- cbind(final, final1)

#----------------------------------------------------------------------------------
#PREDICTING FOR Y10

split3<-data.frame(split3, label3$y10)
colnames(split3)
colnames(split3)[156] <- "y10"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y10 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y10)
#      0      1 
#  143964   1798 
# 1798/145762 = 0.01233517652
#So out of total records only 0.01233517652 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.01233517652 * 545082 = 6724 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 6724 rows

summary(predicted_values3)

predicted_values3[6724,] #0.03945082328

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 6724th row is 0.03945082328 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.03945082327, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y10"
table(final1)

# final1
#   0         1 
#  449668  95414 


final <- cbind(final, final1)

#----------------------------------------------------------------------------------
#PREDICTING FOR Y11

split3<-data.frame(split3, label3$y11)
colnames(split3)
colnames(split3)[157] <- "y11"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y11 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y11)
#      0      1 
#  145215    547
# 547/145762 = 0.00375269274
#So out of total records only 0.00375269274 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00375269274 * 545082 = 2046 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 6724 rows

summary(predicted_values3)

predicted_values3[2046,] #0.02037921013

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 2046th row is 0.02037921013 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.02037921012, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y11"
table(final1)

# final1
#   0         1 
#  496039  49043 


final <- cbind(final, final1)


#----------------------------------------------------------------------------------
#PREDICTING FOR Y13

split3<-data.frame(split3, label3$y13)
colnames(split3)
colnames(split3)[159] <- "y13"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y13 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y13)
#      0      1 
#  145752     10 
# 10/145762 = 0.00006860498
#So out of total records only 0.00006860498 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00006860498 * 545082 = 37 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 37 rows

summary(predicted_values3)

predicted_values3[37,] #0.001306610517

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 37th row is 0.001306610517 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.001306610517, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y13"
table(final1)

# final1
#   0         1 
#  545046     36 
final <- cbind(final, 0)
colnames (final)[12] <- "y12"

final <- cbind(final, final1)


#----------------------------------------------------------------------------------
#PREDICTING FOR Y14

split3<-data.frame(split3, label3$y14)
colnames(split3)
colnames(split3)[160] <- "y14"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y14 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y14)
#      0      1 
#  145752     0 
# 0/145762 = 0
#So out of total records only 0 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0 * 545082 = 0 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 37 rows



final <- cbind(final, 0)
colnames (final)[12] <- "y14"

#----------------------------------------------------------------------------------
#PREDICTING FOR Y15

split3<-data.frame(split3, label3$y15)
colnames(split3)
colnames(split3)[161] <- "y15"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y15 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y15)
#      0      1 
#  145740     22 
# 22/145762 = 0.00015093096
#So out of total records only 0.00015093096 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00015093096 * 545082 = 82 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 82 rows

summary(predicted_values3)

predicted_values3[82,] #0.0001511430041

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 82nd row is 0.0001511430041 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.0001511430041, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y15"
table(final1)

# final1
#   0         1 
#  545000     82 


final <- cbind(final, final1)

#----------------------------------------------------------------------------------
#PREDICTING FOR Y16

split3<-data.frame(split3, label3$y16)
colnames(split3)
colnames(split3)[162] <- "y16"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y16 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y16)
#      0      1 
#  145759      3 
# 3/145762 = 0.00002058149
#So out of total records only 0.00002058149 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00002058149 * 545082 = 11 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 11 rows

summary(predicted_values3)

predicted_values3[11,] #0.001889067467

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 11th row is 0.001889067467 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.001889067467, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y16"
table(final1)

# final1
#   0         1 
#  545070     12 


final <- cbind(final, final1)


#----------------------------------------------------------------------------------
#PREDICTING FOR Y17

split3<-data.frame(split3, label3$y17)
colnames(split3)
colnames(split3)[163] <- "y17"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y17 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(label3$y17)
#      0       
#  145762      


predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))

final <- cbind(final, 0)
colnames (final)[17] <- "y17"

#----------------------------------------------------------------------------------
#PREDICTING FOR Y18


split3<-data.frame(split3, label3$y18)
colnames(split3)
colnames(split3)[164] <- "y18"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y18 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
head(predicted_values3)

table(label3$y18)
#      0      1 
#  145754      8 
# 8/145762 = 0.00005488398
#So out of total records only 0.00005488398 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00005488398 * 545082 = 30 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 30 rows

head(predicted_values3)

predicted_values3[30,] #0.0003509525489

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

#the 30th row is 0.0003509525489 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.0003509525489, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y18"
table(final1)

# final1
#   0         1 
#  545045     37 


final <- cbind(final, final1)

#----------------------------------------------------------------------------------
#PREDICTING FOR Y19


split3<-data.frame(split3, label3$y19)
colnames(split3)
colnames(split3)[165] <- "y19"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y19 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
head(predicted_values3)

table(label3$y19)

final <- cbind(final, 0)
colnames (final)[19] <- "y19"


final <- cbind(final, 0)
colnames (final)[20] <- "y20"

#PREDICTING FOR Y21


split3<-data.frame(split3, label3$y21)
colnames(split3)
colnames(split3)[166] <- "y21"
colnames(split3)

set.seed(129)
model3 <- neuralnet(formula = y21 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
head(predicted_values3)

table(label3$y21)
#      0      1 
#  145760      2 
# 2/145762 = 0.00001372099
#So out of total records only 0.00001372099 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00001372099 * 545082 = 8 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 8 rows



predicted_values3[8,] #0.0009060744913

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

head(predicted_values3)
#the 30th row is 0.0003509525489 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.0009060744913, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y21"
table(final1)

# final1
#   0         1 
#  545074      8 


final <- cbind(final, final1)

#PREDICTING FOR Y23


split3<-data.frame(split3, label3$y23)
colnames(split3)
colnames(split3)[168] <- "y23"
colnames(split3)


model3 <- neuralnet(formula = y23 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
head(predicted_values3)

table(label3$y23)
#      0      1 
#  145750     12 
# 12/145762 = 0.00008232598
#So out of total records only 0.00008232598 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00001372099 * 545082 = 45 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 45 rows



predicted_values3[45,] #0.0001683706523

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

head(predicted_values3)
#the 45th row is 0.0001683706523 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.0001683706523, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y23"
table(final1)

# final1
#   0         1 
#  545038     44 

final <- cbind(final, 0)
colnames (final)[20] <- "y22"

final <- cbind(final, final1)


#PREDICTING FOR Y24


split3<-data.frame(split3, label3$y24)
colnames(split3)
colnames(split3)[169] <- "y24"
colnames(split3)


model3 <- neuralnet(formula = y24 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
head(predicted_values3)

table(label3$y24)
#      0      1 
#  145743     19 
# 19/145762 = 0.00013034947
#So out of total records only 0.00013034947 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00013034947 * 545082 = 71 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 71 rows



predicted_values3[71,] #0.0001810165216

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

head(predicted_values3)
#the 71th row is 0.0001810165216 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.0001810165216, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y24"
table(final1)

# final1
#   0         1 
#  545069     13 


final <- cbind(final, final1)


#PREDICTING FOR Y27


split3<-data.frame(split3, label3$y27)
colnames(split3)
colnames(split3)[172] <- "y27"
colnames(split3)


model3 <- neuralnet(formula = y27 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
head(predicted_values3)

table(label3$y27)
#      0      1 
#  145710     52 
# 52/145762 = 0.00035674592
#So out of total records only 0.00035674592 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00035674592 * 545082 = 195 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 195 rows



predicted_values3[195,] #0.07168495217

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

head(predicted_values3)
#the 195th row is 0.07168495217 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.07168495217, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y27"
table(final1)

# final1
#   0         1 
#  537359   7723 

#final <- cbind(final, 0)
#colnames (final)[26] <- "y26"


final <- cbind(final, final1)


#PREDICTING FOR Y28


split3<-data.frame(split3, label3$y28)
colnames(split3)
colnames(split3)[173] <- "y28"
colnames(split3)


model3 <- neuralnet(formula = y28 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
head(predicted_values3)

table(label3$y28)
#      0      1 
#  145663     99 
# 52/145762 = 0.00067918936
#So out of total records only 0.00067918936 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00067918936 * 545082 = 370 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 370 rows



predicted_values3[370,] #0.1035987938

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

head(predicted_values3)
#the 370th row is 0.1035987938 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.1035987938, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y28"
table(final1)

# final1
#   0         1 
#  544703    379

#final <- cbind(final, 0)
#colnames (final)[26] <- "y26"


final <- cbind(final, final1)


#PREDICTING FOR Y29


split3<-data.frame(split3, label3$y29)
colnames(split3)
colnames(split3)[174] <- "y29"
colnames(split3)


model3 <- neuralnet(formula = y29 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
head(predicted_values3)

table(label3$y29)
#      0      1 
#  140375   5387
# 5387/145762 = 0.03695750607
#So out of total records only 0.03695750607 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.03695750607 * 545082 = 20145 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 20145 rows



predicted_values3[20145,] #0.4197635718

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

head(predicted_values3)
#the 20145th row is 0.4197635718 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.4197635718, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y29"
table(final1)

# final1
#   0         1 
#  463828  81254

#final <- cbind(final, 0)
#colnames (final)[26] <- "y26"


final <- cbind(final, final1)


#PREDICTING FOR Y30


split3<-data.frame(split3, label3$y30)
colnames(split3)
colnames(split3)[175] <- "y30"
colnames(split3)


model3 <- neuralnet(formula = y30 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
head(predicted_values3)

table(label3$y30)
#      0      1 
#  144472   1290 
# 1290/145762 = 0.00885004322
#So out of total records only 0.00885004322 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.00885004322 * 545082 = 4824 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 4824 rows



predicted_values3[4824,] #0.04953015432

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

head(predicted_values3)
#the 4824th row is 0.04953015432 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.04953015432, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y30"
table(final1)

# final1
#   0         1 
#  540258   4824 

#final <- cbind(final, 0)
#colnames (final)[26] <- "y26"


final <- cbind(final, final1)

#PREDICTING FOR Y33


split3<-data.frame(split3, label3$y33)
colnames(split3)
colnames(split3)[178] <- "y33"
colnames(split3)


model3 <- neuralnet(formula = y33 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)

plot(model3)

model3_results <- compute(model3, test[6:10])


#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values

predicted_values3 <- model3_results$net.result
head(predicted_values3)

table(label3$y33)
#      0      1 
#  77978 67784 
# 1290/145762 = 0.46503203852
#So out of total records only 0.46503203852 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.46503203852 * 545082 = 253481 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 253481 rows



predicted_values3[253481,] #0.4261629245

predicted_values3 <- model3_results$net.result
#Rearranging it in original order

head(predicted_values3)
#the 253481th row is 0.4261629245 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.4261629245, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}

final1 <- as.data.frame(convert_counts(predicted_values3))
colnames(final1)[1] <- "y33"
table(final1)

# final1
#   0         1 
#  291439 253643 

final <- cbind(final, 0)
colnames (final)[32] <- "y32"


final <- cbind(final, final1)

x <- final[1, ]
myset <- t(x)

for(i in 2:nrow(final))
{
  
  x<- final[i, ]
  myset<- rbind(myset, t(x))
}

colnames(myset) [0] <- "id_label"
colnames(myset) [1] <- "pred"

write.table (myset, file = "myset.csv", sep = ",", quote = TRUE, row.names = FALSE)

submit <- as.data.frame(sampleSubmission$id_label)

submit <- cbind(submit, myset[,1])

colnames(submit)[1] <- "id_label"
colnames(submit)[2] <- "pred"

write.table (submit, file = "submit.csv", sep = ",", quote = TRUE, row.names = FALSE)

# 3,6,7,12,17, 19, 20,22,25, 26,31,32 columns are zero