attach(split3)

install.packages("neuralnet")
library(neuralnet)


#PREDICTING FOR Y1

split3<-data.frame(split3, label3$y1)
colnames(split3)[147] <- "y1"
model3 <- neuralnet(formula = y1 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)
class(model3)
plot(model3)

model3_results <- compute(model3, test[6:10])
class(model3_results)

#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values
set.seed(123)
predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(trainLabels$y1) #     0 -   1689631     1 - 10369
# 10369/1700000 =   0.0061. So out of total records only 0.0061 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.0061 * 545082 = 3325 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 3325 rows
summary(predicted_values3)

predicted_values3[3325,] #0.0004188730764
predicted_values3 <- model3_results$net.result
#Rearranging it in original order


#the 3325th row is 0.0004188730764 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.0004188730764, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}



finaly1 <- as.data.frame(convert_counts(predicted_values3))
colnames(finaly1)[1] <- "y1"
table(finaly1)
# finaly1
#  0      1 
# 541754   3328

#PREDICTING FOR Y2


split3<-data.frame(split3, label3$y2)
colnames(split3)
colnames(split3)[148] <- "y2"
colnames(split3)

model3 <- neuralnet(formula = y2 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)
class(model3)
plot(model3)

model3_results <- compute(model3, test[6:10])
class(model3_results)

#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values
set.seed(124)
predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(trainLabels$y2) #     0 -   1698871     1  - 1129
# 1129/1700000 =   0.0006. So out of total records only 0.0006 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.0006 * 545082 = 327 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 3325 rows
summary(predicted_values3)

predicted_values3[327,] # 0.0005287281282
predicted_values3 <- model3_results$net.result
#Rearranging it in original order


#the 327th row is 0.0005287281282 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.0005287281282, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}



finaly2 <- as.data.frame(convert_counts(predicted_values3))
colnames(finaly2)[1] <- "y2"
table(finaly2)
# finaly2
#   0      1 
# 544756    326 
finaly1 <- cbind(finaly1, finaly2)


#PREDICTING FOR Y3


split3<-data.frame(split3, label3$y3)
colnames(split3)
colnames(split3)[149] <- "y3"
colnames(split3)

model3 <- neuralnet(formula = y3 ~ YES.x5 + YES.x6 + YES.x7 + YES.x8 + YES.x9 ,data = split3)
plot(model3)

model3_results <- compute(model3, test[6:10])
class(model3_results)

#Compute function returns two values $neurons which stores neurons for each layer and 
# $net.results which stores predicted values
set.seed(124)
predicted_values3 <- model3_results$net.result
summary(predicted_values3)

table(trainLabels$y2) #     0 -   1698871     1  - 1129
# 1129/1700000 =   0.0006. So out of total records only 0.0006 % of records are 1
# rest all are zero in the train labels file
# Hence, for the test file 0.0006 * 545082 = 327 rows should be 1

predicted_values3 <- as.data.frame(sort(predicted_values3, decreasing = T))
#Sorting the data frame as we just need top 3325 rows
summary(predicted_values3)

predicted_values3[327,] # 0.0005287281282
predicted_values3 <- model3_results$net.result
#Rearranging it in original order


#the 327th row is 0.0005287281282 hence, all values above this is termed as 1

convert_counts <- function(x) {
  x <- ifelse(x > 0.0005287281282, 1, 0)
  x <- factor(x, levels = c (0, 1), labels = c ("0", "1"))
  return(x)
}



finaly2 <- as.data.frame(convert_counts(predicted_values3))
colnames(finaly2)[1] <- "y2"
table(finaly2)
# finaly2
#   0      1 
# 544756    326
