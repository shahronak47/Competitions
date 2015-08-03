train <- read.csv("D:/My Folder/R/Kaggle - Digit Recognizer/train.csv", stringsAsFactors=FALSE)

test <- read.csv("D:/My Folder/R/Kaggle - Digit Recognizer/test.csv", stringsAsFactors=FALSE)

n <- names(train)
f <- as.formula(paste("label ~", paste(n[!n %in% "label"], collapse = " + ")))

digit_classifier <- lm(f, data = train)

result <- round(predict.lm(digit_classifier, test))

summary(result)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -33.000   3.000   4.000   4.433   6.000  16.000 

summary(result < 0)

#   Mode   FALSE    TRUE    NA's 
# logical  27812     188       0 

i<- 0

library(foreach)

foreach(i= 1:28000) %do%
{
  if(result[i] < 0)
  {
    result[i] <- -result[i]
    result[i] <- result[i] %% 9
  }
}

  summary(result)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   3.000   4.000   4.449   6.000  16.000 

summary(result > 9)

# Mode   FALSE    TRUE    NA's 
# logical   27805     195       0

foreach(i= 1:28000) %do%
{
  if(result[i] > 9)
  {
    result[i] <- result[i] %% 9
  }
}

summary(result)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   3.000   4.000   4.387   6.000   9.000 

rf_benchmark <- read.csv("D:/My Folder/R/Kaggle - Digit Recognizer/rf_benchmark.csv", stringsAsFactors=FALSE)

rf_benchmark <- cbind(rf_benchmark, result)
rf_benchmark <- rf_benchmark[-2]
colnames(rf_benchmark) [2] <- "Label"

write.table (rf_benchmark, file = "prediction2.csv", sep = ",", quote = TRUE, row.names = FALSE)


