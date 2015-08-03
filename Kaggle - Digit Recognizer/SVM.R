train <- read.csv("D:/My Folder/R/Kaggle - Digit Recognizer/train.csv", stringsAsFactors=FALSE)

test <- read.csv("D:/My Folder/R/Kaggle - Digit Recognizer/test.csv", stringsAsFactors=FALSE)

library(neuralnet)


n <- names(train)
f <- as.formula(paste("label ~", paste(n[!n %in% "label"], collapse = " + ")))

digit_classifier <- neuralnet(f, data = train)

predicted_values <-  compute(digit_classifier, test)

