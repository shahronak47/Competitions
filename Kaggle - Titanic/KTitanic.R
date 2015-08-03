ktest <- read.csv("D:/My Folder/R/Kaggle - Titanic/test.csv", stringsAsFactors=FALSE)

ktrain <- read.csv("D:/My Folder/R/Kaggle - Titanic/train.csv", stringsAsFactors=FALSE)

labels <- ktrain[2]

ktrain <- ktrain[-2]
ktrain <- ktrain[-1]
ktrain <- ktrain[-2]
ktrain <- ktrain[-9]
ktrain <- ktrain[-8]
ktrain <- ktrain[-6]

ktest <- ktest[-11]
ktest <- ktest[-10]
ktest <- ktest[-8]
ktest <- ktest[-3]
ktest <- ktest[-1]

foreach(i = 1: nrow(ktrain)) %do%
{
  ktrain$Sex[i] <- ifelse(ktrain$Sex[i] == "male" , 1, 0)
  
}

foreach(i = 1: nrow(ktest)) %do%
{
  ktest$Sex[i] <- ifelse(ktest$Sex[i] == "male" , 1, 0)
  
}


summary(ktrain$Age)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.42   20.12   28.00   29.70   38.00   80.00     177 


#Lets fill the NA's with this mean value
foreach(i= 1:nrow(ktrain)) %do% 
{
  if(is.na(ktrain$Age[i]))
    ktrain$Age[i] <- 30
}

summary(ktest$Age)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.17   21.00   27.00   30.27   39.00   76.00      86 

foreach(i= 1:nrow(ktest)) %do% 
{
  if(is.na(ktest$Age[i]))
    ktest$Age[i] <- 30
}

summary(ktest)
summary(ktrain)


foreach(i= 1:nrow(ktest)) %do% 
{
  if(is.na(ktest$Fare[i]))
    ktest$Fare[i] <- 36
}


library(class)

cl <- labels[, 1]

test_pred<-knn(ktrain, ktest, cl, k=10)

gendermodel <- read.csv("D:/My Folder/R/Kaggle - Titanic/gendermodel.csv")
gendermodel <- gendermodel[-2]

gendermodel <- cbind(gendermodel, test_pred)
colnames (gendermodel) [2] <- "Survived"

write.table (gendermodel, file = "prediction.csv", sep = ",", quote = TRUE, row.names = FALSE)

#Well, KNN does not work here. Provides only 66.5 % accuracy