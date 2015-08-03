train <- read.csv("D:/My Folder/R/Teamleada - Titanic/train.csv", stringsAsFactors=FALSE)

attach(train)

table(Survived)
# Survived
#   0   1 
#  549 342 


#How many missing age values are in the Train dataset?
summary(Age)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.42   20.12   28.00   29.70   38.00   80.00     177 

# Rounded to a single digit what is the average age of the all of the passengers in the Train dataset?
mean(Age, na.rm = T)
# [1] 29.69912

#Lets fill the NA's with this mean value
foreach(i= 1:nrow(train)) %do% 
{
  if(is.na(train$Age[i]))
    train$Age[i] <- 30
}

#replace(train$Age, 6, 30)

write.table (train, file = "train.csv", sep = ",", quote = TRUE, row.names = FALSE)

#Visualization
#Visualizing survival rate according to Pclass

table(Pclass)
# Pclass
#   1   2   3 
#  216 184 491 

ptable <- data.frame(0,0)


colnames(ptable)[1] <- "PClass"
colnames(ptable)[2] <- "Survived"
colnames(ptable)[3] <- "Not_Survived"



ptable [, ] <- 0
ptable[1, 1] <- 1
ptable[2, 1] <- 2
ptable[3, 1] <- 3

train<-train[order(train$Pclass), ]

count <- c(216, 184, 491)


  for(j in 1:891)
  {
     if(train$Pclass[j] == 1)
     {
       if(train$Survived[j]==1)
         ptable[1, 2]<- ptable[1,2] + 1
       else
         ptable[1,3] <- ptable[1,3] + 1
     }
     else if(train$Pclass[j] == 2)
     {
       if(train$Survived[j]==1)
         ptable[2, 2]<- ptable[2,2] + 1
       else
         ptable[2,3] <- ptable[2,3] + 1
     }
     else 
     {
       if(train$Survived[j]==1)
         ptable[3, 2]<- ptable[3,2] + 1
       else
         ptable[3,3] <- ptable[3,3] + 1
     }
     
  }

#Plotting in the barplot


#plot(x = as.numeric(ptable$PClass), y = ptable$Survived, col = "blue", 
#     main = "Distribution across PClass", xlab = "PClass", ylab = "Count")


barplot(ptable$Survived, main = "Number of people survived", space = 4, 
        names.arg = c("P1", "P2", "P3"), xlab = "PClass", ylab = "Count"
        , density = 200, angle = 135, col= "darkblue")

    


barplot(ptable$Not_Survived, main = "Number of people not survived", space = 4, 
        names.arg = c("P1", "P2", "P3"), xlab = "PClass", ylab = "Count"
        , density = 200, angle = 135, col= "darkblue")


summary(train$Age)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.42   22.00   30.00   29.76   35.00   80.00 

table(Age <= 25)
# FALSE  TRUE 
# 613   301 

table(Age > 25 & Age <= 35 )
# FALSE  TRUE 
# 518   373
table(Age>35)
# FALSE  TRUE 
#  674   217 

sum(table(Age))
# [1] 891
#Lets divide the database in three Age sections 25, 35 and above 35
#Add a new column in the database to find out which section it belongs
train[, 13] <- NA
colnames(train)[13] <- "Section"


foreach(i = 1:891) %do%
{
  if(train$Age[i] <= 25)
    train$Section[i] <- "Young"
  else if(train$Age[i] > 25 & train$Age[i] <= 35)
    train$Section[i] <- "Mid Age"
  else
    train$Section[i] <- "Old"
}

table(train$Section)
#  Mid Age  Old   Young 
#   373     217     301 
#Hence, we can cross verify from above that the table has converted well


ptable1 <- data.frame(0,0)
ptable1[,3] <- 0
colnames(ptable1)[1] <- "Age Section"
colnames(ptable1)[2] <- "Survived"
colnames(ptable1)[3] <- "Not_Survived"



ptable1 [, ] <- 0
ptable1[1, 1] <- "Mid Age"
ptable1[2, 1] <- "Old"
ptable1[3, 1] <- "Young"


for(j in 1:891)
{
  if(train$Section[j] == "Mid Age")
  {
    if(train$Survived[j]==1)
      ptable1[1, 2]<- ptable1[1,2] + 1
    else
      ptable1[1,3] <- ptable1[1,3] + 1
  }
  else if(train$Section[j] == "Old")
  {
    if(train$Survived[j]==1)
      ptable1[2, 2]<- ptable1[2,2] + 1
    else
      ptable1[2,3] <- ptable1[2,3] + 1
  }
  else 
  {
    if(train$Survived[j]==1)
      ptable1[3, 2]<- ptable1[3,2] + 1
    else
      ptable1[3,3] <- ptable1[3,3] + 1
  }
  
}

#apply(ptable1,2, FUN(x)  x/891)

dat <- read.table(text = "  Not_Survived Survived
                               238        135
                               134        83
                               177        124  ", header = T)
                  
                  
                  
barplot(as.matrix(dat) , horiz = T, main = "Survival rate according to Age", 
        col = c("Blue", "Red", "Green"), legend.text = c("25 < Age > 35", "Age > 35", "Age < 25"))


model <- rpart(train$Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare , method="class", data=train)

tree_pred = predict(model, test)

my <- as.array(10)

foreach(i = 1: nrow(tree_pred)) %do%
{
  my[i] <- ifelse(tree_pred[i] >= 0.5  , 0, 1)
  
}

head(my)

# [1] 0 1 0 0 0 0
gendermodel <- gendermodel[-2]
gendermodel[,2] <- NA
colnames(gendermodel) [2] <- "Survived"

gendermodel <- cbind(gendermodel, my)



