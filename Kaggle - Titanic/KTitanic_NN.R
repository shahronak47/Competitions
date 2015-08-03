library(neuralnet)
model <- neuralnet(formula = Survived ~ Pclass , data = train)

plot(model)

test$Pclass <- as.integer(test$Pclass)

model_results <- compute(model, test [2])

predicted_values <- model_results$net.result
class(predicted_values)
#[1] "matrix"

summary(predicted_values)
#Min.   :0.2428650  
#1st Qu.:0.2428650  
#Median :0.2428650  
#Mean   :0.3927363  
#3rd Qu.:0.6311456  
#Max.   :0.6311456 


table(predicted_values)
# predicted_values
#0.242864959844329 0.469748701811165 0.631145616094628 
#218                93               107 

foreach( i = 1:nrow(predicted_values)) %do% 
{
  myarray[i] <- ifelse(predicted_values[i] > 0.47, 1,0)
                    
}
class(myarray)
table(myarray)

#myarray
#0   1 
#311 107 

gendermodel <- cbind(gendermodel, myarray)
gendermodel <- gendermodel[-2]
colnames(gendermodel)[2] <- "Survived"

write.table (gendermodel, file = "prediction4.csv", sep = ",", quote = TRUE, row.names = FALSE)
#Gets to 65% accuracy, not a best entry

foreach(i = 1: nrow(train)) %do%
{
  train$Sex[i] <- ifelse(train$Sex[i] == "male" , 1, 0)
  
}
train$Sex <- as.integer(train$Sex)
#train$Age <- as.integer(train$Age)


foreach(i= 1:nrow(train)) %do% 
{
  if(is.na(train$Age[i]))
    train$Age[i] <- 30
}

model <- neuralnet(formula = Survived ~ Pclass + Sex + SibSp + Parch + Age + Fare  , data = train)

foreach(i = 1: nrow(test)) %do%
{
  test$Sex[i] <- ifelse(test$Sex[i] == "male" , 1, 0)
  
}

test <- test[-1]
test <- test[-2]
test <- test[-9]
test <- test[-8]
test <- test[-6]

test$Sex <- as.integer(test$Sex)

foreach(i= 1:nrow(test)) %do% 
{
  if(is.na(test$Age[i]))
    test$Age[i] <- 30
}

foreach(i= 1:nrow(test)) %do% 
{
  if(is.na(test$Fare[i]))
    test$Fare[i] <- 36
}

model_results <- compute(model, test)


predicted_values <- model_results$net.result
class(predicted_values)
#[1] "matrix"

summary(predicted_values)
#Min.   :0.1199006  
#1st Qu.:0.1199006  
#Median :0.1199009  
#Mean   :0.1334986  
#3rd Qu.:0.1199137  
#Max.   :0.9654497  


table(predicted_values > 0.12)

# predicted_values
#FALSE  TRUE 
#348    70 

foreach( i = 1:nrow(predicted_values)) %do% 
{
  myarray[i] <- ifelse(predicted_values[i] > 0.12, 1,0)
  
}
class(myarray)
table(myarray)

#myarray
#0   1 
#348  70 

gendermodel <- cbind(gendermodel, myarray)
gendermodel <- gendermodel[-2]
colnames(gendermodel)[2] <- "Survived"

write.table (gendermodel, file = "prediction5.csv", sep = ",", quote = TRUE, row.names = FALSE)
#Gets to 70% accuracy, not a best entry