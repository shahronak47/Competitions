raop_train <- read.csv("D:/My Folder/R/Kaggle - RAOP/raop_train.csv", stringsAsFactors=FALSE)

raop_test <- read.csv("D:/My Folder/R/Kaggle - RAOP/raop_test.csv", stringsAsFactors=FALSE)


table(raop_train$requester_received_pizza)
# FALSE  TRUE 
# 3046   994 

rcvd <- raop_train$requester_received_pizza

#Fetching the common columns between two tables

abc<-names(raop_train)
xyz<-names(raop_test)

for(i in 1:32)
{
  for(j in 1:17)
  {
    if(abc[i] == xyz[j])
    {
      cols[i] <-  abc[i]   
      
    }
  }
}

x<-!(is.na(cols))

raop_train <- raop_train[x==TRUE]

raop_train<-data.frame(raop_train, rcvd)

library(foreach)


#Counting number of words in the request


foreach(i = 1:nrow(raop_train)) %do% 
{
  number_of_words[i] <- sapply(gregexpr("\\W+", raop_train$request_text_edit_aware[i]), length) + 1
}

foreach(i = 1:nrow(raop_test)) %do% 
{
  number_of_words_t[i] <- sapply(gregexpr("\\W+", raop_test$request_text_edit_aware[i]), length) + 1
}

raop_train <- cbind(raop_train, number_of_words)
raop_test <- cbind(raop_test, number_of_words_t)

colnames(raop_test)[18] <- "number_of_words"

#Counting number of subreddits

#foreach(i = 1:nrow(raop_train)) %do% 
#{
#  number_of_words[i] <- sapply(gregexpr("\\W+", raop_train$requester_subreddits_at_request[i]), length) + 1
#}

#foreach(i = 1:nrow(raop_test)) %do% 
#{
#  number_of_words_t[i] <- sapply(gregexpr("\\W+", raop_test$requester_subreddits_at_request[i]), length) + 1
#}




#raop_train <- cbind(raop_train, number_of_words)
#raop_test <- cbind(raop_test, number_of_words_t)

#colnames(raop_test)[19] <- "number_of_subreddits"
#colnames(raop_train)[20] <- "number_of_subreddits"
#There is already a subreddit field which does the same work

#actually for every 1 subreddit the number_of_subreddits is 2, so lets make it 1


library(tree)

tree_model <- tree(rcvd ~ number_of_words + number_of_subreddits + requester_account_age_in_days_at_request + requester_days_since_first_post_on_raop_at_request + requester_number_of_comments_at_request + requester_number_of_comments_in_raop_at_request + requester_number_of_posts_at_request + requester_number_of_posts_on_raop_at_request + requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request + unix_timestamp_of_request + unix_timestamp_of_request_utc , raop_train)
tree_pred = predict(tree_model, raop_test)

summary(tree_pred)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1953  0.1953  0.1953  0.2430  0.3151  0.7015 

table(rcvd)
# FALSE  TRUE 
# 3046   994 

#Nearly 25% of the total are true, taking 1953 as a benchmark

table(tree_pred > 0.1953)
# FALSE  TRUE 
# 1046   585 

my <- 0

foreach(i= 1:1631) %do% 
{
  my[i] <- ifelse(tree_pred[i] > 0.1953, 1,0)
}
table(my)
#  0    1 
# 1046  585 

sampleSubmission <- read.csv("D:/My Folder/R/Kaggle - RAOP/sampleSubmission.csv", stringsAsFactors=FALSE)

sampleSubmission <- cbind(sampleSubmission, my)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "requester_received_pizza"

write.table (sampleSubmission, file = "Final7.csv", sep = ",", quote = TRUE, row.names = FALSE)
# Best Score - 0.62744
# Final7 score - 0.58644
# No improvement on the best score

#now reducing some unnecessary parameters



tree_model <- tree(rcvd ~ number_of_words + requester_number_of_comments_in_raop_at_request + requester_number_of_posts_on_raop_at_request + requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request , raop_train)
tree_pred = predict(tree_model, raop_test)

summary(tree_pred)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1953  0.1953  0.1953  0.2430  0.3151  0.7015 

#Doesn't change so not going further
#applying rpart function

library(rpart)

tree_model <- rpart(rcvd ~ number_of_words + number_of_subreddits + requester_account_age_in_days_at_request + requester_days_since_first_post_on_raop_at_request + requester_number_of_comments_at_request + requester_number_of_comments_in_raop_at_request + requester_number_of_posts_at_request + requester_number_of_posts_on_raop_at_request + requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request + unix_timestamp_of_request + unix_timestamp_of_request_utc , raop_train)
tree_pred = predict(tree_model, raop_test)


summary(tree_pred)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1953  0.1953  0.1953  0.2430  0.3151  0.7015 


#Normalizing

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}  


str(raop_train)


raop_train <- raop_train[-(1:4)]
raop_train <- raop_train[-8]
raop_train <- raop_train[-10]
raop_train <- raop_train[-14]


str(raop_test)
raop_test <- raop_test[-(1:4)]
raop_test <- raop_test[-8]
raop_test <- raop_test[-10]
raop_test <- raop_test[-13]

library(rpart)

norm_train <- as.data.frame(lapply(raop_train, normalize))
norm_test <- as.data.frame(lapply(raop_test, normalize))

tree_model <- rpart(rcvd ~ number_of_words + requester_account_age_in_days_at_request + requester_days_since_first_post_on_raop_at_request + requester_number_of_comments_at_request + requester_number_of_comments_in_raop_at_request + requester_number_of_posts_at_request + requester_number_of_posts_on_raop_at_request + requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request + unix_timestamp_of_request + unix_timestamp_of_request_utc , norm_train)
tree_pred = predict(tree_model, norm_test)

summary(tree_pred)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1953  0.1953  0.1953  0.2350  0.1953  0.7015 

table(tree_pred)
# 0.195284872298625 0.315126050420168 0.701492537313433 
#   1323               236                72 

foreach(i= 1:1631) %do% 
{
  my[i] <- ifelse(tree_pred[i] > 0.4, 1,0)
}

table(my)
#  0    1 
# 1395  236

sampleSubmission <- read.csv("D:/My Folder/R/Kaggle - RAOP/sampleSubmission.csv", stringsAsFactors=FALSE)

sampleSubmission <- cbind(sampleSubmission, my)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "requester_received_pizza"

write.table (sampleSubmission, file = "Final13.csv", sep = ",", quote = TRUE, row.names = FALSE)




foreach(i= 1:1631) %do% 
{
  my[i] <- ifelse(tree_pred[i] < 0.4, 1,0)
}

table(my)
#  0    1 
#  236 1395 



sampleSubmission <- cbind(sampleSubmission, my)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "requester_received_pizza"

write.table (sampleSubmission, file = "Final14.csv", sep = ",", quote = TRUE, row.names = FALSE)


#Still poor performance but from the results it could be seen that normalizing the inputs isn't
#helping so lets denormalize again

#From timestamp converting to day of week

day_of_week[2] <- "abc"

foreach(i = 1:nrow(raop_train)) %do%
{
  day_of_week[i] <- weekdays(as.Date(as.POSIXct(raop_train$unix_timestamp_of_request_utc[i], origin="1970-01-01")))
}

raop_train <- cbind(raop_train, day_of_week)

day_of_week <- "abc"

foreach(i = 1:nrow(raop_test)) %do%
{
  day_of_week[i] <- weekdays(as.Date(as.POSIXct(raop_test$unix_timestamp_of_request_utc[i], origin="1970-01-01")))
}

raop_test <- cbind(raop_test, day_of_week)


#Let us check how many rcvd =1 for every weekday

rcvd <- as.logical(rcvd)
table(rcvd)
# FALSE  TRUE 
# 3046   994 

aggregate(rcvd, list(raop_train$day_of_week), FUN = table)

# Group.1 x.FALSE x.TRUE
#1    Friday     413    131
#2    Monday     421    153
#3  Saturday     401    121
#4    Sunday     450    135
#5  Thursday     386    164
#6   Tuesday     472    134
#7 Wednesday     503    156

table(raop_train$day_of_week)

#  Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
# 544       574       522       585       550       606       659 

#Doesn't show any significant difference on which we could do anything

summary(raop_train$number_of_words)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  2.00   37.00   61.00   80.29  101.00  906.00 

# Dividing number of words into 3 categories
# 1 - short , 2- medium , 3 - long  and the range for it is
# 1 - 2-40 2 - 40-100 3-100+

foreach(i = 1:nrow(raop_train)) %do% 
{
  if(raop_train$number_of_words[i] < 40)
    raop_train$request_length[i] <- 1
  else if(raop_train$number_of_words[i] >= 40 & raop_train$number_of_words[i] < 100)
    raop_train$request_length[i] <- 2
  else
    raop_train$request_length[i] <- 3
}

table(raop_train$request_length)
#  1    2    3 
# 1134 1858 1048 

summary(raop_test$number_of_words)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.00   35.00   63.00   80.12  102.00 1436.00 

foreach(i = 1:nrow(raop_test)) %do% 
{
  if(raop_test$number_of_words[i] < 40)
    raop_test$request_length[i] <- 1
  else if(raop_test$number_of_words[i] >= 40 & raop_train$number_of_words[i] < 100)
    raop_test$request_length[i] <- 2
  else
    raop_test$request_length[i] <- 3
}


tree_model <- rpart(rcvd ~ number_of_words + request_length + day_of_week + requester_account_age_in_days_at_request + requester_days_since_first_post_on_raop_at_request + requester_number_of_comments_at_request + requester_number_of_comments_in_raop_at_request + requester_number_of_posts_at_request + requester_number_of_posts_on_raop_at_request + requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request + unix_timestamp_of_request + unix_timestamp_of_request_utc , raop_train)
tree_pred = predict(tree_model, raop_test)

summary(tree_pred)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1953  0.1953  0.1953  0.2430  0.3151  0.7015 

table(tree_pred)
# 0.195284872298625 0.315126050420168 0.701492537313433
#   1046               565                20


foreach(i= 1:1631) %do% 
{
  my[i] <- ifelse(tree_pred[i] > 0.2, 1,0)
}

table(my)
#  0    1 
# 1046  585 


sampleSubmission <- cbind(sampleSubmission, my)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "requester_received_pizza"

write.table (sampleSubmission, file = "Final15.csv", sep = ",", quote = TRUE, row.names = FALSE)




library(party)

tree_model <- ctree(rcvd ~ number_of_words + request_length + day_of_week + requester_account_age_in_days_at_request + requester_days_since_first_post_on_raop_at_request + requester_number_of_comments_at_request + requester_number_of_comments_in_raop_at_request + requester_number_of_posts_at_request + requester_number_of_posts_on_raop_at_request + requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request + unix_timestamp_of_request + unix_timestamp_of_request_utc , raop_train)
tree_pred = predict(tree_model, raop_test)


summary(tree_pred)
#Min.   :0.08078  
#1st Qu.:0.19461  
#Median :0.19461  
#Mean   :0.23500  
#3rd Qu.:0.29115  
#Max.   :0.87500  

foreach(i= 1:1631) %do% 
{
  my[i] <- ifelse(tree_pred[i] > 0.20, 1,0)
}

table(my)
#  0    1 
#  1011  620 



sampleSubmission <- cbind(sampleSubmission, my)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "requester_received_pizza"

write.table (sampleSubmission, file = "Final16.csv", sep = ",", quote = TRUE, row.names = FALSE)



norm_train <- as.data.frame(lapply(raop_train, normalize))
norm_test <- as.data.frame(lapply(raop_test, normalize))


tree_model <- rpart(rcvd ~ number_of_words + request_length + requester_account_age_in_days_at_request + requester_days_since_first_post_on_raop_at_request + requester_number_of_comments_at_request + requester_number_of_comments_in_raop_at_request + requester_number_of_posts_at_request + requester_number_of_posts_on_raop_at_request + requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request + unix_timestamp_of_request + unix_timestamp_of_request_utc , norm_train)
tree_pred = predict(tree_model, norm_test)

summary(tree_pred)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.1953  0.1953  0.1953  0.2350  0.1953  0.7015 

table(tree_pred)
# 0.195284872298625 0.315126050420168 0.701492537313433
#    1323               236                72 


