raop_train <- read.csv("D:/My Folder/R/Kaggle - RAOP/raop_train.csv", stringsAsFactors=FALSE)
View(raop_train)
raop_test <- read.csv("D:/My Folder/R/Kaggle - RAOP/raop_test.csv", stringsAsFactors=FALSE)
View(raop_test)


table(raop_train$giver_username_if_known=="N/A")
## FALSE  TRUE 
##   287  3753 
# Which means we know the names of 287 users


attach(raop_train)
rcvd <- raop_train$requester_received_pizza
table(requester_received_pizza)
## FALSE  TRUE 
##  3046   994
#Which means 994 people received pizza


#Fetching the common columns between two tables

abc<-names(raop_train)
xyz<-names(raop_test)

for(i in 1:32)
  for(j in 1:17)
{
  if(abc[i] == xyz[j])
  {
    cols[i] <-  abc[i]   
    
  }
}
head(cols)
## [1] "giver_username_if_known" "FALSE"                   "FALSE"                  
## [4] "FALSE"                   "request_id"              "FALSE" 
sum(cols==FALSE)
# [1] 15
sum(cols!=FALSE)
# [1] 17

#making the column names same as test data

raop_train <- raop_train[cols!=FALSE]

str(raop_train)

install.packages("party")
library(party)

#Adding the target variable to the training table
raop_train<-data.frame(raop_train, rcvd)

#Creating a decision tree with the target variable and a list of independent variables
tree_model <- tree(rcvd ~ requester_account_age_in_days_at_request + requester_days_since_first_post_on_raop_at_request + requester_number_of_comments_at_request + requester_number_of_comments_in_raop_at_request + requester_number_of_posts_at_request + requester_number_of_posts_on_raop_at_request + requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request + unix_timestamp_of_request + unix_timestamp_of_request_utc , raop_train)
plot(tree_model)
text(tree_model)

#To verify the plot function (not needed for the function)

for(i in 1:nrow(raop_train))
{
if(requester_number_of_posts_on_raop_at_request==0)
{
  x <- unix_timestamp_of_request
}
}
mean(x)

text(tree_model, pretty=0)

tree_pred = predict(tree_model, raop_test)
table(tree_pred > 0.5)
# FALSE  TRUE 
# 1611    20 


convert_counts <- function(x) {
  x <- ifelse(x > 0.5, 1, 0)
  x <- factor(x, levels = c (0, 1))
  return(x)
}
tree_boolean<- convert_counts(tree_pred)
table(tree_boolean)

write.table (tree_boolean, file = "Final.csv", sep = ",", quote = TRUE, row.names = FALSE)

# tree_pred gives different probabilities of 1631 variables. Here selecting the 
# threshold  as 0.5. So anything above 0.5 is considered as pizza received.
  