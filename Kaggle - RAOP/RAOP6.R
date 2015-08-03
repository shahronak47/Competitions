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
{
  for(j in 1:17)
  {
    if(abc[i] == xyz[j])
    {
      cols[i] <-  abc[i]   
      
    }
  }
}

head(cols)
## [1] "giver_username_if_known" "FALSE"                   "FALSE"                  
## [4] "FALSE"                   "request_id"              "FALSE" 
sum(is.na(cols))
# [1] 15

x<-!(is.na(cols))
sum(x==TRUE)
# [1] 17

#making the column names same as test data

raop_train <- raop_train[x==TRUE]

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


cv_tree = cv.tree(tree_model)

names(cv_tree)
plot(cv_tree$size, cv_tree$dev, type="b")

#We get minimum deviance at size =5, so pruning at that position

pruned_model = prune.tree(tree_model, best=4)
plot(pruned_model)
text(pruned_model)


tree_pred = predict(pruned_model, raop_test)
table(tree_pred > 0.2057)

# FALSE  TRUE 
#  324  1307 


convert_counts <- function(x) {
  x <- ifelse(x > 0.2057, 1, 0)
  x <- factor(x, levels = c (0, 1))
  return(x)
}
tree_boolean<- convert_counts(tree_pred)
table(tree_boolean)

write.table (tree_boolean, file = "Final6.csv", sep = ",", quote = TRUE, row.names = FALSE)

# tree_pred gives different probabilities of 1631 variables. Here selecting the 
# threshold  as 0.2057. So anything above 0.2057 is considered as pizza received.

#This is not an improvement on RAOP5. Hence, prunnning does not help here.
