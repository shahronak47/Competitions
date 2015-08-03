library(neuralnet)
tree_model <- neuralnet(rcvd ~ number_of_words + request_length + requester_account_age_in_days_at_request + requester_days_since_first_post_on_raop_at_request + requester_number_of_comments_at_request + requester_number_of_comments_in_raop_at_request + requester_number_of_posts_at_request + requester_number_of_posts_on_raop_at_request + requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request + unix_timestamp_of_request + unix_timestamp_of_request_utc , raop_train)


model_results <- compute(tree_model, raop_test)


predicted_values <- model_results$net.result

summary(predicted_values)
#Min.   :0.1479383  
#1st Qu.:0.1479383  
#Median :0.1479383  
#Mean   :0.1479383  
#3rd Qu.:0.1479383  
#Max.   :0.1479383  

tree_model <- neuralnet(rcvd ~ number_of_words + request_length + requester_account_age_in_days_at_request + requester_days_since_first_post_on_raop_at_request + requester_number_of_comments_at_request + requester_number_of_comments_in_raop_at_request + requester_number_of_posts_at_request + requester_number_of_posts_on_raop_at_request + requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request + unix_timestamp_of_request + unix_timestamp_of_request_utc , norm_train)


model_results <- compute(tree_model, norm_test)


predicted_values <- model_results$net.result

summary(predicted_values)
# Min.   :0.1497431  
# 1st Qu.:0.3101378  
# Median :0.5649724  
# Mean   :0.5350759  
# 3rd Qu.:0.7491853  
# Max.   :0.8312872 


foreach(i= 1:1631) %do% 
{
  my[i] <- ifelse(predicted_values[i] > 0.57, 1,0)
}

table(my)
#  0    1 
#  823 808  



sampleSubmission <- cbind(sampleSubmission, my)
sampleSubmission <- sampleSubmission[-2]
colnames(sampleSubmission)[2] <- "requester_received_pizza"

write.table (sampleSubmission, file = "Final17.csv", sep = ",", quote = TRUE, row.names = FALSE)
