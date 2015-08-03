raop_train <- read.csv("D:/My Folder/R/Kaggle - RAOP/raop_train.csv", stringsAsFactors=FALSE)

hu.liu.pos = scan('D:/My Folder/R/Kaggle - RAOP/opinion-lexicon-English/positive-words.txt',
                  what='character', comment.char=';')

hu.liu.neg = scan('D:/My Folder/R/Kaggle - RAOP/opinion-lexicon-English/negative-words.txt',
                  what='character', comment.char=';')

pos.words = c(hu.liu.pos, 'thanks', 'advance', 'favor', 'return', 'job', 'rent', 'month',
              'parent', 'mom', 'dad', 'paycheck', 'work', 'grocery', 'bill', 'semester',
              'fired', 'loan', 'forward')

neg.words = c(hu.liu.neg, 'girlfriend', 'gf', 'party','friend', 'craving', 'drinks',
              'drunk', 'beer', 'celebrate', 'wasted', 'wine')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence) #Replace full-stops, commas etc with a space
    sentence = gsub('[[:cntrl:]]', '', sentence) #Replace /n, /t, backspace with a space
    sentence = gsub('\\d+', '', sentence) #Replaces digits with a space
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+') #Splits every sentence into words
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list) #See 17 in R important
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores)
  return(scores.df)
}

mytext <- raop_train$request_text
myscore<-score.sentiment(mytext, pos.words, neg.words)


raop_train [, 33] <- myscore


raop_test <- read.csv("D:/My Folder/R/Kaggle - RAOP/raop_test.csv", stringsAsFactors=FALSE)

#Sentiment analysis for test file

mytext1 <- raop_test$request_text_edit_aware
myscore1 <-score.sentiment(mytext1, pos.words, neg.words)

raop_test [, 18] <- myscore1

rcvd <- raop_train$requester_received_pizza

raop_train<-data.frame(raop_train, rcvd)

library(rpart)

tree_model <- rpart(rcvd ~ score + requester_account_age_in_days_at_request + requester_days_since_first_post_on_raop_at_request + requester_number_of_comments_at_request + requester_number_of_comments_in_raop_at_request + requester_number_of_posts_at_request + requester_number_of_posts_on_raop_at_request + requester_number_of_subreddits_at_request + requester_upvotes_minus_downvotes_at_request + requester_upvotes_plus_downvotes_at_request + unix_timestamp_of_request + unix_timestamp_of_request_utc , data = raop_train)
plotcp(tree_model)
printcp(tree_model)
summary(tree_model)

plot(tree_model, uniform = T)
text(tree_model, use.n = T, cex=0.75)


tree_model2 <- prune(tree_model, cp = 0.012)
plot(tree_model2, uniform = T)
text(tree_model2, use.n = T, cex=0.75)

#No change in the output. the graph comes out to be same as in RAOPSemtiment