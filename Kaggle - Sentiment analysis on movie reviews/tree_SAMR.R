train <- read.delim("D:/My Folder/R/Kaggle  - Sentiment analysis on movie reviews/train.xlsx", stringsAsFactors=FALSE)
test <- read.delim("D:/My Folder/R/Kaggle  - Sentiment analysis on movie reviews/test.csv", stringsAsFactors=FALSE)

library(rpart)
library(tree)
library(plyr)
library(stringr)
table(train$Sentiment)
# 0     1     2     3     4 
# 7072 27273 79582 32927  9206 



pos = scan('D:/My Folder/R/Kaggle - RAOP/opinion-lexicon-English/positive-words.txt',
           what='character', comment.char=';')

neg = scan('D:/My Folder/R/Kaggle - RAOP/opinion-lexicon-English/negative-words.txt',
           what='character', comment.char=';')
train$Phrase <- tolower(train$Phrase)

final <- data.frame(matrix(ncol = 1, nrow = 156060))

for(i in 1:nrow(train))
{
  score = 2
  sentence = train$Phrase[i]
  word = str_split(sentence, '\\s+') #Splits every sentence into words
  
  # compare our words to the dictionaries of positive & negative terms
  
  pos.matches = match(word, pos)
  neg.matches = match(word, neg)
  
  # match() returns the position of the matched term or NA
  # we just want a TRUE/FALSE:
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  
  # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
  score = score + sum(pos.matches) - sum(neg.matches)
  final[i,] <- score
}


train <- cbind(train, final)

table(final)

# final
# 1      2      3 
# 1798 153224   1038 

table(train$Sentiment)

# 0     1     2     3     4 
# 7072 27273 79582 32927  9206 


model <- tree(train$Sentiment ~ ., method="class", data=train)

tree_pred = predict(model, test)
tree_pred = round(tree_pred)

table(tree_pred)
#tree_pred
#2 
#66292 


