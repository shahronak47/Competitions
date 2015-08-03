train <- read.delim("D:/My Folder/R/Kaggle  - Sentiment analysis on movie reviews/train.csv", stringsAsFactors=FALSE)

train$Phrase <- tolower(train$Phrase)

pos = scan('D:/My Folder/R/Kaggle - RAOP/opinion-lexicon-English/positive-words.txt',
                  what='character', comment.char=';')

neg = scan('D:/My Folder/R/Kaggle - RAOP/opinion-lexicon-English/negative-words.txt',
                  what='character', comment.char=';')

test <- read.delim("D:/My Folder/R/Kaggle  - Sentiment analysis on movie reviews/test.csv", stringsAsFactors=FALSE)
test$Phrase <- tolower(test$Phrase)


library(plyr)
library(stringr)


final <- data.frame(matrix(ncol = 1, nrow = 66292))

for(i in 1:nrow(test))
{
  score = 2
  sentence = test$Phrase[i]
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



table(train$Sentiment)

# 0     1     2     3     4 
# 7072 27273 79582 32927  9206 

final <- cbind(test$PhraseId, final)

colnames(final)[1] <- "PhraseId"
colnames(final)[2] <- "Sentiment"

write.table (final, file = "final1.csv", sep = ",", quote = TRUE, row.names = FALSE)
