attach(train)

class(Sentiment)
## [1] "integer"

Sentiment <- factor(Sentiment)
str(train)

class(Sentiment)
## [1] "factor"
library(NLP)
library(tm)


#CLeaning Data for Train


traincorpus <- Corpus(VectorSource(Phrase)) #Corpus refers to a 
#collection of text document. Here, a document means a single text message
#Hence, it takes only text arguments(no numbers)

corpus_trainclean <- tm_map(traincorpus, content_transformer(tolower))



corpus_trainclean <- tm_map(corpus_trainclean, removeNumbers) #Removes numbers

corpus_trainclean <- tm_map(corpus_trainclean, removeWords, stopwords()) #remove the stopwords
#such as "to", "and" etc . Stopwords() has all of them

corpus_trainclean <- tm_map(corpus_trainclean, removePunctuation) #remove punctuation
#as well

corpus_trainclean <- tm_map(corpus_trainclean, stripWhitespace) #remove all the extra
#whitespaces leaving only single space between words


train_dtm <- DocumentTermMatrix(corpus_trainclean) #Creates token for every word
#and returns a sparse matrix. 


#Cleaning the test data

testcorpus <- Corpus(VectorSource(test$Phrase)) #Corpus refers to a 
#collection of text document. Here, a document means a single text message
#Hence, it takes only text arguments(no numbers)


corpus_testclean <- tm_map(testcorpus, content_transformer(tolower))

corpus_testclean <- tm_map(corpus_testclean, removeNumbers) #Removes numbers

corpus_testclean <- tm_map(corpus_testclean, removeWords, stopwords()) #remove the stopwords
#such as "to", "and" etc . Stopwords() has all of them

corpus_testclean <- tm_map(corpus_testclean, removePunctuation) #remove punctuation
#as well

corpus_testclean <- tm_map(corpus_testclean, stripWhitespace) #remove all the extra
#whitespaces leaving only single space between words



test_dtm <- DocumentTermMatrix(corpus_testclean) #Creates token for every word
#and returns a sparse matrix. 

#A word cloud is a way to visually depict the frequency at which words 
#appear in text data. Words appearing more often in the text are shown
# in a larger font,while less common terms are shown in smaller fonts


library(RColorBrewer)
library(wordcloud)

wordcloud(corpus_clean, min.freq = 40, random.order = FALSE)
#can be created by a corpus object
# As random.order = false, higher frequency object would be at the center
# The min.freq parameter specifies the number of times a word must appear 
# in the corpus  before it will be displayed in the cloud(roughly 10%)
# So it means that the word should appear in at least 40 docs

a0 <- subset(train, Sentiment == 0)
a1 <- subset(train, Sentiment == 1)
a2 <- subset(train, Sentiment == 2)
a3 <- subset(train, Sentiment == 3)
a4 <- subset(train, Sentiment == 4)

wordcloud(a0$Phrase, max.words = 40, scale = c(3, 0.5))
wordcloud(a1$Phrase, max.words = 40, scale = c(3, 0.5))
wordcloud(a2$Phrase, max.words = 40, scale = c(3, 0.5))
wordcloud(a3$Phrase, max.words = 40, scale = c(3, 0.5))
wordcloud(a4$Phrase, max.words = 40, scale = c(3, 0.5))

a0corpus <- Corpus(VectorSource(a0))
a0_dtm <- DocumentTermMatrix(a0corpus)
a0_dict <- findFreqTerms(a0_dtm, 10)

a1corpus <- Corpus(VectorSource(a1))
a1_dtm <- DocumentTermMatrix(a1corpus)
a1_dict <- findFreqTerms(a1_dtm, 10)

a2corpus <- Corpus(VectorSource(a2))
a2_dtm <- DocumentTermMatrix(a2corpus)
a2_dict <- findFreqTerms(a2_dtm, 10)


a3corpus <- Corpus(VectorSource(a3))
a3_dtm <- DocumentTermMatrix(a3corpus)
a3_dict <- findFreqTerms(a3_dtm, 10)


a4corpus <- Corpus(VectorSource(a4))
a4_dtm <- DocumentTermMatrix(a4corpus)
a4_dict <- findFreqTerms(a4_dtm, 10)


a0 <- DocumentTermMatrix(a0corpus,list(dictionary = a0_dict))
#Limiting the training set to only word which appear at least for 10 times

a1 <- DocumentTermMatrix(a1corpus,list(dictionary = a1_dict))
a2 <- DocumentTermMatrix(a2corpus,list(dictionary = a2_dict))
a3 <- DocumentTermMatrix(a3corpus,list(dictionary = a3_dict))
a4 <- DocumentTermMatrix(a4corpus,list(dictionary = a4_dict))

library(e1071)
classifier <- naiveBayes(train, train$Sentiment)

test_pred <- as.data.frame(predict(classifier, test))
