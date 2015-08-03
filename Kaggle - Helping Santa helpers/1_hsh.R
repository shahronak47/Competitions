toys_rev1 <- read.csv("D:/My Folder/R/Kaggle - Helping Santa helpers/toys_rev1.csv", stringsAsFactors=FALSE)

sampleSubmission <- read.csv("D:/My Folder/R/Kaggle - Helping Santa helpers/sampleSubmission_rev1.csv", stringsAsFactors=FALSE)

#Seperating dates and time data

sapply(gregexpr("\\W+", toys_rev1$Arrival_time[1]), length) + 1
#[1] 5
sapply(gregexpr("\\W+", toys_rev1$Arrival_time[5]), length) + 1
#[1] 5

library(stringr)
#Time format is 'YYYY MM DD HH MM'
install.packages("chron")
library(chron)

word <- str_split(toys_rev1$Arrival_time[1], '\\s+') 
words = as.integer(unlist(word))
toys_rev1$date[1] <- as.Date(paste(words[1:3], collapse = '-'))
x <- paste(words[4:5], collapse = ':')
x <- paste(c(x, 0), collapse = ':')
y[1] <- chron(times = x)


for(i in 2:nrow(toys_rev1))
{
  word <- str_split(toys_rev1$Arrival_time[i], '\\s+') 
  words = as.integer(unlist(word))
  toys_rev1$date[i] <- as.Date(paste(words[1:3], collapse = '-'))
  x <- paste(words[4:5], collapse = ':')
  x <- paste(c(x, 0), collapse = ':')
  y[i] <- chron(times = x)  
}

