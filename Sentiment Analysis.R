# SENTIMENT ANALYSIS

library(readxl)
Sentiments <- read_excel("D:/MASON/1 Semester/AIT-580/Assignment/NLP/A.xls")
View(Sentiments)

install.packages("tm")
library(tm)

corpus <- VCorpus(VectorSource((Sentiments$Tweet)))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus,removeNumbers)
corpus <- tm_map(corpus,removePunctuation)

library("SnowballC")
corpus <- tm_map(corpus,removeWords,stopwords())
corpus <- tm_map(corpus,stemDocument)
corpus <- tm_map(corpus,stripWhitespace)

dtm <- DocumentTermMatrix(corpus)

#Removing less repeated words
dtm <- removeSparseTerms(dtm, 0.999)
dataset <- as.data.frame(as.matrix(dtm))
dataset$Rating <- Sentiments$Rating
dataset$Rating <- factor(dataset$Rating, levels = c(0,1))

library(caTools)
set.seed(123)
split <- sample.split(dataset$Rating, SplitRatio = 0.75)
train <- subset(dataset, split == TRUE)
test <- subset(dataset, split == FALSE)

library(randomForest)
classifier <- randomForest(x=train[-943], y = train$Rating, ntree = 10)
y_pred <- predict(classifier, newdata = test[-943])

cm <- table(test[,943], y_pred)


# Generating an empty table to input 10 tweets
tweettest <- dataset[dataset$Rating==2,]

TenTweets <- read_excel("D:/MASON/1 Semester/AIT-580/Assignment/NLP/TenTweets.xls")
TenTweets$id <- seq.int(nrow(TenTweets))

View(TenTweets)

library(dplyr)
library(tidytext)

tab <- data_frame(TenTweets$id, TenTweets$Tweet)
colnames(tab) <- c("id", "tweet")

tab <- tab %>% unnest_tokens(word, tweet)
tab <- tab %>% anti_join(stop_words)

tab$word <- stemDocument(tab$word)

for(i in 1:length(tab$id))
{
  x <- tab$id[i]
  for(j in 1:942)
  {
    if(tab$word[i] == colnames(tweettest[j]))
    {
      tweettest[x,j] <- c(1)
    }
    else
      tweettest[x,j] <- c(0)
  }
}

tweettest$Rating <- TenTweets$Rating

pred <- predict(classifier, newdata = tweettest[-943])

cm1 <- table(tweettest[,943], pred)
