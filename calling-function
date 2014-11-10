library(twitteR)
library(plyr)
library(stringr)
#Alter 'n' to fetch required number of tweets
tweets = searchTwitter("mh17", n=200, lang = "en")
 
#From a lexical word file containing positive words 
pos = scan('positive-words.txt', what = 'character', comment.char = ';')
 
#From a lexical word file containing negative words
neg = scan('negative-words.txt', what = 'character', comment.char = ';')
 
source('sentiment.R')
tweets.text = laply(tweets, function(t)t$getText())
#Removing emoticons
tweets.text = iconv(tweets.text, "latin1", "UTF-8", sub='')
 
analysis = score.sentiment(tweets.text, pos, neg)
table(analysis$score)
mean(analysis$score)
hist(analysis$score)
#Just to show the amazing Trend function the twitteR package provides
trend = availableTrendLocations()
 
 
# Plotting a wordcloud (bag of words) from the tweets
tweets.text = laply(tweets, function(t)t$getText())
myCorpus = Corpus(VectorSource(tweets.text))
myCorpus = tm_map(myCorpus, PlainTextDocument)
myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords,
                  c(stopwords("SMART")))
myCorpus = tm_map(myCorpus, PlainTextDocument)
wordcloud(myCorpus, scale=c(4,0.5),
              min.freq = 6, max.words=40, rot.per = 0,
              colors = palette(c("#A5C5AF", "#AA7570", "#3D4048")))
