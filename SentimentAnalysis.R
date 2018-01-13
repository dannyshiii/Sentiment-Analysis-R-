# Sentiment Analysis by Danni Shi
# This initially reads tweets on Twitter
# Now I kinda generalized it
library(tidyverse)
library(stringr)
library(tidytext)
library(plyr)

text = read.table("text.txt") # read your txt file
# creat a function to calculate the sentiment score
score.sentiment = function(text, pos.w, neg.w) {
  require(plyr)
  require(stringr)
  scores = laply(text, function(text, pos.w, neg.w) {
    # text = gsub('http://','',text) #temporarily removes links
    # text = gsub('https://','',text) #temporarily removes links
    text = gsub('\\d+','',text) #temporarily removes numbers
    # text = gsub('[^[:graph:]]',' ',text) #temporarily removes emojis
    text = gsub('[[:punct:]]','',text) #temporarily removes punctuation
    text = gsub('[[:cntrl:]]','',text) #temporarily removes control characters
    text = tolower(text) #makes all lowercase :)
    word.list = str_split(text,'\\s+')
    words = unlist(word.list)
    pos.match = match(words,pos.w)
    neg.match = match(words,neg.w)
    pos.match = !is.na(pos.match) #to logistic variables
    neg.match = !is.na(neg.match)
    score = sum(pos.match)-sum(neg.match)
    return(score)
  }, pos.w, neg.w)
  scores.df = data.frame(score=scores, text=text)
  return(scores.df)
}
neg = scan("negative-words.txt", what="character", comment.char=";")
pos = scan("positive-words.txt", what="character", comment.char=";")
senti_score = score.sentiment(text, pos, neg)
