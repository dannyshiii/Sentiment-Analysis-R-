# Sentiment Analysis by Danni Shi
# This initially reads tweets on Twitter
# Now I kinda generalized it
library(tidyverse)
library(stringr)
library(tidytext)
library(plyr)

text = read.table("text.txt") # read your txt file
# creat a function to calculate the sentiment score
score.sentiment = function(twt, pos.w, neg.w) {
  require(plyr)
  require(stringr)
  scores = laply(twt, function(twt, pos.w, neg.w) {
    # twt = gsub('http://','',twt) #temporarily removes links
    # twt = gsub('https://','',twt) #temporarily removes links
    twt = gsub('\\d+','',twt) #temporarily removes numbers
    # twt = gsub('[^[:graph:]]',' ',twt) #temporarily removes emojis
    twt = gsub('[[:punct:]]','',twt) #temporarily removes punctuation
    twt = gsub('[[:cntrl:]]','',twt) #temporarily removes control characters
    twt = tolower(twt) #makes all lowercase :)
    word.list = str_split(twt,'\\s+')
    words = unlist(word.list)
    pos.match = match(words,pos.w)
    neg.match = match(words,neg.w)
    pos.match = !is.na(pos.match) #to logistic variables
    neg.match = !is.na(neg.match)
    score = sum(pos.match)-sum(neg.match)
    return(score)
  }, pos.w, neg.w)
  scores.df = data.frame(score=scores, text=twt)
  return(scores.df)
}
neg = scan("negative-words.txt", what="character", comment.char=";")
pos = scan("positive-words.txt", what="character", comment.char=";")
senti_score = score.sentiment(text, pos, neg)
