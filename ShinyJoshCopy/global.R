
library('shiny')
require('rCharts')
library('shinydashboard')
library('plyr')
library('dygraphs')

######## ADDED ######
library(xts)
######## ADDED ######
#require(RJSONIO)
#library(leaflet)
#library(XML)
#library(rvest)
#library(dplyr)
#library(tidyr)
#library(hexbin)
#library(recommenderlab)
suppressMessages(library(tm))
#suppressMessages(library(SnowballC))
suppressMessages(library(wordcloud))
#suppressMessages(library(data.table))
library("RWeka")
library(ggplot2)

options(stringsAsFactors = F)
options(mc.cores=1)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))

colors<-c('grey','red','green')
names(colors)<-c('Neutral','Negative','Positive')

prof<-readRDS("www/prof_combined_sentiment.rds")
prof$profName<-factor(paste0(prof$first_name,' ',prof$last_name))
pos.words <- scan('www/positive-words.txt', what='character', comment.char=';')
neg.words <- scan('www/negative-words.txt', what='character', comment.char=';')

corpus <- Corpus(VectorSource(prof$workload_text))
docs <- tm_map(corpus, removePunctuation) 
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, c(stopwords("english")))
#docs <- tm_map(docs, stemDocument)  
docs <- tm_map(docs, PlainTextDocument)
tdm <- TermDocumentMatrix(docs)

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none') {
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence <- gsub("&amp", "", sentence)
    sentence <- gsub("[^[:alnum:]///' ]", "", sentence)
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores)
  return(scores.df)
}




