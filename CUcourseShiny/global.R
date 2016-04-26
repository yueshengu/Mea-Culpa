
library('shiny')
require('rCharts')
library('shinydashboard')
library('plyr')
suppressMessages(library(tm))
suppressMessages(library(wordcloud))
library("RWeka")
library(ggplot2)
library(xts)
library('dygraphs')

source("classify_emotion.R")
source("classify_polarity.R")
source("create_matrix.R")

options(stringsAsFactors = F)
options(mc.cores=1)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))

colors<-c('grey','red','green')
names(colors)<-c('Neutral','Negative','Positive')

#prof<-readRDS("www/prof_combined_sentiment.rds")
#prof$profName<-factor(paste0(prof$first_name,' ',prof$last_name))
load('www/prof.RData')
load('www/course.RData')
pos.words <- scan('www/positive-words.txt', what='character', comment.char=';')
neg.words <- scan('www/negative-words.txt', what='character', comment.char=';')

profcorpus <- Corpus(VectorSource(prof$review_text))
profdocs <- tm_map(profcorpus, removePunctuation) 
# docs <- tm_map(docs, removeNumbers) 
profdocs <- tm_map(profdocs, tolower)
profdocs <- tm_map(profdocs, removeWords, c(stopwords("english")))
#docs <- tm_map(docs, stemDocument)  
profdocs <- tm_map(profdocs, PlainTextDocument)
# proftdm <- TermDocumentMatrix(profdocs)

coursecorpus <- Corpus(VectorSource(course$review_text))
coursedocs <- tm_map(coursecorpus, removePunctuation) 
# docs <- tm_map(docs, removeNumbers) 
coursedocs <- tm_map(coursedocs, tolower)
coursedocs <- tm_map(coursedocs, removeWords, c(stopwords("english")))
#docs <- tm_map(docs, stemDocument)  
coursedocs <- tm_map(coursedocs, PlainTextDocument)
# coursedocs <- TermDocumentMatrix(coursedocs)



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




