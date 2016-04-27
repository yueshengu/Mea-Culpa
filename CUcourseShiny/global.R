
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
library(rjson)

source("classify_emotion.R")
source("classify_polarity.R")
source("create_matrix.R")

courseTags<-c('introduction','fundamental','algorithm','java','no prior programming background',
              'matlab','honors-level','model','data structure','array','stack','list','queue','tree',
              'graph','sort','search','hash','storage','practical','c++','unix','perl','web','logic',
              'proof','math','probability','relation','regular expression','theory','set','design',
              'processor','database','sql','xml','query','security','compiler','object-oriented','data',
              'script','code','language','operating system','memory','internet','network','software',
              'interactive','animation','physic','user interface','team','image','robot','machine learning',
              'support vector machine','neural network','kernel','regression','classification','advance',
              'parallel','cloud','mobile'
              )
#grepl(paste(c('java','no prior programming background'),collapse='|'),courseTags)

options(stringsAsFactors = F)
options(mc.cores=1)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))

colors<-c('grey','red','green')
names(colors)<-c('Neutral','Negative','Positive')

#prof<-readRDS("www/prof_combined_sentiment.rds")
#prof$profName<-factor(paste0(prof$first_name,' ',prof$last_name))
load('www/prof.RData')
load('www/course.RData')
course[course$name == "Fundamentals of Computer Systems",]$Description <- 
  "Fundamentals of computer organization and digital logic. Boolean algebra, Karnaugh maps, basic gates and components, flipflops and latches, counters and state machines, basics of combinational and sequential digital design. Assembly language, instruction sets, ALUs, single-cycle and multi-cycle processor design, introduction to pipelined processors, caches, and virtual memory."
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


# dat <- rjson::fromJSON(file="www/heatmap-data.json")
# dat <- lapply(dat, function(x) {
#   x[sapply(x, is.null)] <- NA
#   unlist(x)
# })
# dat <- cbind(rep(0:4,each=25),rep(0:24,5),c(rep(1,5),rep(0,120)))#matrix(dat$data, ncol=3, byrow=TRUE)
# colnames(dat) <- c("x","y","value")


