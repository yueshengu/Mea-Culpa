
library('shiny')
#require('rCharts')
library('shinydashboard')
library('plyr')
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

options(stringsAsFactors = F)
options(mc.cores=1)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))


prof<-readRDS("www/prof_combined_sentiment.rds")
prof$profName<-factor(paste0(prof$first_name,' ',prof$last_name))

corpus <- Corpus(VectorSource(prof$workload_text))
docs <- tm_map(corpus, removePunctuation) 
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, c(stopwords("english")))
#docs <- tm_map(docs, stemDocument)  
docs <- tm_map(docs, PlainTextDocument)
tdm <- TermDocumentMatrix(docs)

createwc <- function(ASIN) {
  print(ASIN)
  if (ASIN %in% coll$product_productid & coll[coll$product_productid == ASIN,]$review_text != "NA"){
    revcorpus <- Corpus(VectorSource(coll[coll$product_productid == ASIN]$review_text))
    
    revcorpus <- tm_map(revcorpus, content_transformer(tolower))
    revcorpus <- tm_map(revcorpus, removePunctuation)
    revcorpus <- tm_map(revcorpus, PlainTextDocument)
    revcorpus <- tm_map(revcorpus, removeWords, c(stopwords('english'),'Movie','film','Film','movie',
                                                  'movies','films','Movies','Films'))
    
    # revcorpus <- tm_map(revcorpus, stemDocument)
    
    wordcloud(revcorpus, max.words = 50, random.order = FALSE,col=pal)
  }
  
}




