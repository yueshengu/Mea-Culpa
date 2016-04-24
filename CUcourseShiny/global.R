
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
#suppressMessages(library(tm))
#suppressMessages(library(SnowballC))
#suppressMessages(library(wordcloud))
#suppressMessages(library(data.table))

options(stringsAsFactors = F)


prof<-readRDS("www/prof_combined_sentiment.rds")
prof$profName<-factor(paste0(prof$first_name,' ',prof$last_name))



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




