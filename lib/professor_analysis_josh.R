setwd("/Users/Josh/Documents/Spring2016/DataScience/Project5/finalproject-p5-team1/lib/")

#################### Load Libraries ##################

library("RJSONIO")
library("dplyr")

#################### Prepare Data ####################

professors <- fromJSON("../data/professors.json")
professors <- matrix(unlist(professors), ncol = 5, byrow = TRUE)
colnames(professors) <- c("first_name", "id", "last_name", "middle_name", "nugget")

prof_reviews <- fromJSON("../data/CS_profs.json")
prof_reviews <- matrix(unlist(prof_reviews), ncol = 6, byrow = TRUE)
colnames(prof_reviews) <- c("course_ids", "created", "id", "professor_ids", "review_text", "workload_text")

library(tm)
library(SnowballC)
library(RWeka)

combined<-readRDS("../data/prof_combined_sentiment.rds")
corpus <- Corpus(VectorSource(combined$workload_text))
docs <- tm_map(corpus, removePunctuation) 
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stemDocument)  
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
tdm <- TermDocumentMatrix(docs)
dtm <- DocumentTermMatrix(docs)

options(mc.cores=1)

#Tokenizer for n-grams and passed on to the term-document matrix constructor
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 3))
BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}

txtTdmBi <- TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer))
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)


terms<-findFreqTerms(txtTdmBi, lowfreq=5, highfreq=Inf)

#temp<-as.vector(combined$review_score)
#temp2<-combined$workload_score
#combined<-combined[,1:9]
#combined$review_score <- temp
#combined$workload_score <- temp2
#summary(combined)

###################   Sentiment  #############################
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

#############################################################


########### Shiny ###################

library(wordcloud)
library(tm)
library(SnowballC)
library(RWeka)
library(plyr)
library(ggplot2)
#wordcloud_rep <- repeatable(wordcloud)

output$sentiment_cloud <- renderPlot({
    combined<-readRDS("../data/prof_combined_sentiment.rds")
    corpus <- Corpus(VectorSource(combined$workload_text))
    docs <- tm_map(corpus, removePunctuation) 
    docs <- tm_map(docs, removeNumbers) 
    docs <- tm_map(docs, tolower)
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, stemDocument)  
    docs <- tm_map(docs, PlainTextDocument)
    tdm <- TermDocumentMatrix(docs)
    
    options(mc.cores=1)
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

    txtTdmBi <- TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer))
    m = as.matrix(txtTdmBi)
    v = sort(rowSums(m),decreasing=TRUE)
    d = data.frame(word = names(v),freq=v)
    wordcloud(words = d$word,freq = d$freq, scale=c(5,0.1), random.order = F, rot.per=0.35, colors=brewer.pal(8, "Dark2"))  
})

output$sentiment_bar_chart <- renderPlot({

  pos.words <- scan('../data/positive-words.txt', what='character', comment.char=';')
  neg.words <- scan('../data/negative-words.txt', what='character', comment.char=';')
  combined<-readRDS("../data/prof_combined_sentiment.rds")
  
    corpus <- Corpus(VectorSource(combined$review_text))
    docs <- tm_map(corpus, removePunctuation) 
    docs <- tm_map(docs, removeNumbers) 
    docs <- tm_map(docs, tolower)
    docs <- tm_map(docs, removeWords, c("take", "assignments", "every", "computer", "cs", "professor", "dont", "prof", "first", "programming"))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, stemDocument)  
    docs <- tm_map(docs, PlainTextDocument)
    tdm <- TermDocumentMatrix(docs)
    
    options(mc.cores=1)
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

    txtTdmBi <- TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer))
    m = as.matrix(txtTdmBi)
    v = sort(rowSums(m),decreasing=TRUE)
    d = data.frame(word = names(v),freq=v)
    d <- score.sentiment(docs, pos.words, neg.words, .progress="text")
  
  d$score <- score.sentiment(d$word, pos.words, neg.words, .progress="text")
  d$sentiment <- rep(0)
  d$sentiment <- ifelse(d$score>=1, "Positive", d$sentiment)
  d$sentiment <- ifelse(d$score==0, "Neutral", d$sentiment)
  d$sentiment <- ifelse(d$score<=-1, "Negative", d$sentiment)

  subset(d[1:50,])%>%
    ggplot(aes(x=word, y=freq, fill=sentiment)) +
    geom_bar(stat="identity", colour="white") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) + ylab("Frequency") + xlab("Word") +
    scale_fill_manual(values=c("#990000", "#003399", "#339933"))

})

output$sentiment_bar_chart2 <- renderPlot({

    pos.words <- scan('../data/positive-words.txt', what='character', comment.char=';')
    neg.words <- scan('../data/negative-words.txt', what='character', comment.char=';')
    combined<-readRDS("../data/prof_combined_sentiment.rds")
  
    corpus <- Corpus(VectorSource(combined$review_text))
    docs <- tm_map(corpus, removePunctuation) 
    docs <- tm_map(docs, removeNumbers) 
    docs <- tm_map(docs, tolower)
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, stemDocument)  
    docs <- tm_map(docs, PlainTextDocument)
    tdm <- TermDocumentMatrix(docs)
    
    options(mc.cores=1)
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

    txtTdmBi <- TermDocumentMatrix(docs, control = list(tokenize = BigramTokenizer))
    m = as.matrix(txtTdmBi)
    v = sort(rowSums(m),decreasing=TRUE)
    d = data.frame(word = names(v),freq=v)
  
  d$score <- score.sentiment(d$word, pos.words, neg.words, .progress="text")
  d$sentiment <- rep(0)
  d$sentiment <- ifelse(d$score>=1, "Positive", d$sentiment)
  d$sentiment <- ifelse(d$score==0, "Neutral", d$sentiment)
  d$sentiment <- ifelse(d$score<=-1, "Negative", d$sentiment)

  subset(d[1:50,])%>%
    ggplot(aes(x=word, y=freq, fill=sentiment)) +
    geom_bar(stat="identity", colour="white") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) + ylab("Frequency") + xlab("Word") +
    scale_fill_manual(values=c("#990000", "#003399", "#339933"))

})

########################## Emotions ##################################

source("classify_emotion.R")
source("classify_polarity.R")
source("create_matrix.R")

# classify emotion
class_emo = classify_emotion(combined$workload_text, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(combined$workload_text, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=combined$workload_text, emotion=emotion,
polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
  emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

#####

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
labs(x="emotion categories", y="number of tweets")

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
geom_bar(aes(y=..count.., fill=polarity)) +
scale_fill_brewer(palette="RdGy") +
labs(x="polarity categories", y="number of tweets")

# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
   tmp = combined$workload_text[emotion == emos[i]]
   emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
emo.docs = removeWords(emo.docs, c("group", "final.", "final", "disgusting"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
   scale = c(2,.5), random.order = FALSE, title.size = 1.5)


