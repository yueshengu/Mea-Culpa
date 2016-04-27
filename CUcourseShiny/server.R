options(shiny.maxRequestSize=50*1024^2)

shinyServer(function(input, output, session) {
  
  observe({
    firstCourseMatch<-unique(as.character(course$name[grepl(paste(input$courseTopics,collapse='|'),
                                                            tolower((course$Description)))|
                                                        grepl(paste(input$courseTopics,collapse='|'),
                                                              tolower((course$name)))]))
    updateSelectInput(session,"firstCourse","First Course:",choices=firstCourseMatch,
                      selected=firstCourseMatch[1])
    

  })
  
  
  profData<-reactive({
    #browser()
    profData<-prof[prof$profName==input$profName,]
    
    profDocs<-profdocs[prof$profName==input$profName]
    
    txtTdmBi <- as.matrix(TermDocumentMatrix(profDocs, control = list(tokenize = BigramTokenizer)))
    v = sort(rowSums(txtTdmBi),decreasing=TRUE)
    d = data.frame(word = names(v),freq=v)
    d<-d[!d$word%in%
           c('final','midterm','finals','midterms','assignment','assignments','problem','problems'),]
    if(nrow(d)>=30) d2<-d[1:30,]
    else d2<-d
    
    d2$score <- score.sentiment(d2$word, pos.words, neg.words, .progress="text")
    d2$sentiment <- rep(0)
    d2$sentiment <- ifelse(d2$score>=1, "Positive", d2$sentiment)
    d2$sentiment <- ifelse(d2$score==0, "Neutral", d2$sentiment)
    d2$sentiment <- ifelse(d2$score<=-1, "Negative", d2$sentiment)
    d2$sentiment<-factor(d2$sentiment,levels=c('Positive','Neutral','Negative'))
    
    d3 = data.frame(date = profData$created, score = profData$review_score)
    d4 <- profData$nugget[1]
    d5 <- as.character(profData$last_name[1])
    
    #d6 <- round(mean(profData[,10]$score), 2)
    
    class_emo=classify_emotion(prof$review_text[prof$profName==input$profName],algorithm="bayes",
                               prior=1.0)
    # get emotion best fit
    emotion = class_emo[,7]
    # substitute NA's by "unknown"
    emotion[is.na(emotion)] = "unknown"
    
    # classify polarity
    class_pol = classify_polarity(prof$review_text[prof$profName==input$profName],algorithm="bayes")
    # get polarity best fit
    polarity = class_pol[,4]
    
    # data frame with results
    sent_df = data.frame(text=prof$review_text[prof$profName==input$profName],emotion=emotion,
                         polarity=polarity, stringsAsFactors=FALSE)
    # sort data frame
    sent_df = within(sent_df,
                     emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
    
    emos = levels(factor(sent_df$emotion))
    nemo = length(emos)
    emo.docs = rep("", nemo)
    
    for (i in 1:nemo){
      tmp = prof$review_text[prof$profName==input$profName][emotion == emos[i]]
      emo.docs[i] = paste(tmp, collapse=" ")
    }
    
    # remove stopwords
    emo.docs = removeWords(emo.docs, stopwords("english"))
    emo.docs = removeWords(emo.docs, c("group", "final.", "final", "disgusting"))
    # create corpus
    corpus = Corpus(VectorSource(emo.docs))
    corpus <- tm_map(corpus, removePunctuation) 
    tdmReview = as.matrix(TermDocumentMatrix(corpus,control=list(wordLengths=c(0,Inf))))
    colnames(tdmReview) = emos
    #browser()
    if(ncol(tdmReview)>=2) tdmReview<-tdmReview[rev(order(rowSums(tdmReview))),][1:50,]
    else {
      tdmReview<-data.frame(tdmReview[rev(order(rowSums(tdmReview))),][1:50])
      colnames(tdmReview) = emos
    }
    #browser()
    
    return(list(profData,d2,tdmReview,emos,d3,d4,d5))
  })
  
  courseData<-reactive({
    courseData<-course[course$name==input$courseName,]
    
    courseDocs<-coursedocs[course$name==input$courseName]
    
    txtTdmBi <- as.matrix(TermDocumentMatrix(courseDocs, control = list(tokenize = BigramTokenizer)))
    v = sort(rowSums(txtTdmBi),decreasing=TRUE)
    d = data.frame(word = names(v),freq=v)
    d<-d[!d$word%in%
           c('final','midterm','finals','midterms','assignment','assignments','problem','problems'),]
    if(nrow(d)>=30) d2<-d[1:30,]
    else d2<-d
    
    d2$score <- score.sentiment(d2$word, pos.words, neg.words, .progress="text")
    d2$sentiment <- rep(0)
    d2$sentiment <- ifelse(d2$score>=1, "Positive", d2$sentiment)
    d2$sentiment <- ifelse(d2$score==0, "Neutral", d2$sentiment)
    d2$sentiment <- ifelse(d2$score<=-1, "Negative", d2$sentiment)
    d2$sentiment<-factor(d2$sentiment,levels=c('Positive','Neutral','Negative'))
    
    d3 <- as.character(courseData$course_ids[1])
    
    return(list(courseData,d2,d3))
  })
  
  # Also, I edited the UI.R code and added images to the www folder
  output$review_dygraph <- renderDygraph({
    d3<-profData()[[5]]
    series <- xts(d3$score, order.by = d3$date, tz="GMT")
    dygraph(series, xlab = "Date", ylab = "Sentiment Score") %>% dyRangeSelector() %>% 
      dyOptions(useDataTimezone = TRUE, fillGraph = TRUE) %>% 
      dySeries("V1", label = "Sentiment Score") %>% dyLegend(show = "always", hideOnMouseOut = FALSE)
  })
  
  output$prof_name = renderText({
    paste("Professor ", input$profName)
  })
  
  output$course_name = renderText({
    paste(input$courseName)
  })
  
  output$prof_sent_score = renderText({
    data <- profData()[[1]]
    paste("Sentiment score ranking: #",data$rank[1],' out of 24 professors')
  })
  
  output$course_sent_score = renderText({
    data <- courseData()[[1]]
    paste("Sentiment score ranking: #",data$rank[1],' out of 27 courses')
  })
  
  output$course_pic <- renderUI({
    d3 <- courseData()[[3]]
    HTML(paste0("<img src='cscoursepics/", d3,
                ".jpg' align = 'center', style='float: left; margin-right: 16px; margin-left: 4px; 
                height: 140px; margin-bottom: 19px; margin-top: 5px; border: 4px solid #3c8dbc; 
                border-radius: 5px;'>"))
    
  })
  
  output$nugget <- renderUI({
    d4<-profData()[[6]]
    if(d4 == "Gold"){
      HTML("<img src='gold1.png' align = 'center', style='width: 79px; float: left; margin-right: 16px; 
           margin-left: 39px; height: 60px;'>")
    } else if(d4 == "Silver"){
      HTML("<img src='silver1.png' align = 'center', style='width: 79px; float: left; margin-right: 16px; 
           margin-left: 39px; height: 60px;'>")
    } else {
      HTML("<img src='no_nugget.png' align = 'center', style='width: 69px; float: left; margin-right: 13px;
           margin-left: 39px; height: 53px;'>")
    }
  })
  
  output$prof_pic <- renderUI({
    d5<-profData()[[7]]
    if(d5 == "Pe'er")
      HTML("<img src='csprofpics/peer.jpg' align = 'center', style='float: left; margin-right: 16px; 
           margin-left: 4px; height: 140px; margin-bottom: 19px; margin-top: 5px; 
           border: 4px solid #3c8dbc; border-radius: 5px;'>")
    else 
      HTML(paste0("<img src='csprofpics/", tolower(d5),
                  ".jpg' align = 'center', style='float: left; margin-right: 16px; margin-left: 4px; 
                  height: 140px; margin-bottom: 19px; margin-top: 5px; border: 4px solid #3c8dbc; 
                  border-radius: 5px;'>"))
  })
  
  output$sentiment_cloudCourse <- renderPlot({
    d2<-courseData()[[2]]
    #browser()
    wordcloud(words = d2$word,freq = d2$freq, scale=c(5,0.1),random.order = F,rot.per=0.35,min.freq=1, 
              colors=brewer.pal(8, "Dark2"))  
  })
  
  # output$workloadScore<-renderText({
  # 
  #   data<-Data()[1]
  #   name<-gsub('\\[.*|\\(.*|[[:punct:]]','',as.character(amzData$Name[amzData$ASIN==data]))
  #   return(name)
  # 
  # })
  
  output$sentiment_bar_chartCourse<-renderChart2({
    d2<-courseData()[[2]]
    #browser()
    colors=colors[as.character(d2$sentiment)]
    names(colors)<-NULL
    
    workload<-Highcharts$new()
    workload$chart(type="column")
    workload$series(data=d2$freq,colorByPoint=T,colors=colors,name='Workload Key Words')
    #workload$series(data=d2$freq[d2$sentiment=='Positive'],name='Positive')
    #mtdChart$series(data=lastYearMonthData,dashStyle="shortdot",name='Same Month Last Year')
    workload$legend(symbolWidth = 80)
    #mtdChart$yAxis(min=0,title=list(text=input$cumTrafficGraphOption))
    workload$xAxis(title=list(text="Words"),categories=as.character(d2$word),labels=list(rotation=45))
    workload$legend(enabled=F)
    workload$set(dom="workload")
    return(workload)
    
  })
  
  
  output$comparison_cloudProf <- renderPlot({
    tdmReview<-data.frame(profData()[[3]])
    if(ncol(tdmReview)==1){
      emos=profData()[[4]]
      wordcloud(words = emos,freq = 10, scale=c(5,0.1),random.order = F,rot.per=0.35,min.freq=1, 
                colors=brewer.pal(8, "Dark2"))
    }
    else
      comparison.cloud(tdmReview, colors = brewer.pal(ncol(tdmReview), "Dark2"),
                       scale = c(2,.5), random.order = FALSE, title.size = 1.5)
  })
  
  output$sentiment_bar_chartProf<-renderChart2({
    d2<-profData()[[2]]
    #browser()
    colors=colors[as.character(d2$sentiment)]
    names(colors)<-NULL
    
    profBar<-Highcharts$new()
    profBar$chart(type="column")
    profBar$series(data=d2$freq,colorByPoint=T,colors=colors,name='Key Words')
    #workload$series(data=d2$freq[d2$sentiment=='Positive'],name='Positive')
    #mtdChart$series(data=lastYearMonthData,dashStyle="shortdot",name='Same Month Last Year')
    profBar$legend(symbolWidth = 80)
    #mtdChart$yAxis(min=0,title=list(text=input$cumTrafficGraphOption))
    profBar$xAxis(title=list(text="Words"),categories=as.character(d2$word),labels=list(rotation=45))
    profBar$legend(enabled=F)
    profBar$set(dom="profBar")
    return(profBar)
    
  })
  
  output$heatmap <- renderChart2({
    map <- Highcharts$new()
    map$chart(zoomType = "x", type = 'heatmap')
    map$credits(text = "Created with rCharts and Highcharts", href = "http://rcharts.io")
    map$title(text='Sales per employee per weekday')
    
    map$series(name = 'Sales per employee',
               data = toJSONArray2(dat, json=FALSE),
               color = "#cccccc",
               dataLabels = list(
                 enabled = TRUE,
                 color = 'black',
                 style = list(
                   textShadow = 'none',
                   HcTextStroke = NULL
                 )
               ))
    
    map$yAxis(categories = c('9-9:30am','9:30-10am','10-10:30am','10:30-11am','11-11:30am','11:30am-12pm', 
                             '12-12:30pm','12:30pm-1pm','1-1:30pm','1:30-2pm','2-2:30pm','2:30-3pm',
                             '3-3:30pm','3:30-4pm','4-4:30pm','4:30-5pm','5-5:30pm','5:30-6pm','6-6:30pm',
                             '6:30-7pm','7-7:30pm','7:30-8pm','8-8:30pm','8:30-9pm','9-9:30pm'))
    
    map$xAxis(categories = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),
              title=list(text = ""))
    
    map$addParams(colorAxis =
                    list(
                      min = 0,
                      minColor='#FFFFFF',
                      maxColor='#7cb5ec'
                    )
    )
    
    # map$legend(align='right',
    #            layout='vertical',
    #            margin=0,
    #            verticalAlign='top',
    #            y=25,
    #            symbolHeight=320)
    
    # custom tooltip
#     map$tooltip(formatter="#! function() { return '<b>' + this.series.xAxis.categories[this.point.x] + 
# '</b> sold <br><b>' + this.point.value + '</b> items on <br><b>' + 
#                 this.series.yAxis.categories[this.point.y] + '</b>'; } !#")
    
    
    # set width and height of the plot and attach it to the DOM
    map$addParams(height = 2000, width=1000, dom="heatmap")
    
    # save heatmap as HTML page heatmap.html for debugging
    #map$save(destfile = 'heatmap.html')
    
    # print map
    print(map)
})
  
  
})