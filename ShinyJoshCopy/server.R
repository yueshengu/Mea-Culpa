options(shiny.maxRequestSize=50*1024^2)

shinyServer(function(input, output, session) {

    Data<-reactive({
      #browser()

      profData<-prof[prof$profName==input$profName,]

      profDocs<-docs[prof$profName==input$profName]
      
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
      
      ############ ADDED CODE HERE ##################
      d3 = data.frame(date = profData$created, score = profData$review_score)
      d4 <- profData$nugget[1]
      d5 <- as.character(profData$last_name[1])
      
      
      return(list(profData,d2, d3, d4, d5))
      ###############################################
    })
    
    output$sentiment_cloud <- renderPlot({
      d2<-Data()[[2]]
      #browser()
      wordcloud(words = d2$word,freq = d2$freq, scale=c(5,0.1),random.order = F,rot.per=0.35,min.freq=1, 
                colors=brewer.pal(8, "Dark2"))  
    })
    
    output$sentiment_bar_chartWorkload<-renderChart2({
      d2<-Data()[[2]]
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
      #workload$set(dom="workload")
      return(workload)
      
    })

    ################### ADDED CODE HERE ############################
    
    # Also, I edited the UI.R code and added images to the www folder
    output$review_dygraph <- renderDygraph({
      d3<-Data()[[3]]
      
      series <- xts(d3$score, order.by = d3$date, tz="GMT")
      dygraph(series, xlab = "Date", ylab = "Sentiment Score") %>% dyRangeSelector() %>% dyOptions(useDataTimezone = TRUE, fillGraph = TRUE) %>% dySeries("V1", label = "Sentiment Score") %>% dyLegend(show = "always", hideOnMouseOut = FALSE)
    })

    output$prof_name = renderText({
      paste("Professor ", input$profName)
    })
    
    output$nugget <- renderUI({
      d4<-Data()[[4]]
      if(d4 == "Gold"){
        HTML("<img src='gold1.png' align = 'center', style='width: 79px; float: left; height: 60px;'>")
      } else if(d4 == "Silver"){
        HTML("<img src='silver1.png' align = 'center', style='width: 79px; float: left; height: 60px;'>")
      } else {
        HTML("<img src='no_nugget.png' align = 'center', style='width: 69px; float: left; height: 53px;'>")
      }
    })
    
    output$prof_pic <- renderUI({
      d5<-Data()[[5]]
      if(d5 == "Pe'er"){
        HTML("<img src='csprofpics/peer.jpg' align = 'center', style='float: left; margin-right: 16px; margin-left: 4px; height: 140px; margin-bottom: 19px; margin-top: 5px; border: 4px solid #3c8dbc; border-radius: 5px;'>")
      }else {
        HTML(paste0("<img src='csprofpics/", tolower(d5),".jpg' align = 'center', style='float: left; margin-right: 16px; margin-left: 4px; height: 140px; margin-bottom: 19px; margin-top: 5px; border: 4px solid #3c8dbc; border-radius: 5px;'>"))
      }
      
    })
    
    #####################################################
    # output$sentiment_bar_chartWorkload<-renderPlot({
    #   d2<-Data()[[2]]
    #   #browser()
    #   ggplot(d2,aes(x=word, y=freq, fill=sentiment)) +
    #     geom_bar(stat="identity", colour="white") +
    #     theme(axis.text.x=element_text(angle=45, hjust=1)) + ylab("Frequency") + xlab("Word") +
    #     scale_fill_manual(values=c("green", "grey", "red"))  
    # })
    
    
    
    # 
    # output$Rec1Name<-renderText({
    #   
    #   data<-Data()[1]
    #   name<-gsub('\\[.*|\\(.*|[[:punct:]]','',as.character(amzData$Name[amzData$ASIN==data]))
    #   return(name)
    #   
    # })
    # 
    # output$profPic = renderImage({
    #   #data<-Data()[1]
    #   #browser()
    #   #name<-gsub('\\[.*|\\(.*|[[:punct:]]','',as.character(amzData$Name[amzData$ASIN==data]))
    #   # omdb.entry=search_by_title(name)
    #   # result<-data.frame(find_by_id(omdb.entry$imdbID[1], include_tomatoes=T))
    #   #url<-read_html(paste0('http://www.cs.columbia.edu/mice/persons/getPhoto.php?personID=1955'))
    # 
    #   #src<-gsub('.*src=\\\"|\".*','',html_nodes(url,xpath="//div[@id='movie-image-section']//img"))
    #   #html_nodes(url,xpath="//img")
    #   #html_nodes(url,xpath="//tbody")
    # 
    #   return(list(
    #     src = "AdamCannon.jpg",
    #     contentType = "image/jpeg",
    #     alt = "Face"
    #   ))
    # })
    # 
    # output$Rec1Rating<-renderText({
    #   data<-Data()[1]
    #   rating<-as.character(amzData$Rating[amzData$ASIN==data])
    #   return(rating)
    # })
    # 
    # output$Rec1Reviews<-renderText({
    #   data<-Data()[1]
    #   Reviews<-as.character(amzData$Reviews[amzData$ASIN==data])
    #   return(Reviews)
    # })
    # 
    # 
    # output$rec1wc <- renderPlot({
    #   
    #   ASIN <- Data()[1]
    #   createwc(as.character(ASIN))
    # })
    # 
    # output$Rec1BotReviewTitle<-renderText({
    #   return('Least Helpful Review:')
    # })
    # 
    # output$Rec1BotReview<-renderText({
    #   data<-Data()[1]
    #   reviews<-data_part[data_part$product_productid==data,]
    #   #browser()
    #   botReview<-gsub("<.*?>", "",reviews$review_text[order(reviews$reviewHelpTotal)][1])
    #   return(paste0(botReview))
    # })
    # 
    # output$Rec1TopReviewTitle<-renderText({
    #   return('Most Helpful Review:')
    # })
    # 
    # output$Rec1TopReview<-renderText({
    #   data<-Data()[1]
    #   reviews<-data_part[data_part$product_productid==data,]
    #   #browser()
    #   topReview<-gsub("<.*?>", "",reviews$review_text[rev(order(reviews$reviewHelpTotal))][1])
    #   return(paste0(topReview))
    # })
    # 
    # 
    # output$Rec2Name<-renderText({
    #   
    #   data<-Data()[2]
    #   name<-gsub('\\[.*|\\(.*|[[:punct:]]','',as.character(amzData$Name[amzData$ASIN==data]))
    #   return(name)
    #   
    # })
    # 
    # output$Rec2image = renderUI({
    #   data<-Data()[2]
    #   #browser()
    #   name<-gsub('\\[.*|\\(.*|[[:punct:]]','',as.character(amzData$Name[amzData$ASIN==data]))
    #   # omdb.entry=search_by_title(name)
    #   # result<-data.frame(find_by_id(omdb.entry$imdbID[1], include_tomatoes=T))
    #   url<-read_html(paste0('http://www.rottentomatoes.com/search/?search=',gsub(' ','+',name)))
    #   
    #   if(length(gsub('.*src=\\\"|\".*','',
    #                  html_nodes(url,xpath="//div[@id='movie-image-section']//img"))!=0))
    #     src<-gsub('.*src=\\\"|\".*','',html_nodes(url,xpath="//div[@id='movie-image-section']//img"))
    #   else{
    #     newSub<-gsub('.*href="|\\/">.*','',
    #                  html_nodes(url,xpath="//div[@class='nomargin media-heading bold']/a"))
    #     url2<-read_html(paste0('http://www.rottentomatoes.com/',newSub[1]))
    #     
    #     src<-gsub('.*src=\\\"|\".*','',html_nodes(url2,xpath="//div[@id='movie-image-section']//img"))
    #   }
    #   return(tags$img(src=src))
    # })
    # 
    # output$Rec2Rating<-renderText({
    #   data<-Data()[2]
    #   rating<-as.character(amzData$Rating[amzData$ASIN==data])
    #   return(rating)
    # })
    # 
    # output$Rec2Reviews<-renderText({
    #   data<-Data()[2]
    #   Reviews<-as.character(amzData$Reviews[amzData$ASIN==data])
    #   return(Reviews)
    # })
    # 
    # output$rec2wc <- renderPlot({
    #   
    #   ASIN <- Data()[2]
    #   createwc(as.character(ASIN))
    # })
    # 
    # output$Rec2BotReviewTitle<-renderText({
    #   return('Least Helpful Review:')
    # })
    # 
    # output$Rec2BotReview<-renderText({
    #   data<-Data()[2]
    #   reviews<-data_part[data_part$product_productid==data,]
    #   #browser()
    #   botReview<-gsub("<.*?>", "",reviews$review_text[order(reviews$reviewHelpTotal)][1])
    #   return(paste0(botReview))
    # })
    # 
    # output$Rec2TopReviewTitle<-renderText({
    #   return('Most Helpful Review:')
    # })
    # 
    # output$Rec2TopReview<-renderText({
    #   data<-Data()[2]
    #   reviews<-data_part[data_part$product_productid==data,]
    #   #browser()
    #   topReview<-gsub("<.*?>", "",reviews$review_text[rev(order(reviews$reviewHelpTotal))][1])
    #   return(paste0(topReview))
    # })
    # 
    # output$Rec3Name<-renderText({
    #   
    #   data<-Data()[3]
    #   name<-gsub('\\[.*|\\(.*|[[:punct:]]','',as.character(amzData$Name[amzData$ASIN==data]))
    #   return(name)
    #   
    # })
    # 
    # output$Rec3image = renderUI({
    #   data<-Data()[3]
    #   #browser()
    #   name<-gsub('\\[.*|\\(.*|[[:punct:]]','',as.character(amzData$Name[amzData$ASIN==data]))
    #   # omdb.entry=search_by_title(name)
    #   # result<-data.frame(find_by_id(omdb.entry$imdbID[1], include_tomatoes=T))
    #   url<-read_html(paste0('http://www.rottentomatoes.com/search/?search=',gsub(' ','+',name)))
    #   if(length(gsub('.*src=\\\"|\".*','',
    #                  html_nodes(url,xpath="//div[@id='movie-image-section']//img"))!=0))
    #     src<-gsub('.*src=\\\"|\".*','',html_nodes(url,xpath="//div[@id='movie-image-section']//img"))
    #   else{
    #     newSub<-gsub('.*href="|\\/">.*','',
    #                  html_nodes(url,xpath="//div[@class='nomargin media-heading bold']/a"))
    #     url2<-read_html(paste0('http://www.rottentomatoes.com/',newSub[1]))
    #     
    #     src<-gsub('.*src=\\\"|\".*','',html_nodes(url2,xpath="//div[@id='movie-image-section']//img"))
    #   }
    #   return(tags$img(src=src))
    # })
    # 
    # output$Rec3Rating<-renderText({
    #   data<-Data()[3]
    #   rating<-as.character(amzData$Rating[amzData$ASIN==data])
    #   return(rating)
    # })
    # 
    # output$Rec3Reviews<-renderText({
    #   data<-Data()[3]
    #   Reviews<-as.character(amzData$Reviews[amzData$ASIN==data])
    #   return(Reviews)
    # })
    # 
    # output$rec3wc <- renderPlot({
    #   
    #   ASIN <- Data()[3]
    #   createwc(as.character(ASIN))
    #   
    # })
    # 
    # output$Rec3BotReviewTitle<-renderText({
    #   return('Least Helpful Review:')
    # })
    # 
    # output$Rec3BotReview<-renderText({
    #   data<-Data()[3]
    #   reviews<-data_part[data_part$product_productid==data,]
    #   #browser()
    #   botReview<-gsub("<.*?>", "",reviews$review_text[order(reviews$reviewHelpTotal)][1])
    #   return(paste0(botReview))
    # })
    # 
    # output$Rec3TopReviewTitle<-renderText({
    #   return('Most Helpful Review:')
    # })
    # 
    # output$Rec3TopReview<-renderText({
    #   data<-Data()[3]
    #   reviews<-data_part[data_part$product_productid==data,]
    #   #browser()
    #   topReview<-gsub("<.*?>", "",reviews$review_text[rev(order(reviews$reviewHelpTotal))][1])
    #   return(paste0(topReview))
    # })
    
    
  })