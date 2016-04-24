options(shiny.maxRequestSize=50*1024^2)

shinyServer(function(input, output, session) {

    # Data<-reactive({
    #   
    #   asinSelected<-amzData$ASIN[amzData$Name%in%input$moviesLiked]
    #   
    #   users<-unique(data_part$review_userid[data_part$product_productid%in%asinSelected])
    #   movies<-data_part$product_productid[data_part$review_userid%in%users&data_part$review_score>=4]
    #   newMovies<-movies[!movies%in%asinSelected]
    #   
    #   return(names(rev(sort(table(newMovies))))[1:3])
    # })
    # 
    # 
    # output$Rec1Name<-renderText({
    #   
    #   data<-Data()[1]
    #   name<-gsub('\\[.*|\\(.*|[[:punct:]]','',as.character(amzData$Name[amzData$ASIN==data]))
    #   return(name)
    #   
    # })
    # 
    output$profPic = renderImage({
      #data<-Data()[1]
      #browser()
      #name<-gsub('\\[.*|\\(.*|[[:punct:]]','',as.character(amzData$Name[amzData$ASIN==data]))
      # omdb.entry=search_by_title(name)
      # result<-data.frame(find_by_id(omdb.entry$imdbID[1], include_tomatoes=T))
      #url<-read_html(paste0('http://www.cs.columbia.edu/mice/persons/getPhoto.php?personID=1955'))

      #src<-gsub('.*src=\\\"|\".*','',html_nodes(url,xpath="//div[@id='movie-image-section']//img"))
      #html_nodes(url,xpath="//img")
      #html_nodes(url,xpath="//tbody")

      return(list(
        src = "AdamCannon.jpg",
        contentType = "image/jpeg",
        alt = "Face"
      ))
    })
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