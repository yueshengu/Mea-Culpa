dbHeader<-dashboardHeader(title='Mea CULPA')


dashboardPage(
  skin="blue",
  dbHeader,
  dashboardSidebar(
    sidebarMenu(id='sidebarmenu',
                menuItem("Professors",tabName="prof",icon=icon("pie-chart")),
                menuItem("Courses",tabName="course",icon=icon("pie-chart")),
                menuItem("Recommendation",tabName="rec",icon=icon("pie-chart"))
    ),
    conditionalPanel("input.sidebarmenu === 'prof'",
                     #h4('Web Traffic Options:',align='center'),
                     selectInput("profName","Professor:",levels(prof$profName),
                                 selected=levels(prof$profName)[1],multiple=F,width="100%")),
    conditionalPanel("input.sidebarmenu === 'course'",
                     #h4('Web Traffic Options:',align='center'),
                     selectInput("courseName","Course:",levels(course$name),
                                 selected=levels(course$name)[1],multiple=F,width="100%")),
    conditionalPanel("input.sidebarmenu === 'rec'",
                     #h4('Web Traffic Options:',align='center'),
                     selectInput("courseTopics","Topics of interest:",courseTags,
                                 selected='cloud',multiple=T,width="100%"))
    
  ),
  
  dashboardBody(
    includeCSS('./www/custom.css'),
    tags$head(tags$script(src = "https://code.highcharts.com/highcharts.js"),
              tags$script(src = "https://code.highcharts.com/highcharts-more.js"),
              tags$script(src = "https://code.highcharts.com/modules/exporting.js"),
              tags$script(src = "https://code.highcharts.com/modules/heatmap.js")),
    tabItems(
      
      tabItem(tabName='prof',
              fluidRow(
                column(width=12,
                       htmlOutput("prof_pic"), 
                       textOutput("prof_name"),
                       textOutput("prof_sent_score"),
                       htmlOutput("nugget"),
                       tags$head(tags$style("#prof_name{color:#3c8dbc;font-size:33px;font-weight:bold;
                                            margin-bottom:8px; margin-top:15px;}"), 
                                 tags$style("#prof_sent_score{float: left; font-style: italic; 
                                            color:#3c8dbc;font-size:23px; padding-top: 10px;}"))                          
                       )
              ),
              
              fluidRow(
                column(width=4,
                       box(title = "Review Sentimental Cloud", status = "primary",
                           width=NULL,solidHeader=T,
                           plotOutput("comparison_cloudProf")
                       )
                ),
                column(width=8,
                       box(title = "Review Sentiments", status = "primary",
                           width=NULL,solidHeader=T,
                           showOutput("sentiment_bar_chartProf","highcharts")
                       )
                )
              ),
              fluidRow(
                column(width=12,
                       box(title = "Timeline of Review Sentiments", status = "primary",
                           width=NULL,solidHeader=T,
                           dygraphOutput("review_dygraph")
                       )
                )
              )
      ),
      tabItem(tabName='course',
              fluidRow(
                column(width=12,
                       htmlOutput("course_pic"), 
                       textOutput("course_name"),
                       textOutput("course_sent_score"),
                       tags$head(tags$style("#course_name{color:#3c8dbc;font-size:33px;font-weight:bold;
                                            margin-bottom:19px;margin-top:4px;}"),
                                 tags$style("#course_sent_score{float: left; font-style: italic; 
                                            color:#3c8dbc;font-size:23px; padding-top: 10px;}"))                          
                )
              ),
              fluidRow(
                column(width=4,
                       box(title = "Workload Cloud", status = "primary",
                           width=NULL,solidHeader=T,
                           plotOutput("sentiment_cloudCourse")
                       )
                ),
                column(width=8,
                       box(title = "Workload Sentiments", status = "primary",
                           width=NULL,solidHeader=T,
                           showOutput("sentiment_bar_chartCourse","highcharts")
                       )
                )
              )
      ),
      tabItem(tabName='rec',
              fluidRow(
                column(width=3,
                       selectInput("firstCourse","First Course:",levels(course$name),
                                   selected='Computer Architecture',multiple=F,width="100%")
                ),
                column(width=3,
                       selectInput("secondCourse","Second Course:",levels(course$name),
                                   selected=NULL,multiple=F,width="100%")
                )
              ),
              fluidRow(
                column(width=12,
                       box(title = "Schedule", status = "primary",
                           width=NULL,solidHeader=T,
                           showOutput("heatmap","highcharts")
                       )
                )
              )
             
      )
    )
  )
)

