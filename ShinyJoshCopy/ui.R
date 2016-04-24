dbHeader<-dashboardHeader(title='CS Courses')

dashboardPage(

    skin="blue",
  
  dbHeader,

  dashboardSidebar(
    sidebarMenu(id='sidebarmenu',
                menuItem("Professors",tabName="prof",icon=icon("pie-chart")),
                menuItem("Recommendation",tabName="rec",icon=icon("pie-chart"))
                ),
    
    conditionalPanel("input.sidebarmenu === 'prof'",
                     #h4('Web Traffic Options:',align='center'),
                     selectInput("profName","Professor:",levels(prof$profName),
                                 selected=levels(prof$profName)[1],multiple=F,width="100%"))
   
    
    ),
              
  dashboardBody(
    includeCSS('./www/custom.css'),
    tabItems(
      
      
      tabItem(tabName='prof',
####################  ADDED CODE HERE  #################################
              fluidRow(column(width=12,htmlOutput("nugget"), textOutput("prof_name"),
              tags$head(tags$style("#prof_name{ color: #3c8dbc; font-size: 33px; font-weight: bold; margin-bottom: 19px; margin-top: 4px;
                                 }"
                         )
              )                               
              )),
##########################################################################
              fluidRow(
                # column(width = 7,
                #        box(width = NULL, solidHeader = TRUE,
                #            leafletOutput("Map"))),
                column(width=4,
                       box(title = "Workload Cloud", status = "primary",
                           width=NULL,solidHeader=T,
                           plotOutput("sentiment_cloud")
                           )
                       ),
                column(width=8,
                       box(title = "Workload Sentiments", status = "primary",
                           width=NULL,solidHeader=T,
                           showOutput("sentiment_bar_chartWorkload","highcharts")
                           #plotOutput("sentiment_bar_chartWorkload")
                       )
                )
                ),
####################  ADDED CODE HERE  #################################
              fluidRow(
                column(width=12,
                       box(title = "Timeline of Review Sentiments", status = "primary",
                           width=NULL,solidHeader=T,
                           dygraphOutput("review_dygraph")
                           )
                )
                
                )
##########################################################################
      )
    )

  )
)

