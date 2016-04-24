dbHeader<-dashboardHeader(title='CS Courses')

dashboardPage(

    skin="blue",
  
  dbHeader,

  dashboardSidebar(
    sidebarMenu(id='sidebarmenu',
                menuItem("Prfoessors",tabName="prof",icon=icon("pie-chart")),
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
              fluidRow(
                # column(width = 7,
                #        box(width = NULL, solidHeader = TRUE,
                #            leafletOutput("Map"))),
                column(width=4,
                       box(title = "First Recommendation", status = "primary",
                           width=NULL,solidHeader=T#,
                           # textOutput("Rec1Name"),
                           # br(),
                           # uiOutput('Rec1image'),
                           # br(),
                           # textOutput("Rec1Rating"),
                           # textOutput("Rec1Reviews"),
                           # br(),
                           #imageOutput("profPic")
                           # textOutput("Rec1BotReviewTitle"),
                           # textOutput("Rec1BotReview"),
                           # br(),
                           # textOutput("Rec1TopReviewTitle"),
                           # textOutput("Rec1TopReview")
                           )
                       )
                )
      )
    )

  )
)

