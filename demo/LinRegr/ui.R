ui <- fluidPage(
  sidebarPanel(style = "background-color: #ffffff ;",
    wellPanel(
      textOutput('a'),
      textInput("tbl", "Table Name", ""),
      textInput("id", "Specify the obsID Column", ""),
      textInput('SplitRatio', 'Train : Data Split Ratio', ""),
      uiOutput("woman")

   ),
    uiOutput("well4"),
    uiOutput("well2"),
    uiOutput("well3")
  ),
  
 mainPanel(h3("Linear Regression"),
    tabsetPanel(
      tabPanel("Parameters",
               br(),br(),
               div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("FetchCoefficients")),
               div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("FetchSummary")),
               div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("FetchStats")),
               fluidRow(
                 h4(textOutput('c')),
                 br(),
                 dataTableOutput('b')
               )
      ),
      tabPanel("Plots",
               br(),br(),
               uiOutput("Plot"),
               plotOutput("plot1")
      ),
      tabPanel("Scoring",
                br(), br(),
                div(style="display: inline-block;align: center; vertical-align:top; width: 150px;",uiOutput("FetchFittedValues")),
                plotOutput('d')

       )
    )
  )
)