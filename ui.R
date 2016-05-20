
require(rCharts)
library(plotly)
library(networkD3)
pdd <- read.csv('pddistrict.csv',stringsAsFactors = FALSE)
# Define UI for random distribution application 
shinyUI(fluidPage(theme = 'bootstrap.css',
																		 headerPanel(tags$h1(tags$strong("San Francisco Crime Prediction",style='color:red3'))),
  
  # Application title
   br(),
  fluidRow(
  
  column(
    absolutePanel(top = 100, left =20, right = '75%', bottom = 100,fixed=T,
    	 tabsetPanel(type = "tabs",
    		tabPanel("Prediction", br(), 
      textInput("datetime", "Date", value = "12/23/2014 1:05:00 pm" ),
      uiOutput('DayOfWeek'),
      
      selectInput('pddd','PD DISTRICT', choices = pdd$x,
        selectize = TRUE),
      
      textInput("address", label = ("Address"), value = "2000 Block of THOMAS AV"),
      textInput("X", label = "Longitude [X]", value="-122.399587704189"),
      textInput("Y", label = "Latitude [Y]",value="37.7350510103906 "),
      submitButton("Submit",icon = icon("fa fa-spinner"))),
    		tabPanel("Animation", br(),
    											
    			textInput("starttime", "Start Date/Time", value = "12/23/2014 1:05:00 pm" ),
      
#       	radioButtons("datetimerange", label = "Generate Dates By ?",
#     choices = list("Years" = "years", "Months" = "months", "Days" = "days","Hours"= "hours"), 
#     selected = "days"), 
#       selectInput('pddd','PD DISTRICT', choices = pddd$x,
#         selectize = TRUE),
        selectInput('category','Categories of Crime', choices = c('NonResiProp','ResiProp',
     																																																										'Personal','Statutory','WhiteCollar','Others'),
        selectize = TRUE),
        submitButton("Submit",icon = icon("fa fa-spinner"))
     
     # uiOutput('datestuff')
    			
    		)
    		
    		)
     
     
    ),width = 3),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(br(), br(),
    	tabsetPanel(type = "tabs", 
    		tabPanel('Prediction Controls', icon = icon("fa fa-code-fork" ), br(), 
     										 radioButtons("model", "Choose Model",
     										              c("OVA Logistic " = "ova",
     										                "Multinomial Logit" = "mult",
     										                "Bagging" = "bg", 
     										                "Random Forest" = "rf", 
     										                "Gradient Boosting Trees" = "gbm"),inline=TRUE), hr(),
      
      tabsetPanel(type = "tabs",
      												tabPanel('Map',plotOutput('Map')),
      												tabPanel('Class Labels',diagonalNetworkOutput("force")),
      												
      												tabPanel("Error Tab",
                           verbatimTextOutput('experiment')),
                  tabPanel("Individual Plot", br(),plotlyOutput("plot")), 
      												tabPanel("All Model Plots ",showOutput("myChart","nvd3")),
                  tabPanel("Predicted Probabilities", verbatimTextOutput('summary'))))
                  ,
    		            tabPanel('Visualization',icon = icon("fa fa-bar-chart"),
    	 									   tabPanel('Plot',plotOutput('plotViz'))
    	 									
    	 	
    	 ),
    		tabPanel('How  To Use This App? ',icon = icon("fa fa-info"),
    	 									tabPanel('Help',br(),includeMarkdown('include.Rmd'))
    		
    		)
    	           
    
               )
    )
)))