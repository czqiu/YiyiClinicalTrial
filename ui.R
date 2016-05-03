

library(shiny)
library(ggplot2)
library(plyr)
require(rCharts)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("A Bayesian Up-and-Down Design for Clinical Trials with Multiple Adverse Events in Opposite Directions"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      h2("Model 1 Stage"),
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      numericInput("obs", "Number of observations to view:", 10,min=1,max=100),
      
      #tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
#       sliderInput("bins",
#                   "Number of bins:",
#                   min = 5,
#                   max = 50,
#                   value = 30),
      selectInput(inputId = "type",
                  label = "Choose Chart Type",
                  choices = c("multiBarChart", "multiBarHorizontalChart"),
                  selected = "multiBarChart"),
      
      actionButton("ShowPlot","Show Summaried Freq Plot in Results Tab",style="font-size:80%"),
      br(),
     h2("Model 2 Stage"),
    numericInput('ID',"Subject ID",0),
     selectInput('x','X',choices=c("Yes","No")),
     selectInput('y','Y',choices=c("Yes","No")),
     selectInput('z','Z',choices=c("Yes","No")),
    selectInput("days", label = "pin remove days from installation:", 
            choices = list("19-21" = 1, "22-24" = 2,
                           "25-27" = 3, "28-30"=4,"31-35"=5), selected = 1),
     #numericInput('days',"pin remove days from installation:",1,min=0,max=40),
 
     actionButton("save","SAVE"),
      p(strong("Only when your current cohort has completed")),
    actionButton("stage2", "Find Assignment for Next Cohort")

    ),
    
    
   
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id ="theTabs",
        tabPanel("Summary",  
                 verbatimTextOutput('summary1'),"Please click SAVE to see updated summary table",
                 verbatimTextOutput('summary2'),tableOutput('table1')),
                 #verbatimTextOutput('summary3')), #for interim result check purpose
        tabPanel("Results", wellPanel(
        #fluidRow(column (4,tableOutput('values2')),(column (8,tableOutput('values3'))))
          fluidRow((column (8,tableOutput('values3'))))
        ),
        fluidRow(column (12 ,"Frequency of each type for each window will show here", showOutput("myChart", "nvd3"))),
        
        fluidRow(column (12 , "value from loss function will show here after Running 'Find Assignment for Next Cohort'",showOutput("myChart1", "morris")))
        )
        
       
    )
    )
  )
))
