# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)
source("http://goo.gl/w64gfp")
library(magrittr)
library(XML)
library(reshape)
library(gsheet)

odd_indexes<-seq(4,39,2)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Incidents by Region"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "Region:", 
                  choices=colnames(acled.long[odd_indexes])),
      sliderInput("days", "Start-End", min = 1, max = nrow(acled.long),
                  value = 30, step = 100, round = 0),
      hr(),
      helpText("Data from Innovation Jetson Google Sheet")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("IncidentPlot"),
      plotOutput("ArrivalsPlot"),
      plotOutput("DeparturesPlot")
    )
    
  )
)
