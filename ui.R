# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)
source("http://goo.gl/w64gfp")
library(magrittr)
library(XML)
library(reshape)
library(gsheet)

odd_indexes<-seq(5,39,2)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Data Generation"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "Region:", 
                  choices=colnames(acled.long[odd_indexes])),
      sliderInput("days", "Number of Days", min = 1, max = nrow(acled.long),
                  value = 30, step = 100, round = 0),
      sliderInput("fatalities", "Number of Fatalities", min = 1, max =  max(acled.long["FATALITIES_incl"]),
                  value = 100, step = 100, round = 0),
      sliderInput("arrivals", "Number of Arrivals", min = 1, max =  1000000,
                  value = 10, step = 100, round = 0),
      sliderInput("departures", "Number of Departures", min = 1, max = 1000000,
                  value = 10, step = 100, round = 0),
      hr(),
      helpText("Data from Innovation Jetson Google Sheet"),
      tableOutput("datatable")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      checkboxGroupInput("Indicators", "",
                         c("Incidents", 
                           "Arrivals", 
                           "Departures"),
                         selected=c(
                           "Incidents", 
                           "Arrivals", 
                           "Departures"),
                         inline=TRUE),
      plotOutput("graph1"),
      plotOutput("IncidentPlot"),
      plotOutput("ArrivalsPlot"),
      plotOutput("DeparturesPlot")
    )
    
  )
)
