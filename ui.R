# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(shiny)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)

incidents <- "https://drive.google.com/open?id=1VX3XeVXzDWWhcb36jik3k9Whq_gnELw_07L21L_QmnE"
acled <- gsheet2text(incidents)
acled.long <- read.csv(text=acled)



odd_indexes<-seq(5,39,2)
shinyUI(
# Use a fluid Bootstrap layout
  fluidPage(  
  
  tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src="main.js")
  ),
  
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
      sliderInput("futuredays", "Days to Predict", min = 1, max = 30,
                  value = 5, step = 1, round = 0),
      sliderInput("futureconflct", "Average number of Conflicts", min = 1, max = 1000000,
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
      plotOutput("graph2"),
      plotOutput("IncidentPlot"),
      plotOutput("ArrivalsPlot"),
      plotOutput("DeparturesPlot")
    )
    
  )
)
)