library(shiny)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)
library(zoo)
library(rjson)


jscode <- '
  Shiny.addCustomMessageHandler("mymessage", function(message) {
alert(message);
});
'

jetson <- "https://docs.google.com/spreadsheets/d/1oPTPmoJ9phtMOkp-nMB7WHnPESomLzqUj9t0gcE9bYA"
conflicts <- gsheet2text(jetson, sheetid = 819472314)
conflicts.long <- read.csv(text=conflicts)

Dates <- sapply(conflicts.long[,1],as.character.Date)
conflicts.long$Date <- as.Date(conflicts.long$Date, format="%m/%d/%Y")

odd_indexes<-seq(2,19,1)
regions <- colnames(conflicts.long[odd_indexes])
list_regs <- rep(NA,18)
for (i in 1:18){
  list_regs[i] <- strsplit(regions[i],  "_(?=[^_]+$)",perl=TRUE)[[1]][1]
}

list_months <- rep(NA, 6)
list_formonths <- cbind("01/06/2017","01/07/2017","01/08/2017","01/09/2017")
list_months <- cbind("Jul 2017","Aug 2017","Sep 2017")

shinyUI(
  # Use a fluid Bootstrap layout
  fluidPage(  
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src="main.js")
    ),
    
    # Give the page a title
    titlePanel("Predictive Engine"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      # Define the sidebar with one input
      sidebarPanel(
        
        selectInput("region", "Region:", selected="Bay",
                    choices=list_regs),
        selectInput("date", "Month:", selected = "Jul 2017",
                    choices=list_months),
        hr(),
        helpText("The Model is affected by the following parameters:"),
        sliderInput("dep_var_1", "XXX", min = 0, max = 10000,
                    value = 0, step = 100, round = 0),
        sliderInput("dep_var_2", "XXX", min = 0, max = 10000,
                    value = 0, step = 100, round = 0),
        sliderInput("dep_var_3", "XXX", min = 0, max = 10000,
                    value = 0, step = 100, round = 0),
        helpText("Data from Innovation Jetson Google Sheet"),
        tableOutput("datatable")
        #tags$head(tags$script(HTML(jscode)))
      ),
      
      # Create a spot for the barplot
      mainPanel(
        textOutput("arrivals_number"),
        checkboxGroupInput("Indicators", "",
                           c("Future Arrivals", 
                             "Arrivals"),
                           selected=c(
                             "Future Arrivals", 
                             "Arrivals"),
                           inline=TRUE),
        
        plotOutput("graph1"),
        sliderInput("months", "Months", min = (as.Date(min(conflicts.long$Date))+30*9),max =as.Date(max(conflicts.long$Date)),
                    value=c((as.Date(min(conflicts.long$Date))+30*3),as.Date(max(conflicts.long$Date))),timeFormat="%b %Y",width='100%')

      )
      
    )
  )
)
