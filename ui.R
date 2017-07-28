# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)
source("http://goo.gl/w64gfp")
library(magrittr)
library(XML)
library(reshape)


# Use the google spreadsheet
u <- "https://drive.google.com/open?id=1VX3XeVXzDWWhcb36jik3k9Whq_gnELw_07L21L_QmnE"

acled <- readGoogleSheet(u) %>% cleanGoogleTable(table=1) %>% as.data.frame

acled$Date <- as.Date(acled$Date, format="%m/%d/%Y")
acled$Day  <- acled$Date-min(acled$Date)

# Force columns to be text
acled[,2:ncol(acled)] <- sapply(acled[,2:ncol(acled)], as.numeric)

# Create totals across countries
# Total Suspected
acled$totalIncidents <- with(acled, Awdal_incl+Bakool_incl+Banaadir_incl+Bari_incl+Bay_incl+Galguduug_incl)


acled.long <- melt(acled, id.vars=c('Date','Day'))

# Create a variable for country
acled.long$Country <- ''
acled.long$Country[grepl("Awdal_incl",acled.long$variable)] <- "Awdal"
acled.long$Country[grepl("Bakool_incl",acled.long$variable)] <- "Bakool"
acled.long$Country[grepl("Banaadir_incl",acled.long$variable)] <- "Banaadir"
acled.long$Country[grepl("Bari_incl",acled.long$variable)] <- "Bari"
odd_indexes<-seq(4,40,2)


# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Incidents by Region"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "Region:", 
                  choices=colnames(acled[odd_indexes])),
      hr(),
      helpText("Data from Innovation Jetson Google Sheet")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("IncidentPlot")  
    )
    
  )
)