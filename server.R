# package (which generally comes preloaded).
library(shiny)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)
library(ggplot2)
library(scales)



# Define a server for the Shiny app
shinyServer(function(input, output, session) {
  
  # Use the google spreadsheet
  incidents <- "https://drive.google.com/open?id=1VX3XeVXzDWWhcb36jik3k9Whq_gnELw_07L21L_QmnE"
  arrivals <- "https://drive.google.com/open?id=1HoMZooolfAAlEzyFhWhxxmzcpDqQu_WdBCT7v0o4X-Y"
  departures <- "https://drive.google.com/open?id=1jbwk2rzuq86RVQs0_Jcr_34DLFgdun1ctKc6y5kj_9I"
  rainfall <-"https://drive.google.com/open?id=1DZ8ZIdJnXWWxcpaqjETgvOyOP_dLxXt-V8bBqwTkWOg"
  
  acled <- gsheet2text(incidents)
  acled.long <- read.csv(text=acled)
  #acled.long <- head(acled.long, -30)
  
  arrs <-gsheet2text(arrivals)
  arrs.long <- read.csv(text=arrs)
  #arrs.long <- head(arrs.long, -30)
  
  deps <-gsheet2text(departures)
  deps.long <-read.csv(text=deps)
  #deps.long <- head(deps.long, -30)
  
  
  rain <-gsheet2text(rainfall)
  rain.long <- read.csv(text=rain)
  #rain.long <- head(rain.long, -30)
  
  
  acled.long$Date <- as.Date(acled.long$Date, format="%m/%d/%Y")
  arrs.long$Date <- as.Date(arrs.long$Date, format="%m/%d/%Y")
  deps.long$Date <- as.Date(deps.long$Date, format="%m/%d/%Y")
  rain.long$Date <-as.Date(rain.long$Date, format="%m.%d.%Y")
  
  # Force columns to be text
  acled.long[,2:ncol(acled.long)] <- sapply(acled.long[,2:ncol(acled.long)], as.numeric)
  arrs.long[,2:ncol(arrs.long)] <- sapply(arrs.long[,2:ncol(arrs.long)], as.numeric)
  deps.long[,2:ncol(deps.long)] <- sapply(deps.long[,2:ncol(deps.long)], as.numeric)
  rain.long[,2:ncol(rain.long)] <- sapply(rain.long[,2:ncol(rain.long)], as.numeric)
  
  
  mydata <- reactive({
    # prepare columns for the merged graph
    reg <- paste(strsplit(input$region, "_")[[1]][1],"Arrival",sep="_")
    I <- acled.long[ 0:(input$days),input$region]
    A <- arrs.long[ 0:input$days, reg ]
    D <- deps.long[ 0:(input$days),strsplit(input$region, "_")[[1]][1]]
    
    long <- data.frame(
      Period=rep((1:input$days),3), 
      Population = c(I, A, D), 
      Indicator=rep(c("Incidents", 
                      "Arrivals", 
                      "Departures"), 
                    each=input$days))
    wide <- cbind(I, A, D)
    list(long=long, wide=wide)
    
    
  })

   pred_data <- reactive({
    # prepare columns for the merged graph
    reg <- paste(strsplit(input$region, "_")[[1]][1],"Arrival",sep="_")
    I <- acled.long[ 0:(input$days),input$region]
    A <- arrs.long[ 0:input$days, reg ]
    D <- deps.long[ 0:(input$days),strsplit(input$region, "_")[[1]][1]]
    
    
    rainstart <- nrow(rain.long) - 2*365
    rainend <- rainstart+input$futuredays
    
    R <- rain.long[ rainstart:rainend, reg]
    lengthI <- nrow(I)

    TI<- PA <- PD <- rep(NA, input$futuredays+1)
    
    for (t in 1:input$futuredays){
      newi <-lengthI -30 +t
      #fake function to follow the rainfall in the prediction of July
      TI[t] <- R[t]*35.5 + 10
      if(input$region == "Shabeellaha_Hoose"){
        Sanaag <- arrs.long[0:lengthI, "Sanaag"]
        Bay <- arrs.long[0:lengthI, "Bay"]
        Shabeellaha_Dhexe <- arrs.long[0:lengthI, "Shabeeellaha_Dhexe"]
        
        PA[t] <- 5*Sanaag[newi] + 95.4347826086957*(Bay[newi] + 1.01713148623104*Shabeellaha_Dhexe[newi])^(Sanaag[newi])
      }
      else{
        PA[t] <- 0
      }
      PD[t] <- 0
    }
    
    long <- data.frame(
      Period=rep((1:input$futuredays),3), 
      Population = c(TI, PA, PD), 
      Indicator=rep(c("Incidents", 
                      "Arrivals", 
                      "Departures"), 
                    each=input$futuredays))
    wide <- cbind(I, A, D)
    list(long=long, wide=wide)
    
    
  })
  #Create a datatable with all the values from the inputs
  output$datatable <- renderTable({
      Tdata <- mydata()[["wide"]]
      Tdata <- cbind(day=1:nrow(Tdata), Tdata)
      Tdata[seq(1, nrow(Tdata), length.out=30),]
  })
  
  
  # Fill in the spot we created for a plot
  output$IncidentPlot <- renderPlot({
    
    
    my_vector = c(acled.long[ 1:(input$days),input$region])
    names(my_vector) = c(acled.long[1:(input$days),"Date"])
    # Render a barplot
    barplot(my_vector,
            ylab="Number of days",
            xlab="Incidents")
  })
  
  output$ArrivalsPlot <- renderPlot({
    
    reg <- paste(strsplit(input$region, "_")[[1]][1],"Arrival",sep="_")
    arrivals_vector = c(arrs.long[ 1:input$days, reg ])
    names(arrivals_vector) = c(arrs.long[1:input$days,"Date"])
    
    barplot(arrivals_vector,
            ylab = "Date",
            xlab = "Arrivals")
    
  })
  
  output$DeparturesPlot <- renderPlot({
    
    departures_vector = c(deps.long[ 1:(input$days),strsplit(input$region, "_")[[1]][1]])
    names(departures_vector) = c(deps.long[1:(input$days),"Date"])
    
    barplot(departures_vector,
            ylab = "Date",
            xlab = "Departures")
    
  })
  
  output$graph1 <- renderPlot({
    
    long <- mydata()[["long"]]
    p <- ggplot(long[long$Indicator %in% input$Indicators,], 
                aes(x=Period, y=Population, group=Indicator))    
    p <- p + 
      geom_line(aes(colour = Indicator), size=1, alpha=.75) + 
      ggtitle("Total Data")+
      scale_x_continuous(name="Days")+ 
      scale_y_continuous(labels = comma, name="")
    print(p)
  })
  
  output$graph2 <- renderPlot({
    
    long <- mydata()[["long"]]
    p <- ggplot(long[long$Indicator %in% input$Indicators,], 
                aes(x=Period, y=Population, group=Indicator))    
    p <- p + 
      geom_line(aes(colour = Indicator), size=1, alpha=.75) + 
      ggtitle("Predictions")+
      scale_x_continuous(name="Days")+ 
      scale_y_continuous(labels = comma, name="")
    print(p)
  })
}

)
