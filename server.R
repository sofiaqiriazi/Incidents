library(shiny)
library(datasets)
library(magrittr)
library(XML)
library(reshape)
library(gsheet)
library(ggplot2)
library(scales)
library(zoo)
library(rjson)



# Use the google spreadsheet
jetson <- "https://docs.google.com/spreadsheets/d/1oPTPmoJ9phtMOkp-nMB7WHnPESomLzqUj9t0gcE9bYA"
conflicts <- gsheet2text(jetson, sheetid = 819472314)
conflicts.long <- read.csv(text=conflicts)

arrs <-gsheet2text(jetson, sheetid = 457614883)
arrs.long <- read.csv(text=arrs)
#arrs.long <- head(arrs.long, -30)

deps <-gsheet2text(jetson, sheetid = 677621454)
deps.long <-read.csv(text=deps)
#deps.long <- head(deps.long, -30)

rain <-gsheet2text(jetson, sheetid = 1473662223)
rain.long <- read.csv(text=rain,stringsAsFactors = FALSE)
#rain.long <- head(rain.long, -30)

#read the WaterDrumPrices
water <-gsheet2text(jetson, sheetid =27261871)
water.long <- read.csv(text=water,stringsAsFactors = FALSE)

#read the Rivers
rivers <-gsheet2text(jetson, sheetid =407366559)
rivers.long <- read.csv(text=water,stringsAsFactors = FALSE)

#read the Goat Prices
goats <-gsheet2text(jetson, sheetid =1601716765)
goats.long <- read.csv(text=water,stringsAsFactors = FALSE)

#read the Fatalities
fatalities <-gsheet2text(jetson, sheetid =343810263)
fatalities.long <- read.csv(text=water,stringsAsFactors = FALSE)



Dates <- sapply(conflicts.long[,1],as.character.Date)
conflicts.long$Date <- as.Date(conflicts.long$Date, format="%m/%d/%Y")
arrs.long$Date <- as.Date(arrs.long$Date, format="%m/%d/%Y")
deps.long$Date <- as.Date(deps.long$Date, format="%m/%d/%Y")
rain.long$Date <-as.Date(rain.long$Date, format="%m/%d/%Y")
water.long$Date <-as.Date(water.long$Date, format="%m/%d/%Y")
rivers.long$Date <-as.Date(rivers.long$Date, format="%m/%d/%Y")
goats.long$Date <-as.Date(goats.long$Date, format="%m/%d/%Y")
fatalities.long$Date <-as.Date(fatalities.long$Date, format="%m/%d/%Y")

# Force columns to be text
conflicts.long[,2:ncol(conflicts.long)] <- sapply(conflicts.long[,2:ncol(conflicts.long)], as.numeric)
arrs.long[,2:ncol(arrs.long)] <- sapply(arrs.long[,2:ncol(arrs.long)], as.numeric)
deps.long[,2:ncol(deps.long)] <- sapply(deps.long[,2:ncol(deps.long)], as.numeric)
rain.long[,2:ncol(rain.long)] <- sapply(rain.long[,2:ncol(rain.long)], as.numeric)
water.long[,2:ncol(rain.long)] <- sapply(water.long[,2:ncol(rain.long)], as.numeric)
rivers.long[,2:ncol(rain.long)] <- sapply(rivers.long[,2:ncol(rain.long)], as.numeric)
goats.long[,2:ncol(rain.long)] <- sapply(goats.long[,2:ncol(rain.long)], as.numeric)
fatalities.long[,2:ncol(rain.long)] <- sapply(fatalities.long[,2:ncol(rain.long)], as.numeric)

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

date_index <- function(x){
  full.date <- as.POSIXct(x, tz="GMT")
  index <- which(conflicts.long$Date== monthStart(full.date))
  return(index)
}

bay_6A_arrivals <- function(value, start, end, var1=0, var2=0, var3=0){

  PA <- rep(NA, end-start)

  
  for (t in start:end){
    #Bay_Arrival = Shabeellaha_Dhexe_rain +
    if(value==arrs.long[t,"Date"]){
    if(var1!=0){SD_R<-var1}
    else{SD_R <- rain.long[t,"Shabeellaha_Dhexe_rain"]}
      #delay(Woqooyi_Galbeed_Arrival, 1) + 
    if(var2!=0){WG_A<-var2}
    else{WG_A <- arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]}
    #wma(Togdheer_Arrival, 6) +
    if(var3!=0){T_A<-var3}
    else{T_A <- weighted.mean(arrs.long[ (t-6):t, "Togdheer_Arrival"])}
    }
    else{
      SD_R <- rain.long[t,"Shabeellaha_Dhexe_rain"]
      WG_A <- arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
      T_A <- weighted.mean(arrs.long[ (t-6):t, "Togdheer_Arrival"])
    }
    #max(
    #2.84291053988138*Bakool_Arrival + 
    Bak_A <- 2.84291053988138*arrs.long[t,"Bakool_Arrival"]
    #1.90605269499887*Bari_Arrival +
    Bari_A <-  1.90605269499887* arrs.long[t,"Bari_Arrival"]
    #0.582938861571093*Bay_Departures + 
    Bay_D <- 0.582938861571093*deps.long[t,"Bay_Departures"]
    #delay(Woqooyi_Galbeed_Arrival, 1) -
    WG_A2 <- arrs.long[(t-1),"Woqooyi_Galbeed_Arrival"]
    #Shabeellaha_Dhexe_rain*sma(Jubbada_Hoose_Departures, 2) +
    SH_R_JH_D <- -1*rain.long[t,"Shabeellaha_Dhexe_rain"]*mean(deps.long[(t-2):t,"Jubbada_Hoose_Departures"])
    #- 0.0801078676959042*Hiiraan_WaterDrumPrice +
    H_WDP <- -0.0801078676959042*as.numeric(water.long[t, "Hiiraan_WaterDrumPrice"])
    #- 6.62472178142272*Awdal_Departures, 
    A_D <- -6.62472178142272*deps.long[t,"Awdal_Departures"]
    #0.263886473589876*Bay_Departures)
    Bay_D2 <- 0.263886473589876*deps.long[t,"Bay_Departures"]
    
    #Bay_Arrival = Shabeellaha_Dhexe_rain + delay(Woqooyi_Galbeed_Arrival, 1) + wma(Togdheer_Arrival, 6) 
    #+ max(2.84291053988138*Bakool_Arrival + 1.90605269499887*Bari_Arrival + 0.582938861571093*Bay_Departures 
    #+ delay(Woqooyi_Galbeed_Arrival, 1) - (Shabeellaha_Dhexe_rain*sma(Jubbada_Hoose_Departures, 2) )
    #- 0.0801078676959042*Hiiraan_WaterDrumPrice - 6.62472178142272*Awdal_Departures, 0.263886473589876*Bay_Departures)
    
    
    #breaking the function to section max
    PA[t] <- 0
    
    section_max <- max(sum(Bak_A,Bari_A,Bay_D,WG_A2,SH_R_JH_D,H_WDP,A_D,na.rm=TRUE),Bay_D2)
    
    section_add <- sum(SD_R, WG_A, T_A, section_max, na.rm=TRUE)
    
    PA[t] <- section_add
    
  }
  return(PA)
}

shinyServer(function(input, output, session) {
  
  
  produce_slider <- function(x,start,end){
    if(x =="Bay"){
      
      exact_time <- paste(as.character(input$date)," 01") 
      
      value <- as.Date(exact_time, format = "%b %Y %d")
      position <- which(arrs.long$Date == value)
      #session$sendCustomMessage("mymessage", "sofia")

      updateSliderInput(session, "dep_var_1", "Rain in Shabeellaha Dhexe",
                                              #      as.character(rain.long[position,"Shabeellaha_Dhexe_rain"])), 
                        min = 0, max = (max(as.numeric(unlist(rain.long[,"Shabeellaha_Dhexe_rain"])),na.rm=T)) , step = 2)  
      updateSliderInput(session, "dep_var_2", "Arrivals in Woqooyi",
                        min = 0, max = (max(as.numeric(unlist(arrs.long[,"Woqooyi_Galbeed_Arrival"])),na.rm=T)+5000) , step = 2)  
      updateSliderInput(session, "dep_var_3", "Mean of Arrivals in Togdheer",
                        min = 0, max = (max(as.numeric(unlist(arrs.long[,"Togdheer_Arrival"])),na.rm=T)+5000) , step = 2)
    
   
    }
    else if(x!="Bay"){
      updateSliderInput(session, "dep_var_1", "XXX", min = 1, max = 10000,
                        value = 0, step = 100)
      updateSliderInput(session, "dep_var_2", "XXX", min = 1, max = 10000,
                        value = 0, step = 100)
      updateSliderInput(session, "dep_var_3", "XXX", min = 1, max = 10000,
                        value = 0, step = 100)

      output$arrivals_number <- renderText({" "
        #as.character(bay_6A_arrivals(start,end)[position])
      })
      
    }
  }
  
  mydata <- reactive({
    
    # prepare columns for the merged graph
    region <-input$region
    time_start <- date_index(input$months[1])
    time_end <- date_index(input$months[2])
    
    produce_slider(region, time_start, time_end)
    #region <- "Bay"
    
    time_end <- date_index(as.Date("01/08/2017","%d/%m/%Y"))

    reg_con <- paste(region,"Conflict",sep="_")
    reg_arr <- paste(region,"Arrival",sep="_")
    reg_dep <- paste(region,"Departures",sep="_")
    reg_rain <- paste(region,"rain",sep="_")
    
    
    A <- arrs.long[ time_start:time_end, reg_arr ]
    FA <- rep(NA, (time_end-time_start+1))
    pre_final <- length(A)-1
    
    
    final <-length(A)
    
    var1<-as.numeric(input$dep_var_1)
    var2<-as.numeric(input$dep_var_2)
    var3<-as.numeric(input$dep_var_3)

    exact_time <- paste(as.character(input$date)," 01") 
    
    value <- as.Date(exact_time, format = "%b %Y %d")
    position <- which(arrs.long$Date == value)
    
    output$arrivals_number <- renderText({paste("Sofia",as.character(as.integer(bay_6A_arrivals(value,time_start,time_end,var1,var2,var3)[position])))
      
    })
    FA<-bay_6A_arrivals(value,time_start,time_end,var1=var1,var2=var2,var3=var3)[time_start:time_end]
    
    D <- deps.long[ time_start:time_end, reg_dep ]
    extend <- time_end-time_start + 1
    
    
    long <- data.frame(
      Period=rep((1:extend),2),
      Date = conflicts.long$Date[time_start:time_end],
      Population = c(FA, A), 
      Indicator=rep(c("Future Arrivals", 
                      "Arrivals"), 
                    each=(extend)))
    wide <- cbind(FA[time_start:time_end], 
                  A[time_start:time_end])
    list(long=long, wide=wide)
    
    
  })
  
  output$graph1 <- renderPlot({
    
    long <- mydata()[["long"]]
    
    p <- ggplot(long[long$Indicator %in% input$Indicators,], 
                aes(x=Date, y=Population, group=Indicator), fill ="black" ,size=10)    
    # mytheme3 <- theme(legend.text = element_text(face = "italic",colour="#008dfa",
    #                                              family = "Lato", size = rel(1)), 
    #                   axis.title = element_text(colour="#008dfa",family = "Lato",
    #                                             size = rel(1.5)), 
    #                   axis.text = element_text(family = "Lato",colour = "steelblue1",
    #                                            size = rel(1.5)), 
    #                   axis.line = element_line(size = 1,colour = "black"), 
    #                   axis.ticks = element_line(colour="grey",size = rel(0.2)),
    #                   panel.grid.major = element_line(colour="#3f464b",size = rel(0.1)), 
    #                   panel.grid.minor = element_blank(), 
    #                   panel.background = element_rect(fill = "white"), 
    #                   legend.key = element_rect(fill = "whitesmoke"), 
    #                   legend.title = element_text(colour = "#008dfa",size = rel(1.5),
    #                                               family = "Lato"), 
    #                   plot.title = element_text(colour = "#008dfa", face = "bold",
    #                                             size = rel(1.5),family = "Lato"))
    p <- p + 
      geom_line(aes(colour = Indicator), size=1, alpha=.75) + 
      ggtitle("Arrivals, Departures, Incidents as Recorder")+
      scale_x_date(name="Month", date_breaks = "4 month", date_minor_breaks = "1 month", date_labels = "%b %Y")+ 
      scale_y_continuous(labels = comma)
    print(p)
  })
})

