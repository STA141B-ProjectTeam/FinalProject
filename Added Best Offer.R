library(shiny)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(shinyjs)
library(readr)
library(stringr)
library(httr)
library(ggplot2)
library(leaflet)
library(magrittr)

#
# Airport Data
#

airport_codes <- read_csv("airport-codes1.csv")
d <- airport_codes$code # destination codes
o <- d                  # origin codes will need the same list   

#
# App Body
#
ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  
  #
  # Title Panel  
  titlePanel("Final Project"),
  
  #
  # Main Page SideBar: Flight Selection
  sidebarLayout(
    sidebarPanel( p("To get actual flight prices, please, choose departure and arrival airports."),
                  selectInput("origin", "Origin:", o, selected = "JFK"), 
                  selectInput("dest", "Destination:", d, selected = "SFO"),
                  dateInput("date1", "Departure Date:", value = "2020-04-29"),
                  dateInput("date2", "Return Date:", value = "2020-05-29"),      # For round trip implementation
                  
                  submitButton("Update View", icon("refresh")),
                  checkboxInput("return", "Check if one-way", TRUE),
    ), #sidebarPanel
    
    #
    # Main Page Body: Flight Results 
    mainPanel(
      p("Here you can see actual prices for your origin and destination for selected dates"),
      textOutput("text"),
      textOutput("text1"),
      
      #
      # Tab Conditions: Name and Content for each tab
      #    See TAB CONTENT below, we're plucking those output variables and using them here. 
      tabsetPanel(
        tabPanel("Results Table", tableOutput("table")),
        tabPanel("Flight Time v Price", plotOutput("flightPriceScatterPlot"),verbatimTextOutput("summaryONE")),
        tabPanel("Price Distribution",plotOutput("priceHistoPlot"),verbatimTextOutput("summaryTWO")),
        tabPanel("Map of Airports",leafletOutput("Map")),
        tabPanel("Best Offers",column(4,tableOutput("offersprice")),column(6, tableOutput("mintime"), tableOutput("maxtime")))
        
        
        
      ) #tabsetPanel
    ) #mainPanel
  ) #sidebarLayout
) #fluidPage



#
# Data Access
#

#SETUP README: Do you want to use online Amadeus data or just stored data dA.Rdata from earlier request?
#    set requestAmadeus to FALSE if you want to requests to Amadeus (for testing purposes)
#    set requestAmadeus to TRUE for online requests

load(file = "dA.Rdata")  # if request False, local data (static table)
requestAmadeus <- FALSE


#
# Data Retrieval and Variable Assignment
#

server <- function(input, output) {
  
  
  #
  # User Inputs Setup: 
  fromTo <- reactive({c(input$origin, input$dest)}) # Origin and Destinations
  dates<- reactive({c(input$date1, input$date2)})   # One-way and roundtrip dates
  
  
  #
  # Checking if accessing Amadeus is required and preparing data 
  #if(is.null(dA) | requestAmadeus ){
  if(requestAmadeus ){
    dataAmadeus <-reactive({ doit(input$origin,input$dest,input$date1)})
    #isolate ({dAtemp<-dataAmadeus()})
    #dA<-dAtemp
    #save(dA, file = "dA.Rdata")
  }
  
  # 
  # If one-way flight gray out return date
  observeEvent(input$return, {
    toggleState("date2")
  })
  
  
  #
  # API Call 
  #
  
  #
  # Establishing connection, getting response, parsing response 
  doit<- function (origin, destination, date) {
    
    # Requesting access token
    headers = c('Content-Type' = 'application/x-www-form-urlencoded')
    res <- httr::POST(url = 'https://test.api.amadeus.com/v1/security/oauth2/token', httr::add_headers(.headers=headers), body = upload_file("1.cred"))
    json <- content(res, as = "text")
    tkn<-fromJSON(json)$access_token
    
    # Creating data request
    # req=paste("https://test.api.amadeus.com/v1/shopping/flight-offers?origin=",input$origin,"&destination=",input$dest,"&departureDate=",input$date1, sep = "")
    req=paste("https://test.api.amadeus.com/v1/shopping/flight-offers?origin=",origin,"&destination=",destination,"&departureDate=",date, sep = "")
    
    # Get cheapest date       
    #req="https://test.api.amadeus.com/v1/shopping/flight-dates?origin=MAD&destination=MUC"
    
    # Requesting data
    rr <- GET(req,
              add_headers(Authorization = paste("Bearer ", tkn))
    )
    stop_for_status(rr)
    json <- content(rr, as = "text")
    x<-  fromJSON(json) #x1=x$data$offerItems[1];y=as.data.frame(x1)
    
    #-------------------
    
    # Extracting data from response into resdf dataframe
    # Creating initial dataframe
    resdf<- data.frame("price"=.0, "via"="", "totaltime"="", stringsAsFactors=FALSE) 
    
    length(x$data$offerItems)
    for (k in 1:length(x$data$offerItems)) {
      
      #extracting data from all of the offers  
      xx=x$data$offerItems[k]
      xx=as.data.frame(xx)
      price=xx$price$total
      departs=as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments)$flightSegment)$departure)$iataCode
      departs_at=as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments)$flightSegment)$departure)$at
      arrives=as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments)$flightSegment)$arrival)$iataCode
      arrives_at=as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments)$flightSegment)$arrival)$at
      
      #total travel time calculation
      depart = ymd_hms(departs_at[1])
      arrive =  ymd_hms(tail(arrives_at, n=1))
      travel_time=as.character( seconds_to_period(as.numeric(arrive-depart,units="secs")))
      travel_time=word(travel_time, 1, 2)
      
      
      via=paste(departs,collapse=" ")
      resdf=rbind(resdf,list(as.numeric(price),via,travel_time))
      response=paste("Travel via",via,"      Price is",price) 
    }
    resdf<-resdf[-1,]
    ordered <- resdf[order(resdf$price),]
    #as.character(ordered)
    
    
  } # end of doit()
  
  #
  # Data Outputs
  #
  
  # output$text<-renderText(paste("From ",input$origin," to",input$dest,unlist(doit(input$origin,input$dest))))
  output$text<-renderText(paste("From ",input$origin," to",input$dest, "on the date of ", dates()[1] ))
  output$text1 <- renderText({if(!input$return)"not implemented"})
  #output$table <- renderTable(doit(input$origin,input$dest,input$date1))   
  
  # t=doit("SMF","JFK","2020-03-01")
  # t$price
  # output$table <- renderTable(t)
  
  #
  # Tab Content 
  #
  
  #======= Main Tab - TABLE ================
  
  if(requestAmadeus) # Where is your data from? Return respective table
    output$table <- renderTable(dataAmadeus())
  else 
    output$table <- renderTable(dA)
  
  #======= Scatter Plot ================
  
  output$flightPriceScatterPlot <- renderPlot({
    #--  t=doit(input$origin,input$dest,input$date1)
    #qplot(y=t$price,x=as.numeric(hm(t$totaltime))/3600,xlab ="flight time (hours)", ylab = "price")
    if (requestAmadeus)
      qplot(y=dataAmadeus()$price,
            x=as.numeric(hm(dataAmadeus()$totaltime))/3600,
            xlab ="Flight Time (hours)", ylab = "Price (US Dollars)",main = "Flight Time vs Price",ylim = c(100,200)) + 
      theme_minimal() +
      geom_point(shape = 23, fill = "lightgray",color = "black", size = 5) + 
      geom_smooth(method=lm,se=FALSE)
    else 
      qplot(y=dA$price,
            x=as.numeric(hm(dA$totaltime))/3600,
            xlab ="Flight Time (hours)", ylab = "Price (US Dollars)",main = "Flight Time vs Price",ylim = c(100,200))+ 
      theme_minimal() +
      geom_point(shape = 23, fill = "lightgray",color = "black", size = 5) + 
      geom_smooth(method=lm,se=FALSE)
    
    
  }) #renderPlot
  
  #=====Scatter Plot Summary Statistics======
  
  
  #======= Histogram ================
  
  output$priceHistoPlot <- renderPlot({
    if (requestAmadeus)
      hist(dataAmadeus()$price,xlab = "Price (US Dollars)",ylab="Frequency",main="Price of Flights",col = 'skyblue3',
           breaks = 100,ylim =c(0,20))
    else 
      hist(dA$price,xlab = "Price (US Dollars)",ylab="Frequency",main="Price of Flights",col = 'skyblue3',
           breaks = 500,ylim =c(0,20))  
  }) #renderPlot
  
  #=====Histogram Summary Statistics=======
  
  output$summaryTWO<-renderPrint({
    if(requestAmadeus)
      summary(dataAmadeus()$price)
    else
      summary(dA$price)
  })
  
  
  #======= Map of Airports ================
  Longitude_Latitude <- read_csv("Longitude_Latitude.csv")
  output$Map <- renderLeaflet({
    if(requestAmadeus)
      leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE))%>%
      addMarkers(data = Longitude_Latitude,popup = ~as.character(code), label = ~as.character(Location))
    else
      leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = Longitude_Latitude,popup = ~as.character(code), label = ~as.character(Location)) 
    
  })
  
  #======= Best Offer ================
  minprice <- reactive({
    if (requestAmadeus)
      dataAmadeus() %>% filter(price == min(dataAmadeus()$price))
    else
      dA %>% filter(price == min(dA$price))
  })
  time <- reactive({
    if (requestAmadeus)
      dataAmadeus() %>% 
        mutate(Time = as.numeric(hm(dataAmadeus()$totaltime))/3600)
    else
      dA %>% 
        mutate(Time = as.numeric(hm(dA$totaltime))/3600)
  })
  
  output$offersprice <- renderTable(
    {minprice()},
    caption = "The cheapest tickets available:",
    caption.placement = getOption("xtable.caption.placement", "top")
  )
  
  output$mintime <- renderTable({
    time() %>%
      filter(Time == min(Time)) %>%
      select(price, via, totaltime)
  },
  caption = "The best estimated flight time:",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$maxtime <- renderTable({
    time() %>%
      filter(Time == max(Time)) %>%
      select(price, via, totaltime)
  },
  caption = "The slowest estimated flight time:",
  caption.placement = getOption("xtable.caption.placement", "top"))
} #server


#
# Run the application 
shinyApp(ui = ui, server = server)