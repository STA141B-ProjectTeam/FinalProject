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
library(rsconnect)


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
        tabPanel("Best Offers",column(4,tableOutput("offersprice")),column(6, tableOutput("mintime"), tableOutput("maxtime"))),
        tabPanel("All Flights", tableOutput("table")),
        tabPanel("Airport Map",leafletOutput("Map")),
        tabPanel("Flight Time v Price", plotOutput("flightPriceScatterPlot"),verbatimTextOutput("summaryONE")),
        tabPanel("Price Distribution",plotOutput("priceHistoPlot"),verbatimTextOutput("summaryTWO"))
       
      ) #tabsetPanel
    ) #mainPanel
  ) #sidebarLayout
) #ui



#
# Data Access
#

# IF TRUE: Calling API
requestAmadeus <- TRUE


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
  if(requestAmadeus){
    dataAmadeus <-reactive(if(input$return){ 
      #if one-way
      {flight_call(input$origin,input$dest,input$date1)}
    }else{
      #otherwise round trip
      {flight_call(input$origin,input$dest,input$date1,input$date2)}
    })
   
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
  flight_call<- function (origin, destination, date1, date2=NULL) {
    
    # Requesting access token
    headers = c('Content-Type' = 'application/x-www-form-urlencoded')
    res <- httr::POST(url = 'https://test.api.amadeus.com/v1/security/oauth2/token', httr::add_headers(.headers=headers), body = upload_file("1.cred"))
    json <- content(res, as = "text")
    tkn<-fromJSON(json)$access_token
    
    # Creating data request
    if(input$return){ 
      #if one-way omit return information
      req=paste("https://test.api.amadeus.com/v1/shopping/flight-offers?origin=",origin,"&destination=",destination,"&departureDate=",date1, sep = "")
    }else{
      req=paste("https://test.api.amadeus.com/v1/shopping/flight-offers?origin=",origin,
                "&destination=",destination,"&departureDate=",date1,"&returnDate=",date2, sep = "")

    }
    
    # Requesting user authorization
    rr <- GET(req,
              add_headers(Authorization = paste("Bearer ", tkn))
    )
    stop_for_status(rr)
    json <- content(rr, as = "text") 

    x<-  fromJSON(json) 
    #-------------------
    
    # Extracting relevant flights into resdf dataframe
    
    # initialize dataframe
    resdf<- data.frame("price"=.0, "via"="", "totaltime"="", stringsAsFactors=FALSE) 
    
    #if this is a oneway flight
    if(input$return){ 
      length(x$data$offerItems)
      for (k in 1:length(x$data$offerItems)) {

        #extracting data from all matching flights, create variable  
        xx=as.data.frame(x$data$offerItems[k])
        price=xx$price$total
      
        departs=as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments)$flightSegment)$departure)$iataCode
        arrives=as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments)$flightSegment)$arrival)$iataCode
        departs_at <- as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments)$flightSegment)$departure)$at
        arrives_at=as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments)$flightSegment)$arrival)$at
    
        #total travel time calculation
        depart = ymd_hms(departs_at[1])
        arrive =  ymd_hms(tail(arrives_at, n=1))
        travel_time=as.character( seconds_to_period(as.numeric(arrive-depart,units="secs")))
        travel_time=word(travel_time, 1, 2)
      
        # total price calculation for roundtrip
        via=paste(departs[-1],collapse=" ") #pulls origin
        resdf=rbind(resdf,list(as.numeric(price),via,travel_time))
        response=paste("Travel via",via,"      Price is",price) 
      }
      resdf<-resdf[-1,]
      ordered <- resdf[order(resdf$price),]
    
    # otherwise calculate round trip  
    }else{
      data <- x$data$"0"$offerItems$"0"
      length(data)
      for (i in 1:length(x$data$offerItems)) {
        
        #extracting data from all matching flights  
        xx=as.data.frame(x$data$offerItems[i])
        price=xx$price$total
        
        for (j in 0:1)
          departs=as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments[j])$flightSegment)$departure)$iataCode
          arrives=as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments[j])$flightSegment)$arrival)$iataCode        
        
        departs_at <- as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments)$flightSegment)$departure)$at
        arrives_at=as.data.frame(as.data.frame(as.data.frame(as.data.frame(xx$services)$segments)$flightSegment)$arrival)$at
        
        # Roundtrip calculations
        #   total travel time calculation
        depart = ymd_hms(departs_at[1])
        arrive =  ymd_hms(tail(arrives_at, n=1))
        travel_time=as.character( seconds_to_period(as.numeric(arrive-depart,units="secs")))
        travel_time=word(travel_time, 1, 2)
        
        #  total price 
        
        via= paste(departs[-1],collapse=" ") 
        resdf=rbind(resdf,list(as.numeric(price),via,travel_time))
        response=paste("Travel via",via,"      Price is",price) 
      } # end for
      
      resdf<-resdf[-1,]
      ordered <- resdf[order(resdf$price),]
    } #end else
      
  } #end flight call
  
  

  #
  # Tab Content 
  #
  
  #======= Main Tab - TABLE ================
 
  # descriptive text above table
  output$text<-renderText(paste("From ",input$origin," to",input$dest, "on the date of ", dates()[1] ))
  output$text1 <- renderText({if(!input$return)paste("Returning ",dates()[2])})
    
  # all results table
  output$table <- renderTable(dataAmadeus())
  
  
  #======= Scatter Plot ================
  
  # create scatter plot
  output$flightPriceScatterPlot <- renderPlot({
      qplot(y=dataAmadeus()$price,
            x=as.numeric(hm(dataAmadeus()$totaltime))/3600,
            xlab ="Flight Time (hours)", ylab = "Price (US Dollars)",main = "Flight Time vs Price") + 
      theme_minimal() +
      geom_point(shape = 23, fill = "lightgray",color = "black", size = 5) + 
      geom_smooth(method=lm,se=FALSE)

  }) #renderPlot
  
  #=====Scatter Plot Summary Statistics======
  
  
  #======= Histogram ================
  
  output$priceHistoPlot <- renderPlot({
    hist(dataAmadeus()$price,xlab = "Price (US Dollars)",ylab="Frequency",main="Price of Flights",col = 'skyblue3',
           breaks = 100,ylim =c(0,20))
  }) #renderPlot
  
  #=====Histogram Summary Statistics=======
  
  output$summaryTWO<-renderPrint({
      summary(dataAmadeus()$price)

  })
  
  
  #======= Map of Airports ================
  
  Longitude_Latitude <- read_csv("Longitude_Latitude.csv")
  output$Map <- renderLeaflet({
    loc <- Longitude_Latitude %>%
      filter(code == input$origin | code == input$dest)
    
   leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap =TRUE))%>%
      addMarkers(data = loc, label = ~as.character(code), popup = ~as.character(Location))
    }) #render map
  

  #======= Best Offer ================
  minprice <- reactive({
      dataAmadeus() %>% filter(price == min(dataAmadeus()$price))
  })
  
  time <- reactive({
      dataAmadeus() %>%
         mutate(Time = as.numeric(duration(dataAmadeus()$totaltime)))
  })
  
  output$offersprice <- renderTable(
    {minprice()},
    caption = "The cheapest tickets available:",
    caption.placement = getOption("xtable.caption.placement", "top")
  )
  
  output$mintime <- renderTable({
    time() %>%
      filter(price == min(price)) %>%
      filter(Time == min(Time)) %>%
      select(price, via, totaltime)
  },
  caption = "The best estimated flight time:",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  output$maxtime <- renderTable({
    time() %>%
      filter(price == min(price)) %>%
      filter(Time == max(Time)) %>%
      select(price, via, totaltime)
  },
  caption = "The slowest estimated flight time:",
  caption.placement = getOption("xtable.caption.placement", "top"))

  
} #server


#
# Run the application 
shinyApp(ui = ui, server = server)