library(shiny)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(shinyjs)
#library(readr)
#library(stringr)
library(httr)

#d=c("NYC","LGA")
airport_codes <- read_csv("airport-codes1.csv")
d=airport_codes$code
#o=c("SMF","SFO","LAX")
o=d

ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  titlePanel("Final Project"),
  
  sidebarLayout(
    sidebarPanel( p("To get actual flight prices, please, choose departure and arrival airports."),
                  selectInput("origin", "Origin:", o, selected = "JFK"), 
                  selectInput("dest", "Destination:", d, selected = "SFO"),
                  dateInput("date1", "Departure Date:", value = "2020-04-29"),
                  dateInput("date2", "Return Date:", value = "2020-05-29"),
                  
                  submitButton("Update View", icon("refresh")),
                  
                  checkboxInput("return", "Check if one-way", TRUE),
    ), #sidebarPanel
    
    
    mainPanel(
      p("Here you can see actual prices for your origin and destination for selected dates"),
      textOutput("text"),
      textOutput("text1"),
      
      # tableOutput("table"),
      #  plotOutput("plot", inline = FALSE),
      tabsetPanel(
        tabPanel("Table (1)", tableOutput("table")),
        tabPanel("Plot (1)", plotOutput("plot")),
        tabPanel("(2)",plotOutput("user2")) ,
        tabPanel("(3)",textOutput("user3")),
        tabPanel("(4)",textOutput("user4"))
        
      ) #tabsetPanel
    ) #mainPanel
  ) #sidebarLayout
) #fluidPage


#do you want to use online Amadeus data or just stored data dA.Rdata from earlier request?
#set requestAmadeus to FALSE if you want to requests to Amadeus (for testing purposes)
#set requestAmadeus to TRUE for online requests

load(file = "dA.Rdata")
requestAmadeus <- FALSE

server <- function(input, output) {
  
  fromTo <- reactive({c(input$origin,input$dest)})
  dates<- reactive({c(input$date1,input$date2)})
  
  #checking if accessing Amadeus is required and preparing data 
  #if(is.null(dA) | requestAmadeus ){
  if(requestAmadeus ){
    dataAmadeus <-reactive({ doit(input$origin,input$dest,input$date1)})
    #isolate ({dAtemp<-dataAmadeus()})
    #dA<-dAtemp
    #save(dA, file = "dA.Rdata")
  }
  
  observeEvent(input$return, {
    toggleState("date2")
  })
  #doit function
  #establishing connection, getting response, parsing response 
  doit=function (origin, destination, date) {
    #requesting access token
    headers = c('Content-Type' = 'application/x-www-form-urlencoded')
    res <- httr::POST(url = 'https://test.api.amadeus.com/v1/security/oauth2/token', httr::add_headers(.headers=headers), body = upload_file("1.cred"))
    json <- content(res, as = "text")
    tkn<-fromJSON(json)$access_token
    
    #creating data request
    # req=paste("https://test.api.amadeus.com/v1/shopping/flight-offers?origin=",input$origin,"&destination=",input$dest,"&departureDate=",input$date1, sep = "")
    req=paste("https://test.api.amadeus.com/v1/shopping/flight-offers?origin=",origin,"&destination=",destination,"&departureDate=",date, sep = "")
    
    #getting chipest date       
    #req="https://test.api.amadeus.com/v1/shopping/flight-dates?origin=MAD&destination=MUC"
    
    #requesting data
    rr <- GET(req,
              add_headers(Authorization = paste("Bearer ", tkn))
    )
    stop_for_status(rr)
    json <- content(rr, as = "text")
    x<-  fromJSON(json) #x1=x$data$offerItems[1];y=as.data.frame(x1)
    
    #-------------------
    #extracting data from responce into resdf dataframe
    #creating initial dataframe
    resdf<- data.frame("price"=.0, "via"="", "totaltime"="", stringsAsFactors=FALSE) 
    
    length(x$data$offerItems)
    for (k in 1:length(x$data$offerItems)) {
      #extgracting data from all of the offers  
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
      responce=paste("Travel via",via,"      Price is",price) 
    }
    resdf<-resdf[-1,]
    ordered <- resdf[order(resdf$price),]
    #as.character(ordered)
    
    
  } # end of doit()
  
  # output$text<-renderText(paste("From ",input$origin," to",input$dest,unlist(doit(input$origin,input$dest))))
  output$text<-renderText(paste("From ",input$origin," to",input$dest, "on the date of ", dates()[1] ))
  output$text1 <- renderText({if(!input$return)"not implemented"})
  #output$table <- renderTable(doit(input$origin,input$dest,input$date1))   
  
  # t=doit("SMF","JFK","2020-03-01")
  # t$price
  # output$table <- renderTable(t)
  
  if(requestAmadeus)output$table <- renderTable(dataAmadeus())
  else output$table <- renderTable(dA)
  output$plot <- renderPlot({
    #--  t=doit(input$origin,input$dest,input$date1)
    #qplot(y=t$price,x=as.numeric(hm(t$totaltime))/3600,xlab ="flight time (hours)", ylab = "price")
    if (requestAmadeus)qplot(y=dataAmadeus()$price,x=as.numeric(hm(dataAmadeus()$totaltime))/3600,xlab ="flight time (hours)", ylab = "price")
    else qplot(y=dA$price,x=as.numeric(hm(dA$totaltime))/3600,xlab ="flight time (hours)", ylab = "price")
    
  }) #renderPlot
  
  #======= user 2 ================
  output$user2 <- renderPlot({
    if (requestAmadeus)hist(dataAmadeus()$price)else hist(dA$price,xlab = "Price",ylab="Frequency")  
  }) #renderPlot
  
  #======= user 3 ================
  output$user3 <- renderText({"user 3's work"})
  
  #======= user 4 ================
  output$user4 <- renderText({"user 4's work"})
} #server

# Run the application 
shinyApp(ui = ui, server = server)

