# Just do it!

library(shiny)
library(tidyverse)

ui <- fluidPage(
h2("Final project stub",align="center"),
p("Final project stub page."),
    
    
 mainPanel(
  
      textOutput("min")
    )
  )

server <- function(input, output) {
  output$min<-renderText(paste("Hello!"))
  }

shinyApp(ui = ui, server = server)
