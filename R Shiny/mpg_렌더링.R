library(shiny)
ui <- fluidPage(
  sliderInput("range", "연비", min = 0, max =35, value = c(0,10)),
  textOutput("value"))
server <- function(input, output, session){
  output$value <-  