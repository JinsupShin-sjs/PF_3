library(shiny)
ui <- fluidPage(
  fluidRow(
    column(9,div(style = "height:450px;border:4px solid red;","폭 9")),
    column(3,div(style = "height:450px;border:4px solid red;","폭 3")),
    tabsetPanel(
      tabPanel("탭1",
      column(4,div(style = "height:300px;border:4px solid red;","폭 4")),
      column(4,div(style = "height:300px;border:4px solid red;","폭 4")),
      column(4,div(style = "height:300px;border:4px solid red;","폭 4")),),
      tabPanel("탭2", div(style = "height:300px;border:4px solid blue;","폭 12")))))
server <- function(input, output, session){}
shinyApp(ui, server)
