# Application 2: Create an application that uses at least two types of input
# widgets (text, slider, date, etc.) and at least two kinds of output (html,
# plot, text, etc).

library(shiny)
library(tidyverse)

load("proj2_homeproj.rda")

ui <- fluidPage(

  titlePanel("Application 2 --Zhenqi Yin"),
  sidebarLayout(
    sidebarPanel(
      p("This is a data filter that may have to filter data built over a certain period of time"),
      textInput("name", "1.What's your name?"),
      textOutput("greeting"),
      selectInput("x", p("2.select  variables to see"), 
                  c(names(homeproject)
                  ),multiple = T),
      sliderInput("obs", label = c("Sets the number of data to be displayed"),
                  min = 1, max = 10, value = 3),
      dateRangeInput("data", "Choose a date range:",
                     start = "1919-1-1",
                     end = "2021-1-1"),
      br(),
      p("You can download the data for the time period you choose"),
      downloadButton("downloadData", "Download")
  ),
    # Show a plot of the generated distribution
    mainPanel(
      p("Raw data presentation"),
      verbatimTextOutput("num"),
      br(),
      h4('Examples of select variables'),
      verbatimTextOutput("summary"),
      hr(),
      h4("Construction time frequency distribution plot"),
      plotOutput(outputId = "distPlot")
    )
  )
)
server <- function(input, output) {
  output$distPlot <- renderPlot({
    new=homeproject %>% filter(YRBUILT%in%c(year(input$data[1]):year(input$data[2])))
    ggplot(new,aes(x=YRBUILT))+geom_histogram(fill=5,col=1)
  }, height = 250, width = 400)
  output$greeting <- renderText({
    paste0("Hello ", input$name, " !Welcome to use ^_^")
  })
  output$num <- renderPrint({
    new=homeproject %>% filter(YRBUILT%in%c(year(input$data[1]):year(input$data[2])))
    paste0('there are ',nrow(new)," row data")
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mydata",".csv", sep = "")
    },
    content = function(file) {
      new=homeproject %>% filter(YRBUILT%in%c(year(input$data[1]):year(input$data[2])))
       write.csv(new, file)
    }
  )
  output$summary <- renderPrint({
    homeproject[1:input$obs,as.character(input$x)]
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
