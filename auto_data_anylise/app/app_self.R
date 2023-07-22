library(shiny)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("App11"),
  tabsetPanel(
    tabPanel(
      "模块1",
      titlePanel("Top gen by limmer"),
      a(herf = "www,http://rstudio.com", "RStudio Home "),
      #网页
      hr(),
      #横线
      br(),
      #空行
      sidebarLayout(
        sidebarPanel(sliderInput(
          "bins",
          "Number of bins:",
          min = 1,
          max = 50,
          value = 30
        )),
        
        # Show a plot of the generated distribution
        mainPanel(
          #插入图片
          img(
            src = "rstudio.png",
            height = 80,
            width = 200
          ),
          img(
            src = "甘雨.jpeg",
            height = 80,
            width = 200
          ),
          plotOutput("distPlot"),
          tags$a("超链接", href = "https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/")
        )
      )
    ),
    tabPanel(
      "输入框架",
      titlePanel("Basic widgets"),
       fileInput("file1", "New CSV File",
                 multiple = TRUE,
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
      tableOutput('tab1')
    ),
    tabPanel(
      "输出框架",
      titlePanel("Top gen by limmer"),
       fluidRow(
        column(
          3,
          p("我的github网页:https://github.com/ijkchz/chz.git")
        )
      )
    ),
    tabPanel("888",
             numericInput("steps", "How many steps?", 10),
             actionButton("go", "go"),
             textOutput("result")
             )
  ))
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$tab1 <- renderTable({
    req(input$file1)
    dat<- read_csv(input$file1$datapath)
    dat
  })
    data <- eventReactive(input$go, {
      withProgress(message = "Computing random number", {
        for (i in seq_len(input$steps)) {
          Sys.sleep(0.5)
          incProgress(1 / input$steps)
        }
        runif(1)
      })
    }) 
      output$result <- renderText(round(data(), 2))
}

# Run the application
shinyApp(ui = ui, server = server)
