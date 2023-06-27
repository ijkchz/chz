# Application 2: Create an application that uses at least two types of input
# widgets (text, slider, date, etc.) and at least two kinds of output (html,
# plot, text, etc).


library(shiny)
library(tidyverse)

load("proj2_homeproj.rda")

ui <- fluidPage(
  
  titlePanel("yizheli app2"),
  fluidRow(
    column(2,offset = 0,
      selectInput("x", p("select  variables of x"), 
                  names(homeproject),
                  selected ='JOBCOST'
                  )),
    column(2,offset = 0,
           selectInput("y", p("select variables of y"), 
                  names(homeproject),
                  selected ='FINCP'
                       )),
    column(2,offset = 0,
           selectInput("c", p("Categorical variable"), 
                       names(homeproject),
                       selected ='HOA'
           )),
    column(4,offset = 0,
           
           selectInput("var", p("select  variables to show"), 
                       names(homeproject),
                       selected =c('JOBCOST','FINCP','HOA'),multiple = T
           ),br()),
    column(2,offset = 0,
           numericInput("obs", label = c("Display quantity"),
                        value = 3, min = 0, max = 20)),
    column(6,offset = 0,
          sliderInput("data", label = c("Choose the year of construction"),
                       value = c(1980,2010), min = 1919, max = 2022),
                       plotOutput(outputId = "distPlot")
           ),
    column(6, offset = 0,
           p('1.The index is calculated by grouping'),
           tableOutput("summary"),
           p('2.examlp of data'),
           tableOutput("summary2"),
           textAreaInput("story", "Document your findings", rows = 3))
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

 new <- reactive({
      homeproject %>% filter(YRBUILT%in%c(input$data[1]:input$data[2]))
      })
  output$distPlot <- renderPlot({
    ggplot(new(),aes(col=factor(get(input$c)),x=get(input$x),y=get(input$y)))+
      geom_point()+theme_bw()+theme(legend.position = 'bottom')
  })
  output$greeting <- renderText({
    paste0("Hello ", input$name, " !Welcome to use ^_^")
  })
  ?renderPlot
  output$num <- renderPrint({
     paste0('there are ',nrow(new())," row data")
  })
  output$summary <- renderTable({
    a<-new() %>% group_by(get(input$c)) %>% summarise(n=n(),u=mean(get(input$y),na.rm = T),
                                                   med= median(get(input$y),na.rm = T),
                                                   sd= sd(get(input$y),na.rm = T))
  colnames(a)[1]=input$c
  a
    })
output$summary2 <- renderTable({
  dat=new()
  dat[1:input$obs,as.character(input$var)]
  })
}
 
shinyApp(ui = ui, server = server)
