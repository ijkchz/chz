# Application 1: Use interactive graphics (clicks or brushes) to allow the user
# to better understand a plot. The plot should involve at least one column that
# was originally in the `household` and at least one column from the `project`
# table. table

library(shiny)
library(tidyverse)

load("proj2_homeproj.rda")
name1=homeproject %>% select(-matches("^JOB")) %>% names()
name2=homeproject %>% select(matches("^JOB")) %>% names()
ui <- fluidPage(
  titlePanel("yizheli app1"),
  fluidRow(
    column(3,offset = 0,
           selectInput("x", p("Select x from household"), 
                       name1,
                       selected ='FINCP')),
    column(3,offset = 1,
           selectInput("y", p("Select y from project"), 
                       name2,
                       selected ='JOBCOST')),
    column(3,offset = 1,
           radioButtons("plot", "Select a plot type", c("Point" = 1,
                                           "jitter"=2,
                                           "geom_bin_2d"=3)))
    ),
  sidebarLayout(
    sidebarPanel(
      selectInput("col", p("col"), 
                  choices = c(names(homeproject),'NULL'),
                  selected ='JOBDIY'),
      
      selectInput("factor", p("col is factor?"), 
                  choices = c("num","factor"),
                  selected ='factor'),
      p("The main analysis of the relationship between scatter plots, when the variable overlap is serious can be selected according to the needs of the *jitter chart* or *2d chart*")
      ),
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    name=names(homeproject)
    ind1=as.character(input$x)
    ind2=as.character(input$y)
    ind4=as.character(input$col)
    if(input$factor=="factor"){
      homeproject[[ind4]]<-as.factor(homeproject[[ind4]])}
    if(input$plot==1){
      ggplot(homeproject, aes_string(x=ind1, y=ind2,col=ind4)) +
        geom_point(alpha=0.5) +theme(legend.position = 'bottom')
     }
    else{
      if(input$plot==2){
        ggplot(homeproject, aes_string(group=ind2,x=ind1,y=ind2,col=ind4)) +
          geom_jitter(alpha=0.5)+theme(legend.position = 'bottom')}
      else { 
        ggplot(homeproject, aes_string(group=ind2,x=ind1,y=ind2)) +
          geom_bin_2d()+theme(legend.position = 'bottom')} }
  }, height = 400, width = 600)
}
shinyApp(ui = ui, server = server)
