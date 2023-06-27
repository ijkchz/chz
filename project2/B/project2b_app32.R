
# Application 3: Create a new column that indicates if a project was "required"
# or not based on the JOBTYPE column. Use this variable to allow the user to
# select either required or non-required jobs and then further select particular
# types of jobs to investigate with a plot.

library(shiny)
library(tidyverse)

load("proj2_homeproj.rda")

ind1<-str_subset(levels(homeproject$JOBTYPE),'required')
cag<-which(map_dbl(homeproject,~length(unique(.x)))<40) %>% names()
ui <- fluidPage(
  titlePanel("yizheli app3"),
  fluidRow(
  column(3,offset = 0,
  radioButtons("JOB","if a project was required",c("required","non-required "))),
  column(2,offset = 0,
  selectInput("x", p("Variable x "), 
              names(homeproject),
              selected ='FINCP')),
  column(2,offset = 0,
  selectInput("y", p("Variable y "), 
              cag,
              selected ='JOBTYPE')),
  column(5,offset = 0,
  textAreaInput("story", "Write down your findings, your impressions", rows = 3)),
  plotOutput(outputId = "distPlot"),
  column(4,offset = 0,h4('-----------------------------')),
  column(5,offset = 0,h4('group_by--summarise of data')),
  column(3,offset = 0,h4('------------------------')),
  tableOutput("summary")
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  requiredd <- reactive({
    if(input$JOB=="required"){
      homeproject %>% filter(JOBTYPE%in%ind1)}
    else{homeproject %>% filter(!JOBTYPE%in%ind1)}
  })
  output$distPlot <- renderPlot({
    ggplot(requiredd(),aes(group=get(input$y),y=get(input$y),x=get(input$x)))+geom_boxplot(fill=4,col=1,alpha=0.6)+
      labs(x=input$x,y=input$y)+theme_bw()
  },width=700,height = 280)
  output$summary1 <- renderTable({
    head(requiredd()%>% select(matches("^JOB")),input$obs)
  })
  output$summary <- renderTable({
    out<-requiredd() %>% group_by(category=get(input$y)) %>% summarise(u=mean(get(input$x),na.rm = T),
                                                            med= median(get(input$x),na.rm = T),
                                                            sd= sd(get(input$x),na.rm = T))
    colnames(out)=c(input$y,paste0(input$x,c("_mean","_median","_sd")))
    out
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

