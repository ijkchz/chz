# Application 1: Use interactive graphics (clicks or brushes) to allow the user
# to better understand a plot. The plot should involve at least one column that
# was originally in the `household` and at least one column from the `project`
# table. table

library(shiny)
library(tidyverse)
load("proj2_homeproj.rda")

ui <- fluidPage(
  titlePanel("Application 1 --Zhenqi Yin"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x", h6("Variable of the X-axis"), 
                  c('MARKETVAL'='MARKETVAL',
                    'TOTHCAMT'='TOTHCAMT',
                    'BATHROOMS'='BATHROOMS',
                    'JOBCOST'='JOBCOST'
                  )),
      selectInput("y", h6("Variable of the Y-axis"), 
                  c("BLD" = 'BLD',
                    "HOA" = 'HOA',
                    "HHSEX"="HHSEX",
                    "JOBTYPE"='JOBTYPE',
                    'FINCP'='FINCP')),
      radioButtons("plot", "which kind of plot?(Note the type of the variable)", c("violin" = 1,
                                             "boxplot"=2,
                                             "Point"=3),
                                            selected ='2'),
      selectInput("fill", p("add fill?"), 
                  c("BLD" = 'BLD',
                    "HOA" = 'HOA',
                    "HHSEX"="HHSEX",
                    "JOBTYPE"='JOBTYPE',
                    'NULL'='NULL'),
                    selected ='NULL'),
      selectInput("col", p("add col?"), 
                  c("BLD" = 'BLD',
                    "HOA" = 'HOA',
                    "JOBTYPE"='JOBTYPE',
                    "HHSEX"="HHSEX",
                  'NULL'='NULL'),
                  selected ='NULL')
    ),
    mainPanel(
      p("Based on the variables you choose, you can choose the numeric and categorical variables, add colors, and choose the appropriate graphics, but you want the type of the main variable."),
      br(),
      plotOutput(outputId = "distPlot")
    )
  )
)
server <- function(input, output) {
  output$summary <- renderTable({
    head(homeproject)
  })
  output$distPlot <- renderPlot({
    name=names(homeproject)
    ind1=as.character(input$x)
    ind2=as.character(input$y)
    ind3=as.character(input$fill)
    ind4=as.character(input$col)
    if(input$plot==1){
      ggplot(homeproject, aes_string(group=ind2,x=ind1,y=ind2,fill=ind3,col=ind4)) +
        geom_violin()+theme(legend.position = 'bottom')}
    else{
      if(input$plot==2){
        ggplot(homeproject,aes_string(group=ind2,x=ind1, y=ind2,fill=ind3,col=ind4)) +
          geom_boxplot()+theme(legend.position = 'bottom')}
      else { ggplot(homeproject, aes_string(x=ind1, y=ind2,fill=ind3,col=ind4)) +
          geom_point() +theme(legend.position = 'bottom')} }
  }, height = 400, width = 600)
}
shinyApp(ui = ui, server = server)
