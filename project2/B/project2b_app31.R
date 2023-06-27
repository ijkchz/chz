
# Application 3: Create a new column that indicates if a project was "required"
# or not based on the JOBTYPE column. Use this variable to allow the user to
# select either required or non-required jobs and then further select particular
# types of jobs to investigate with a plot.

library(shiny)
library(tidyverse)

load("proj2_homeproj.rda")

ind1<-str_subset(levels(homeproject$JOBTYPE),'required')

ui <- fluidPage(
  # Application title
  titlePanel("Application 3 --Zhenqi Yin"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      p("This is a placeholder"),
      radioButtons("JOB","if project required?",c("required","no_required")),
     selectInput("x", p("Select x from project"), 
                 names(homeproject),
                 selected ='JOBCOST'),
     selectInput("y", p("Select y from project"), 
                 names(homeproject),
                 selected ='JOBTYPE'),
      sliderInput("obs", label = c("Number of head"),
                  min = 1, max = 20, value = 3)
    ),
    # Show a plot of the generated distribution
    mainPanel(
     column(12,offset = 0,
     column(6,offset = 0,
            sliderInput("col", p("change fill"), 
                        min = 1, max = 10, value = 3)),
     column(6,offset = 0,
            sliderInput("fill", p("change col"), 
                        min = 1, max = 10, value = 2))),
      plotOutput(outputId = "distPlot")
    )
  ),
  h4('project data:'),
  tableOutput("summary1")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  requiredd <- reactive({
    if(input$JOB=="required"){
    homeproject %>% filter(JOBTYPE%in%ind1)}
    else{homeproject %>% filter(!JOBTYPE%in%ind1)}
  })
  output$distPlot <- renderPlot({
    ggplot(requiredd(),aes(y=get(input$y),x=get(input$x)))+geom_violin(fill=input$fill,col=input$col)+
      labs(x=input$x,y=input$y)
  },width = 600,height = 300)
  output$summary1 <- renderTable({
    head(requiredd()%>% select(matches("^JOB")),input$obs)
  })


}
# Run the application 
shinyApp(ui = ui, server = server)
