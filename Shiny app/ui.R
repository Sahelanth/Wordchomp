#ui <- fluidPage(
 # textInput(inputId="caption", label="Text Entry", value="Enter text here"),
  #tableOutput("table"),
  #plotOutput("cloud")
#)


# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Wordchomp"),
  
  sidebarLayout(
    mainPanel(
      textInput(inputId="caption", label="Text Entry", value="Enter text here"),
      tableOutput("table")
    ),

    sidebarPanel(plotOutput("cloud"))
    

  )
)
