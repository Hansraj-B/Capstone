##Text prediction Shiny application
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(
            h1("Data Science Capstone: Text Prediction", style="color:maroon"),
            windowTitle = "Data Science Capstone: Text Prediction by HANSRAJ"
      ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        h3("Using the application"),
        hr(style="height:1px;border:none;color:#333;background-color:#333"),
        strong("Try and see the power of data science!!."),
        br(),
        strong("Natural language processing is used to predict the next word as you type. To try the application just enter a phrase in the indicated field and observe the predicted words. If a prediction is not found, the application learns the new phrases and uses it for future prediction. For demo purpose the new phrases learned is not retained in the memory after the application closes.", style="font-family:verdana, color:darkgray"),
        br(),
        br(),
        "Created by: Hansraj Balakrishnan",
        br(),
        "Created on: 12-March-2017"
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       span(textInput("txtin","Enter a phrase here (prediction begins after you hit the space bar)",placeholder="Write something...example good morning"), style="font-size:20px; height:50px;"),
       br(),
       br(),
       span(textOutput("result", inline=TRUE),style="font-size:20px;"), 
       br(),
       span(textOutput("term1", inline=TRUE),style="width:150px; border-style:solid; border-width:2px; border-color:#ffffff ; font-weight: bold; color:black; text-align:center; background-color:#aaffaa"),
       span(textOutput("term2",inline=TRUE),style="width:150px;border-style:solid; border-width:2px; border-color:#ffffff ; font-weight: bold; color:black; text-align:center; background-color:yellow"),
       span(textOutput("term3",inline=TRUE),style="width:150px;border-style:solid; border-width:2px; border-color:#ffffff ; font-weight: bold; color:black; text-align:center; background-color:#ffb6c1")
    )
  )
))
