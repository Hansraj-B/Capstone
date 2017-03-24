library(shiny)
source("PredictNextWord.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    NextWords <- reactive({
        inputtext <- input$txtin
        totchars <- nchar(inputtext)
        result <- character()
        result[1] <- ""
        result[2] <- ""
        if(substr(inputtext,totchars,totchars)==" "){
                nxtwords <- predictNextWord(input$txtin)
               if(nxtwords[4]==""){
                   if(nxtwords[1]==""){ # do nothing
                    }
                    else{
                        result[1] <- "Next possible words are..."
                        result[2] <- paste(nxtwords[1], nxtwords[2],nxtwords[3], sep =" ; ")   
                    }
               }
               else{
                   result[1] <- "Learned a new phrase: "
                   result[2] <- paste(nxtwords[4],sep =" ")
                   
               }
        }
        result
    })
    
    
        output$result <- renderText(NextWords()[1])
        output$term1 <- renderText(NextWords()[2])
        #output$term2 <- renderText(NextWords()[2])
        #output$term3 <- renderText(NextWords()[3])        
    
})
