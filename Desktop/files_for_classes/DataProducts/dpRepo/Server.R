#Server.R
library(shiny)
library(ggplot2)

shinyServer(
        function(input, output) {
               output$text1 <- renderText({
               paste("Your selection of", input$slider, "degrees Celsius is equal to", (input$slider*(9/5)+32) , "degrees Fahrenheit")
        })
                 output$plot1 <- renderPlot({
                plot(input$slider,(input$slider*(9/5)+32))
                abline(v=input$slider)
                abline(h=input$slider*(9/5)+32)
                xlim(-100, max(300))
                })
})



plot(tempdf)