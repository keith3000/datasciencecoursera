## ui.R
#
library(shiny)

shinyUI(fluidPage(
        titlePanel("My Shiny Temperature Converter"),
        
        sidebarLayout(
                sidebarPanel(
                        sliderInput("slider", label = h3("select temperature"),
                                    min= -100, max = 300, value = 25)
                ),
                mainPanel(
                        h1("Temperature Conversion App"),
                        h2("What temperature do you want to convert from 'C to 'F?"),
                        em(h2(textOutput("text1"))),
                        plotOutput("plot1"),
                        em("graphics to add some interest to the page"),
                        img(src="tempgage.jpg", height = 200, width = 240) 
        )
        ) 
        
))