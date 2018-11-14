#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
set.seed(1000)
# Define server logic required to draw a histogram
Server <- shinyServer(function(input, output) {
    
    output$bin_plot <- renderPlot({
        
        # generate bins based on input$bins from Binomial distribution.R
        x    <- rbinom(n=input$sample_size,size=input$n,prob=input$p)
        bins <- seq(min(x), max(x))
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = input$cl, border = 'white',freq = FALSE,main="Binomial Distribution")
        
        
        # generate bins based on input mu from Binomial distribution.R
    output$normal_plot <- renderPlot({
            y <- rnorm(n=input$sample_size,mean=input$n * input$p,sd=sqrt(input$n * input$p*(1-input$p)))
            
            # draw the plot of normal distribution
            plot(density(y),type="l",col=input$cl,main = "Normal Distribution")
        })
        
    })
    
})

shinyApp(UI, Server)
