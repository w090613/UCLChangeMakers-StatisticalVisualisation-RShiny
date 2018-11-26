library(shiny)

# Shiny UI
UI <- shinyUI(fluidPage(
  
  titlePanel("Geometric Brownian Motion - Monte Carlo Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("drift",
                   "Drift Rate (%):",
                   min = 1,
                   value = 15),
      sliderInput("T", "Time T for Each Simulated Path",
                  min = 0,
                  max = 1000,
                  value =500),
      numericInput("stdev",
                   "Yearly Standard Deviation (%)",
                   min = 1,
                   value = 30),
      numericInput("initPrice",
                   "Initial Stock Price",
                   min = 1,
                   value = 100),
      numericInput("simul",
                   "Number of Simulations",
                   min = 1,
                   value = 2),
      checkboxInput("seeds",
                    "Set seed?"),
      numericInput("setseed",
                   "Select number of seed",
                   min = 1,
                   value = 1),
      checkboxInput('showave','showave')
      # submitButton("Submit")
    ),
    
    mainPanel(
      plotOutput("gbm_ts"),
      # headerPanel(withMathJax("$$\\text{GBM Model: } S_0 \\exp\\left(\\mu t + \\sigma W_t\\right) $$")),
      headerPanel(withMathJax("$$\\text{GBM Model: } S_0 \\exp\\left(\\left(\\mu - \\frac{\\sigma^2}{2}\\right)t + \\sigma W_t\\right) $$")),
      h4("To run the simulation you have to enter the following inputs on the side bar:"),
      h4("Initial Stock Price is the current price of the stock;"),
      h4("Drift rate is the expected rate of return;"),
      h4("Yearly Standard Deviation is the volatility of the stock price;"),
      h4("Number of Simulation represents how many simulation of stock price you want to display;"),
      h4("In the side bar is also possible, through a check box, to set the seed to a fix value. Please mark the check box and select the value from the numeric box. If it is unmarked the seed will be assigned randomly.
         As the calculation time increases with the number of simulation, there is a 'Submit' button to click as soon as the parameters are decided.")
      )
    )
  ))

# Shiny Server
Server <- shinyServer(function(input, output) {
  
  output$gbm_ts <- renderPlot({
    if (input$seeds == TRUE) {
      set.seed(input$setseed)
    }
    mu <- input$drift/100
    sigma <- input$stdev/100
    S0 <- input$initPrice
    nsim <- input$simul
    t <- input$T
    
    
    gbm <- data.frame(nrow = t, ncol = nsim)
    for (simu in 1:nsim) {
      for (day in 2:t) {
        epsilon <- rnorm(t)
        dt = 1 / 3650
        gbm[1, simu] <- S0
        gbm[day, simu] <- exp((mu - sigma * sigma / 2) * dt + sigma * epsilon[day] * sqrt(dt))
        # gbm[day, simu] <- exp((mu) * dt + sigma * epsilon[day] * sqrt(dt))
      }
    }
    gbm <- apply(gbm, 2, cumprod)
    # 
    # time <- c(1:t)
    # cbind(gbm, time)
    # gbm_data <- data.frame(gbm, )
    # 
    # p <- plot_ly(x=~1:t, y = ~gbm[,1], mode = 'lines', color = list(col=rainbow(10)), line = list(width = 3))%>%
    #   add_lines(y = ~gbm[,2], mode = 'lines', color = list(col=rainbow(10)), line = list(width = 3)) %>%
    #   layout(xaxis = list(title = 'Time'), yaxis = list(title = 'Stock Price'))
    # 
    ts.plot(gbm, gpars = list(col=rainbow(10)))
    
    xtave <- apply(gbm,1,mean)
    if (input$showave)
      lines(c(1:t), xtave, type="l", col="black", lwd = 3)
    # 
    # plot_ly(x = ~1:t, y = ~gbm[,1], mode = 'lines', color = list(col=rainbow(10)), line = list(width = 3)) %>%
    #   add_lines(y=~gbm[ ,2:ncol(gbm)], mode = 'lines') %>%
    #   layout(xaxis = list(title = "Time"), yaxis = list(title = "Stock Price"))
    # 
    
    # p <- plot_ly(source = "source") %>%
    #   add_lines(x=~1:t, y = ~gbm, type = 'scatter',mode = 'lines', color = list(col=rainbow(10)), line = list(width = 3))%>%
    #   layout(xaxis = list(title = 'Time'), yaxis = list(title = 'Stock Price'))
    
    # # for (j in 1:ncol(gbm)){
    # p <- p %>%
    #     add_lines(y = ~gbm[,2:ncol(gbm)], type = 'scatter',mode = 'lines', color = list(col=rainbow(10)), line = list(width = 3))
    # # }
# 
#     p
    
  })
})

# Run the app
shinyApp(UI, Server)