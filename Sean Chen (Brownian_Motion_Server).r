#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
Server <- shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
        # Take the inputs
        n = input$npt
        T = input$T
        nsim = input$nsim;
        
        # Build up matrices for plotting
        t = seq(0, T, by = T/n)
        x <- matrix(sqrt(T/n)*rnorm(n*nsim, mean=0, sd=1), nrow=nsim)
        xt <- cbind(rep(0, nsim), t(apply(x, 1, cumsum)))
        xtave <- apply(xt,2,mean)
        
        # Visualise the simulations
        plot(t, xt[1, ], xlab = "time 0:T", ylab = "W(t)", 
             ylim=c(-3*sqrt(T), 3*sqrt(T)), type="l", col="blue")
        title(paste(nsim, "simulated paths of Brownian motion with", 
                    n, "points each"))
        if (nsim>1) 
          apply(xt[2:nsim,],1,function(x,t) lines(t,x,col="blue"),t=t)
        if (input$showave)
          lines(t, xtave, type="l", col="red", lwd = 2)
      })
    })

shinyApp(UI, Server)    

  
