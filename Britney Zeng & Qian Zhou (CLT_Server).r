#CLT_server
server <- function(input, output, session){
  
  #Defining population function for storing population data
  population <- reactive({ #executes the code inside {} and render data to population object
    if (input$dist == "norm") {rnorm(input$pop_size,mean=as.numeric(input$miu),sd=as.numeric(input$sigma))}
    else if (input$dist == "unif") {runif(input$pop_size,min=as.numeric(input$a),max=as.numeric(input$b))}
    else if (input$dist == "exp") {rexp(input$pop_size, rate = as.numeric(input$lambda))}
    else if (input$dist == "binom") {rbinom(input$pop_size, size=as.numeric(input$n), prob=as.numeric(input$p))}
    else if (input$dist == "nbinom") {rnbinom(input$pop_size, size=as.numeric(input$r), prob=as.numeric(input$p2))}
    else if (input$dist == "pois") {rpois(input$pop_size, lambda=as.numeric(input$lambda2))}
    else if (input$dist == "geom") {rgeom(input$pop_size, prob=as.numeric(input$p3))}
    else if (input$dist == "hyper") {rhyper(input$pop_size,m=as.numeric(input$M), n=as.numeric(input$N), k=as.numeric(input$K))}
    else if (input$dist == "chisq") {rchisq(input$pop_size,df=as.numeric(input$df))}
    else if (input$dist == "t") {rt(input$pop_size, df=as.numeric(input$df2))}
    else if (input$dist == "beta") {rbeta(input$pop_size, shape1=as.numeric(input$Alpha), shape2=as.numeric(input$Beta))}
    else if (input$dist == "gamma") {rgamma(input$pop_size, shape=as.numeric(input$k), scale=as.numeric(input$Theta))}
    
  }
  )
  
  #Defining sample mean function for storing sample mean data
  smpl_mean <- reactive({ #executes the code inside {} and render data to smpl_mean object
    for (i in 1:input$smpl_size) {
      if (i==1) {
        smpl_mean <- c(mean(sample(population(), input$smpl_iterate, replace = TRUE ))) #creating object for the first time
      } else {
        smpl_mean <- c(smpl_mean,mean(sample(population(), input$smpl_iterate, replace = TRUE ))) #apending data to existing smpl_mean object
      }
    }
    smpl_mean #printing smpl_mean object in order to return via reactive function to main smpl_mean object
  })
  
  #Rendering summary statistics and data information of population and sample mean data
  output$pop_summary <- renderPrint({summary(population())})
  output$pop_structure <- renderPrint({str(population())})
  output$smpl_mean_summary <- renderPrint({summary(smpl_mean())})
  output$smpl_mean_structure <- renderPrint({str(smpl_mean())})
  
  #Rendering population plot
  output$plot_pop <-renderPlot({
    plot(density(population()),axes=FALSE,xlab="",ylab="",main="", col="blue",lwd=2) #density plot
    par(new=TRUE) #new plot should not clean the frame before drawing as if it were on a new device
    hist(population(), freq = FALSE,main="Population Histogram & Density Plot", xlab = "") #ploting histogram
    abline(v = mean(population()), col = "red", lwd = 2) #ploting straight vertical red line for mean
    text(x=mean(population()), y=0,labels="Mean",col="red")
  })
  
  #Rendering sample plot
  output$plot_smpl_mean <-renderPlot({
    par(mfrow = c(1, 2))
    max_y<-max(hist(smpl_mean(),plot = FALSE)$density,
               dnorm(seq(min(smpl_mean()), max(smpl_mean()), 0.0001),mean(smpl_mean()), sd(smpl_mean())))
    hist(smpl_mean(), freq = FALSE, main="Sample Mean Histogram",  
         cex.main = 1.25, ylim=c(0,max_y), xlab="Blue line = N(sample mean, sample variance)")
    abline(v = mean(smpl_mean()), col = "red", lwd = 2)
    text(x=mean(population()), y=0,labels="Mean",col="red")
    lines(x = seq(min(smpl_mean()), max(smpl_mean()), 0.0001), 
          y = dnorm(seq(min(smpl_mean()), max(smpl_mean()), 0.0001),mean(smpl_mean()), sd(smpl_mean())),
          col = "blue", lwd = 2)
    qqnorm(smpl_mean())
    qqline(smpl_mean(), col = "blue",lwd=2)
  })
}

shinyApp(ui, server)
