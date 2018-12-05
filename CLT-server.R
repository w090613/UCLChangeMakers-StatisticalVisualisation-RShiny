#CLT_server
server <- function(input, output, session){
  
  #Defining population function for storing population data
  population <- reactive({ #executes the code inside {} and render data to population object
    if (input$dist == "norm") {rnorm(input$pop_size,mean=input$miu,sd=input$sigma)}
    else if (input$dist == "unif") {runif(input$pop_size,min=input$a,max=input$b)}
    else if (input$dist == "exp") {rexp(input$pop_size, rate = input$lambda)}
    else if (input$dist == "binom") {rbinom(input$pop_size, size=input$n, prob=input$p)}
    else if (input$dist == "nbinom") {rnbinom(input$pop_size, size=input$r, prob=input$p2)}
    else if (input$dist == "pois") {rpois(input$pop_size, lambda=input$lambda2)}
    else if (input$dist == "geom") {rgeom(input$pop_size, prob=input$p3)}
    else if (input$dist == "hyper") {rhyper(input$pop_size,m=input$M, n=input$N, k=input$K)}
    else if (input$dist == "chisq") {rchisq(input$pop_size,df=input$df)}
    else if (input$dist == "t") {rt(input$pop_size, df=input$df2)}
    else if (input$dist == "beta") {rbeta(input$pop_size, shape1=input$Alpha, shape2=input$Beta)}
    else if (input$dist == "gamma") {rgamma(input$pop_size, shape=input$k, scale=input$Theta)}
    
  }
  )
  
  #Defining sample mean function for storing sample mean data
  smpl_mean <- reactive({ #executes the code inside {} and render data to smpl_mean object
    for (i in 1:input$smpl_iterate) {
      if (i==1) {
        smpl_mean <- c(mean(sample(population(), input$smpl_size, replace = TRUE ))) #creating object for the first time
      } else {
        smpl_mean <- c(smpl_mean,mean(sample(population(), input$smpl_size, replace = TRUE ))) #apending data to existing smpl_mean object
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
    plot(density(population()),axes=FALSE,xlab="",ylab="",main="") #density plot
    par(new=TRUE) #new plot should not clean the frame before drawing as if it were on a new device
    hist(population(), main="Population histogram and density plot", xlab = "") #ploting histogram
    abline(v = mean(population()), col = "blue", lwd = 2) #ploting straight vertical blue line for mean
  })
  
  #Rendering sample plot
  output$plot_smpl_mean <-renderPlot({
    plot(density(smpl_mean()),axes=FALSE,xlab="",ylab="",main="")
    par(new=TRUE)
    hist(smpl_mean(), main="Sample mean histogram and density plot", xlab = "")
    abline(v = mean(smpl_mean()), col = "blue", lwd = 2)
  })
  
}