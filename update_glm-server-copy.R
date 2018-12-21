# Define server logic required to draw a histogram
Server <- shinyServer(function(input, output, session) {
  
  # Build glm models and predict values based on that
  glm1 <- reactive({
    input_x <- input$x
    input_y <- input$y
    indVar <- myData[,which(colnames(myData) == input_x)]
    depVar <- myData[,which(colnames(myData) == input_y)]
    
    x.grid <- seq(min(c(indVar,depVar)),max(c(indVar,depVar)),by=0.1)
    # Define glm family and link functions
    input_family <- input$family
    input_link <- input$link
    
    if (input_link == "Canonical Link")
    {if (input_family == "binomial"){input_link = "(link='logit')"}
      if (input_family == "gaussian"){input_link = "(link='identity')"}
      if (input_family == "Gamma"){input_link = "(link='inverse')"}
      if (input_family == "inverse.gaussian"){input_link = "(link='1/mu^2')"}
      if (input_family == "poisson"){input_link = "(link='log')"}
      if (input_family == "quasi"){input_link = "(link='identity')"}
      if (input_family == "quasibinomial"){input_link = "(link='logit')"}
      if (input_family == "quasipoisson"){input_link = "(link='log')"}}
    
    family_link <- eval(parse(text = paste(input_family, input_link, sep = "")))
    
    # input_x <- eval(parse(text = input_x))
    # input_y <- eval(parse(text = input_y))
    # 
    if (input$t_x == "None"){glm1 <- glm(depVar ~ indVar, family=family_link)}
    if (input$t_x == "log"){glm1 <- glm(depVar ~ log(indVar), family=family_link)}
    if (input$t_x == "sqrt"){glm1 <- glm(depVar ~ sqrt(indVar), family=family_link)}
    if (input$t_x == "square"){glm1 <- glm(depVar ~ indVar**2, family=family_link)}
    if (input$t_x == "third power"){glm1 <- glm(depVar ~ indVar**3, family=family_link)}
    glm1
    
  })
  
  
  # Make predictions for glm1
  glm1pred <- reactive({
    input_x <- input$x
    input_y <- input$y
    indVar <- myData[,which(colnames(myData) == input_x)]
    depVar <- myData[,which(colnames(myData) == input_y)]
    x.grid <- seq(min(c(indVar,depVar)),max(c(indVar,depVar)),by=0.1)
    newdataframe = data.frame(indVar=x.grid)
    predict(glm1(),newdata=newdataframe,se.fit=TRUE)
  })
  
  # output scatterplot and regression lines
  output$plot_glm1 <- renderPlot({
    input_x <- input$x
    input_y <- input$y
    indVar <- myData[,which(colnames(myData) == input_x)]
    depVar <- myData[,which(colnames(myData) == input_y)]
    x.grid <- seq(min(c(indVar,depVar)),max(c(indVar,depVar)),by=0.1)
    # Plot the scatterplot with the data given
    plot(depVar ~ indVar,col="lightblue",
         pch=15,log="x",xlab=input_x,ylab=input_y,
         xlim = c(min(c(indVar,depVar)), max(c(indVar,depVar))),
         ylim = c(min(c(indVar,depVar)), max(c(indVar,depVar))))
    
    z <- qnorm(0.975,mean=0,sd=1,lower.tail=TRUE)     # That's the z-value
    UL95 <- glm1pred()$fit + (z*glm1pred()$se.fit)    # Upper and lower CI limits
    LL95 <- glm1pred()$fit - (z*glm1pred()$se.fit)  
    # for linear predictor
    
    lines(x.grid,exp(glm1pred()$fit),col="blue")
    lines(x.grid,exp(UL95),lty=2,col="blue")
    lines(x.grid,exp(LL95),lty=2,col="blue")
    
    legend("topleft",col=c("lightblue","blue","blue"),
           pch=c(15,NA,NA),lty=c(NA,1,2),
           legend=c("Observations","Fitted relationship","95% CI"))
    
    
  })
  
  # diagnostic plot for glm1
  output$diagnostic_plot1 <- renderPlot({
    # RegressionPlots(glm1())
    par(mfrow = c(2,2))
    plot(glm1(), which = 1:4)
  })
  
  output$summary_glm1 <- renderPrint({
    summary(glm1())
  })
  
  
  ###############################################################################
  # Build higher dimensional glm models and predict values based on that
  glm2 <- reactive({
    selected_data <- data_set()[,c(input$selectInput,input$inCheckboxGroup)]
    indVar <- as.matrix(selected_data[,-1])
    depVar <- as.matrix(selected_data[,1])

    
    # Define glm family and link functions
    input_family <- input$family2
    input_link <- input$link2
    
    if (input_link == "Canonical Link")
    {if (input_family == "binomial"){input_link = "(link='logit')"}
      if (input_family == "gaussian"){input_link = "(link='identity')"}
      if (input_family == "Gamma"){input_link = "(link='inverse')"}
      if (input_family == "inverse.gaussian"){input_link = "(link='1/mu^2')"}
      if (input_family == "poisson"){input_link = "(link='log')"}
      if (input_family == "quasi"){input_link = "(link='identity')"}
      if (input_family == "quasibinomial"){input_link = "(link='logit')"}
      if (input_family == "quasipoisson"){input_link = "(link='log')"}}
    
    family_link <- eval(parse(text = paste(input_family, input_link, sep = "")))
    
    glm2 <- glm(depVar ~ indVar, family=family_link)
    glm2
  })
  
  # diagnostic plot for glm1
  output$diagnostic_plot2 <- renderPlot({
    # RegressionPlots(glm1())
    par(mfrow = c(2,2))
    plot(glm2(), which = 1:4)
  })
  
  output$summary_glm2 <- renderPrint({
    summary(glm2())
  })
  

  ############ Xiaowen ########################################################
  dsnames <- c()
  data_set <- reactive({
    req(input$file1)
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL) }
    
    if (input$fileType_Input == "1") {
      data_set<-read.csv(inFile$datapath,
                         header = TRUE,
                         stringsAsFactors = FALSE)
    } else {
      data_set<-read_excel(inFile$datapath)
    }
  })
  
  
  observe({
    req(input$file1)
    dsnames <- names(data_set())
    # cb_options <- list()
    # cb_options[dsnames] <- dsnames
    updateCheckboxGroupInput(session, "inCheckboxGroup",
                             label = "Select explanatory variables",
                             choices = dsnames,  
                             selected = "")
    updateSelectInput(session, "selectInput",
                      label = "Select a response variable",
                      choices = dsnames,  
                      selected = "")
  })
  
  
  output$contents<- renderTable({
    
    selected_data <- data_set()[,c(input$selectInput,input$inCheckboxGroup)]
    selected_data
  })
  
  
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
  })
  
  output$text1<-renderPrint({
    model<-lm(as.formula(paste(input$selectInput,"~",paste(input$inCheckboxGroup,collapse="+"))),data=data_set())
    summary(model)
    
  })
  output$text2<-renderPrint({
    model<-lm(as.formula(paste(input$selectInput,"~",paste(input$inCheckboxGroup,collapse="+"))),data=data_set())
    anova(model)
  })
})


shinyApp(ui, Server)