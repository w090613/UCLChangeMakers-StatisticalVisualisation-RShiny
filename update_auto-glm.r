library(shiny)
library(lattice)
library(shinydashboard)
library(plotly)
library(devtools)


# myData <- read.table("galapagos.dat")
myData <- iris
myData <- mtcars
myData <- USArrests



# Define server logic required to draw a histogram
Server <- shinyServer(function(input, output) {
  
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
  
  glm1pred <- reactive({
    input_x <- input$x
    input_y <- input$y
    indVar <- myData[,which(colnames(myData) == input_x)]
    depVar <- myData[,which(colnames(myData) == input_y)]
    x.grid <- seq(min(c(indVar,depVar)),max(c(indVar,depVar)),by=0.1)
    newdataframe = data.frame(indVar=x.grid)
    predict(glm1(),newdata=newdataframe,se.fit=TRUE)
  })
  
  output$summary_glm <- renderPrint({
    summary(glm1())
  }
  
  )
  
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
  
  
  # Using Plotly
  RegressionPlots <- function(fit){
    # Extract fitted values from lm() object
    Fitted.Values <-  fitted(fit)
    # Extract residuals from lm() object
    Residuals <-  resid(fit)
    # Extract standardized residuals from lm() object
    Standardized.Residuals <- MASS::stdres(fit)  
    # Extract fitted values for lm() object
    Theoretical.Quantiles <- qqnorm(Residuals, plot.it = F)$x
    # Square root of abs(residuals)
    Root.Residuals <- sqrt(abs(Standardized.Residuals))
    # Calculate Leverage
    Leverage <- lm.influence(fit)$hat
    # Create data frame 
    # Will be used as input to plot_ly
    regMat <- data.frame(Fitted.Values, 
                         Residuals, 
                         Standardized.Residuals, 
                         Theoretical.Quantiles,
                         Root.Residuals,
                         Leverage)
    # Plot using Plotly
    # Fitted vs Residuals
    # For scatter plot smoother
    LOESS1 <- loess.smooth(Fitted.Values, Residuals)
    plt1 <- regMat %>% 
      plot_ly(x = Fitted.Values, y = Residuals, 
              type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
              marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
      
      add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "line", name = "Smooth",
                line = list(width = 2)) %>% 
      
      layout(title = "Residuals vs Fitted Values", plot_bgcolor = "#e6e6e6", width = 1000)
    
    # QQ Pot
    plt2 <- regMat %>% 
      plot_ly(x = Theoretical.Quantiles, y = Standardized.Residuals, 
              type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
              marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
      
      add_trace(x = Theoretical.Quantiles, y = Theoretical.Quantiles, type = "scatter", mode = "line", name = "",
                line = list(width = 2)) %>% 
      layout(title = "Q-Q Plot", plot_bgcolor = "#e6e6e6")
    
    # Scale Location
    # For scatter plot smoother
    LOESS2 <- loess.smooth(Fitted.Values, Root.Residuals)
    
    plt3 <- regMat %>% 
      plot_ly(x = Fitted.Values, y = Root.Residuals, 
              type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
              marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
      
      add_trace(x = LOESS2$x, y = LOESS2$y, type = "scatter", mode = "line", name = "Smooth",
                line = list(width = 2)) %>% 
      
      layout(title = "Scale Location", plot_bgcolor = "#e6e6e6", width = 1000)
    
    # Residuals vs Leverage
    # For scatter plot smoother
    LOESS3 <- loess.smooth(Leverage, Residuals)
    
    plt4 <- regMat %>% 
      plot_ly(x = Leverage, y = Residuals, 
              type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
              marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
      
      add_trace(x = LOESS3$x, y = LOESS3$y, type = "scatter", mode = "line", name = "Smooth",
                line = list(width = 2)) %>% 
      
      layout(title = "Leverage vs Residuals", plot_bgcolor = "#e6e6e6")
    
    plt = list(plt1, plt2, plt3, plt4)
    return(plt)
  }
  
  output$diagnostic_plot1 <- renderPlot({
    # RegressionPlots(glm1())
    par(mfrow = c(2,2))
    plot(glm1(), which = 1:4)
  })
})
# 
# # print the predicted values
# output$sum1 <- renderText({
#   glm1 <- glm(depVar ~ log(indVar), 
#                         family=input$family(link=input$link),
#                         data=myData)
#   sum1 <- summary(glm1)
#   sum1$aic
# })


#Side bar
sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem(text='Generalised Linear Modelling',tabName='glm'),
    menuItem(text='Diagnostics and Analytics',tabName='diag')
  )
)

#Body
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "glm",
      selectInput("x", label = "Choose an independent variable (x)",
                  choices = colnames(myData)),
      selectInput("y", label = "Choose a dependent varialbe (y)",
                  choices = colnames(myData)),
      selectInput("family",
                  label = "Choose a family to run glm",
                  choices = c('poisson', 
                              "gaussian",
                              "Gamma", 
                              "inverse.gaussian",
                              "quasi",
                              "quasibinomial",
                              "quasipoisson",
                              "binomial"
                  ),
                  selected = "poisson"),
      selectInput("link",
                  label = "Change link function",
                  choices = c("Canonical Link",
                              "()",
                              "(link='log')",
                              "(link='probit')",
                              "(link='cauchit')", 
                              "(link='cloglog')",
                              "(link='identity')",
                              "(link='logit')",
                              "(link='sqrt')",
                              "(link='1/mu^2')",
                              "(link='inverse')"),
                  selected = "Canonical Link"),
      selectInput("t_x",
                  label = "Choose a certain transformation for the independent variable",
                  choices = c("None", 
                              "log",
                              "sqrt",
                              "square",
                              "third power"
                  ), 
                  selected = "log"),
      plotOutput("plot_glm1"), #plotting histogram and density plot for population
      verbatimTextOutput("summary_glm")
    ),
    
    
    tabItem(tabName = "diag",
            plotOutput("diagnostic_plot1"))
  )
)


ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = sidebar,
                    body = body
)


shinyApp(ui, Server)