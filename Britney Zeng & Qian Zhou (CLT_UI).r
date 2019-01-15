#CLT_ui
library(shiny)

ui <- fluidPage(
  #Header
  h1("Central Limit Theorem - CLT"),
  
  #Sidebar Layout
  sidebarLayout(
    
    #Sidebar panel
    sidebarPanel(
      #Pupulation size
      sliderInput("pop_size",
                  label = "Population Size",
                  value = 2000, min = 1000, max = 100000),
      
      #Enter sample size
      textInput("smpl_size",
                label = "Enter Sample Size",
                value = 50),
      
      #Distribution
      selectInput("dist", label = "Distribution ",
                  choices = c("Normal" = "norm", "Uniform" = "unif", "Exponential"="exp",
                              "Binomial"="binom", "Negative binomial"="nbinom", "Poission"="pois", 
                              "Geometric"="geom", "Hypergeometric"="hyper", "Chi-squared"="chisq", 
                              "Student's t"="t", "Beta"="beta", "Gamma"="gamma"), 
                  selected = "norm"),
      conditionalPanel(
        condition = "input.dist == 'norm'",
        textInput("miu", label = "Mean", value = 0),
        textInput("sigma", label = "Standard Deviation", value = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'unif'",
        textInput("a", "Minimum value", value = 0),
        textInput("b", "Maximum value", value = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'exp'",
        textInput("lambda", "Rate", value = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'binom'",
        textInput("p", "Probability", value = 0.5),
        textInput("n", "Number of Trials", value = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'nbinom'",
        textInput("p2", "Probability", value = 0.5),
        textInput("r", "Number of Failures", value = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'pois'",
        textInput("lambda2", "Rate", value = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'geom'",
        textInput("p3", "Probability", value=0.5)
      ),
      conditionalPanel(
        condition = "input.dist == 'hyper'",
        textInput("M", "Number of Success States in Population", value=10),
        textInput("N", "Population Size",value=20),
        textInput("K", "Number of Draws", value=5)
      ),
      conditionalPanel(
        condition = "input.dist == 'chisq'",
        textInput("df", "Degrees of Freedom", value=1)
      ),
      conditionalPanel(
        condition = "input.dist == 't'",
        textInput("df2", "Degrees of Freedom", value=1)
      ),
      conditionalPanel(
        condition = "input.dist == 'beta'",
        textInput("Alpha", "First Shape", value=0.5),
        textInput("Beta", "Second Shape", value=0.5)
      ),
      conditionalPanel(
        condition = "input.dist == 'gamma'",
        textInput("k", "Shape", value=0.5),
        textInput("Theta", "Scale", value=0.5)
      ),
      
      #Sampling iteration
      sliderInput("smpl_iterate",
                  label = "Sampling Iteration",
                  value = 200, min = 100, max = 10000)
    ),
    
    #Main panel
    mainPanel(
      tabsetPanel(
        #Plot tab
        tabPanel("Plot",
                 plotOutput("plot_pop"), #plotting histogram and density plot for population
                 plotOutput("plot_smpl_mean")), #plotting histogram and density plot for sample
        #Data tab
        tabPanel("Data", 
                 h4("Population"), #heading for the population summary and descriptive statistics
                 verbatimTextOutput("pop_summary"), #rendering population summary statistics
                 verbatimTextOutput("pop_structure"), 
                 h4("Sample"), #heading for the sample mean summary and descriptive statistics
                 verbatimTextOutput("smpl_mean_summary"), #rendering sample summary statistics
                 verbatimTextOutput("smpl_mean_structure"))
      )
    )
  )
)
