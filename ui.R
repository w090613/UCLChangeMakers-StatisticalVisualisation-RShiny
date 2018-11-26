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
        sliderInput("miu", "Mean", 
                    min=-100, max=100, value=0, step = 0.5),
        sliderInput("sigma", "Standard Deviation", 
                    min=1, max=50, value=1, step = 0.5)
      ),
      conditionalPanel(
        condition = "input.dist == 'unif'",
        sliderInput("a", "Minimum value", 
                    min=-50, max=50, value=0),
        sliderInput("b", "Maximum value", 
                    min=-50, max=50, value=1)
      ),
      conditionalPanel(
        condition = "input.dist == 'exp'",
        sliderInput("lambda", "Rate", 
                    min=1, max=50, value=1)
      ),
      conditionalPanel(
        condition = "input.dist == 'binom'",
        sliderInput("p", "Probability", 
                    min=0, max=1, value=0.5, step = 0.05),
        sliderInput("n", "Number of Trials", 
                    min=1, max=100, value=1)
      ),
      conditionalPanel(
        condition = "input.dist == 'nbinom'",
        sliderInput("p2", "Probability", 
                    min=0, max=1, value=0.5, step = 0.05),
        sliderInput("r", "Number of Failures", 
                    min=1, max=50, value=1)
      ),
      conditionalPanel(
        condition = "input.dist == 'pois'",
        sliderInput("lambda2", "Rate", 
                    min=1, max=50, value=1)
      ),
      conditionalPanel(
        condition = "input.dist == 'geom'",
        sliderInput("p3", "Probability", 
                    min=0, max=1, value=0.5, step = 0.05)
      ),
      conditionalPanel(
        condition = "input.dist == 'hyper'",
        sliderInput("M", "Number of Success States in Population", 
                    min=1, max=50, value=10),
        sliderInput("N", "Population Size", 
                    min=1, max=100, value=20),
        sliderInput("K", "Number of Draws", 
                    min=1, max=30, value=5)
      ),
      conditionalPanel(
        condition = "input.dist == 'chisq'",
        sliderInput("df", "Degrees of Freedom", 
                    min=1, max=50, value=1, step = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 't'",
        sliderInput("df2", "Degrees of Freedom", 
                    min=1, max=50, value=1, step=1)
      ),
      conditionalPanel(
        condition = "input.dist == 'beta'",
        sliderInput("Alpha", "First Shape", 
                    min=0.5, max=50, value=0.5, step = 0.5),
        sliderInput("Beta", "Second Shape", 
                    min=0.5, max=50, value=0.5, step = 0.5)
      ),
      conditionalPanel(
        condition = "input.dist == 'gamma'",
        sliderInput("k", "Shape", 
                    min=0.5, max=50, value=0.5, step = 0.5),
        sliderInput("Theta", "Scale", 
                    min=0.5, max=50, value=0.5, step = 0.5)
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