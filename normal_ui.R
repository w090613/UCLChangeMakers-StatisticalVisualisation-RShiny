library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Normal distribution"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plotting", tabName = "plot", icon = icon("pencil")),
      menuItem("Computing probabilities", tabName = "cdf", icon = icon("plus")),
      menuItem("Checking normality", tabName = "qq", icon = icon("search"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot",
              fluidRow(
                box(plotOutput("normal_plot"), width = 7),
                
                box(
                  title = "Controls",
                  
                  sliderInput("mu_plot",
                              "Mean of X:",
                              min = -10.0,
                              max = 10.0, 
                              value = 0,
                              step = 0.1),
                  
                  sliderInput("sigma_plot",
                              "Standard deviation of X:",
                              min = 0.1,
                              max = 3,
                              value = 1),
                  
                  checkboxInput("keep_limits",
                                "Use input x-range?"),
                  
                  numericInput("x_min",
                               "Lower bound of x-range:",
                               value = round(qnorm(0.00001), 1),
                               step = .1),
                  
                  numericInput("x_max",
                               "Upper bound of x-range:",
                               value = round(qnorm(0.00001, lower.tail = FALSE), 1),
                               step = .1),
                  width = 5
                )
              )
            ),
      
      tabItem(tabName = "cdf",
              fluidRow(
                box(plotOutput("cdf_plot"), width = 7),
                
                box(
                  title = "Controls",
                  
                  numericInput("mu_cdf",
                               "Mean of X:",
                               value = 0),
                  
                  numericInput("var_cdf",
                               "Variance of X:",
                               value = 1,
                               min = 0.01,
                               step = 0.01),
                  
                  selectInput("sign",
                              "Sign of probability to be calculated:",
                              choices = c("<", ">", "between")),
                  
                  
                  ### Present suitable input boxes depending on whether <, > or 'between' is chosen
                  conditionalPanel(
                    condition = "input.sign == 'between'",
                    tagList(
                      numericInput("smaller_prob_value",
                                   "Smaller value:",
                                   value = 0,
                                   step = 0.1),
                      numericInput("larger_prob_value",
                                   "Larger value:",
                                   value = 0,
                                   step = 0.1)
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.sign == '<'",
                    numericInput("smaller_than_prob_value", 
                                 "X smaller than:", 
                                 value = 0,
                                 step = 0.1)
                  ),
                  
                  conditionalPanel(
                    condition = "input.sign == '>'",
                    numericInput("larger_than_prob_value", 
                                 "X larger than:", 
                                 value = 0,
                                 step = 0.1)
                  ),
                  
                  width = 5
                  ),
                
                box(
                  title = textOutput("calculate_prob"),
                  width = 7
                  )
                )
              ),
      
      tabItem(tabName = "qq",
              fluidPage(
                box(plotOutput("hist"), width = 9),
                box(title = "Controls",
                    
                    numericInput("sample_size",
                                 "Sample size:",
                                 value = 1000,
                                 min = 20,
                                 max = 100000),
                    
                    selectInput("distribution",
                                "Distribution to sample from:",
                                choices = c("Normal", "Student t", "Exponential", "Random number generator")),
                    
                    ### Ensure that correct parameters are displayed based on selected distribution
                    conditionalPanel(
                      condition = "input.distribution == 'Normal'",
                      tagList(
                          numericInput("mu_sample", "Mean:", value = 0),
                          numericInput("var_sample", "Variance:", value = 1, min = 0.01)
                      )
                    ), 
                    
                    conditionalPanel(
                      condition = "input.distribution == 'Student t'",
                      numericInput("df", "Degrees of freedom:", value = 5, min = 1, step = 1)
                    ),
                    
                    conditionalPanel(
                      condition = "input.distribution == 'Exponential'",
                      numericInput("rate", "Rate", value = 1, min = 0.01, step = 0.01)
                    ),
                    
                    conditionalPanel(
                      condition = "input.distribution == 'Random number generator'",
                      tagList(
                        numericInput("min", "From:", value = 0),
                        numericInput("max", "To:", value = 10)
                      )
                    ),
                      
                    
                    width = 3)
              )
            )
    )
  )
)
