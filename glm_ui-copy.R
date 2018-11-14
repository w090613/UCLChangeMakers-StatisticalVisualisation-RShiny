#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
# myData <- read.table("galapagos.dat")
myData <- iris
# myData <- mtcars
#myData <- read.table("CO2_passenger_cars_v14 2.csv")

# Define UI for application that draws a histogram
UI <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Fitting Generalised Linear Models"),
  
  # Sidebar with a slider taking input (Petal.Length)
  # and checkbox for choosing show models
  sidebarLayout(
    sidebarPanel(
      # sliderInput("sliderIris", "What is the petal length of the iris?",
      #             min = 1, max = 6.9, value = 3),
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
                  label = "Choose a link function",
                  choices = c("(link='log')",
                              "()",
                              "(link='probit')",
                              "(link='cauchit')", 
                              "(link='cloglog')",
                              "(link='identity')",
                              "(link='logit')",
                              "(link='sqrt')",
                              "(link='1/mu^2')",
                              "(link='inverse')"),
                  selected = "(link='log')"),
      selectInput("t_x",
                  label = "Choose a certain transformation for the independent variable",
                  choices = c("None", 
                              "log",
                              "sqrt",
                              "square",
                              "third power"
                              ), 
                  selected = "log")
    ),
    
    # main panel for ploting the data and the prediction models
    mainPanel(
      # plotOutput("plot_glm1"), height=100
      # h3("AIC of Model 1: "),
      # textOutput("sum1")
      #Plot tab
      tabPanel("Plot",
               plotOutput("plot_glm1"), #plotting histogram and density plot for population
               plotOutput("diagnostic_plot1")), #plotting histogram and density plot for sample
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
))
