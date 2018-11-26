#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
UI <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Brownian Motion is cool! :)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("npt",
                   "Number of Points for Each Simulated Path",
                   min = 10,
                   max = 2000,
                   value = 100),
       sliderInput("T", "Time T for Each Simulated Path",
                   min = 0.1,
                   max = 2,
                   value =1),
       sliderInput("nsim", "Number of Simulations",
                   min = 1,
                   max = 200,
                   value = 5),
       checkboxInput('showave','showave')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
