# install.packages("shiny")
# install.packages("devtools")
# devtools::install_github("rstudio/shiny-incubator")
# install.packages("markovchain")
# install.packages("diagram")

library(devtools)
library(shiny)
# devtools::install_github("rstudio/shiny-incubator")
library(shinyIncubator)
library(markovchain)
library(diagram)

Init_States = 3
Min_States = 2
Max_States = 7


ui <- shinyUI(fluidPage(
  titlePanel(title = "Discrete Time Markov Process"),
  
  sidebarLayout(
    sidebarPanel(h4("Setting up transition matrix"),
                 numericInput("dimension", "Enter the number of states:", 
                              value = Init_States, min = Min_States, max = Max_States),
                 helpText("You can choose between",Min_States,"and",Max_States, "states"),
                 uiOutput("matrix"),
                 h5("First Passage Probability"), 
                 uiOutput("select"),
                 numericInput("step", "Enter the number of steps: ", min = 1, value = 1),
                 submitButton("Submit"),
                 width = 5),
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("State Space Diagram", plotOutput("trans_plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("First Passage Prob", verbatimTextOutput("pas_prob")))

    )
  )
))

