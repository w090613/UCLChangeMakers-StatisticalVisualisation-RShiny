library(shiny)

# Define UI for application that draws a histogram
# fluidpage -> page type
UI <- shinyUI(fluidPage(
  # 1. Application title
  titlePanel("Linear Regression App"),
  
  # 2. Sidebar with a header and a slider input for number of bins 
  sidebarLayout(
    # Create a sidebar panel
    sidebarPanel(
      # header
      h4("General linear model"), 
      #file Input
      fileInput("file1","choose excel file"),
      radioButtons("fileType_Input",
                   label = h4("Choose File type"),
                   choices = list(".csv/txt" = 1, ".xlsx" = 2),
                   selected = 1,
                   inline = TRUE),
      selectInput("selectInput",
                  "Checkbox group input for response variable:",
                  c("label 1" = "option1",
                    "label 2" = "option2")),
      checkboxGroupInput("inCheckboxGroup",
                         "Checkbox group input for explanatory variable:",
                         c("label 1" = "option1",
                           "label 2" = "option2")),

      
      uiOutput("choose_columns")
    ),
    # 3. Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("data",tableOutput("contents") ),
                  tabPanel("model summary",verbatimTextOutput("text1")),
                  tabPanel("anova",verbatimTextOutput("text2")))

    )
  )))



