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
      h4("Hi there :)"), 
      #file Input
      radioButtons("fileType_Input",
                   label = h4("Choose File type"),
                   choices = list(".csv/txt" = 1, ".xlsx" = 2),
                   selected = 1,
                   inline = TRUE),
      fileInput('file1',
                h4("Upload Items List"),
                accept = c(".csv",".xlsx")),
      selectInput("selectInput",
                  "Checkbox group input for response variable:",
                  c("label 1" = "option1","label 2" = "option2")),
      
      selectInput("selecttrans1","Select transformation for response variable:",
                  c("log(Y)","1/Y","Y^2","sqrt(Y)","null"),selected="null"),
      checkboxGroupInput("inCheckboxGroup",
                         "Checkbox group input for explanatory variable:",
                         c("label 1" = "option1","label 2" = "option2")),
      selectInput("selecttrans2","Select transformation for explanatory variable:",
                  c("log(X)","1/X","X^2","sqrt(X)","null"),selected="null"),
      radioButtons("intercept","Include intercept?",c("yes","no"),selected="yes"),
      radioButtons("interactions","Include interaction?",c("yes","no"),selected ="no"),
      
      
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



