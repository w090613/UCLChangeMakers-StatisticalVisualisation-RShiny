library(shiny)
library(readxl)
library(shiny)

# Define UI for application that draws a histogram
# fluidpage -> page type



Server <- shinyServer(function(input, output,session) {
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
    cb_options <- list()
    cb_options[dsnames] <- dsnames
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
    
    data_set()[,c(input$selectInput,input$inCheckboxGroup)]
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

shinyApp(UI, Server)
