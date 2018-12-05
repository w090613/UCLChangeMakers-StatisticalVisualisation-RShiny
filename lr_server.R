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
                             label = "Check Box Group of explanatory variable",
                             choices = dsnames,  
                             selected = "")
    updateSelectInput(session, "selectInput",
                             label = "Select response variable",
                             choices = dsnames,  
                             selected = "")
  })
  
  


  output$contents<- renderTable({
    
    data_set()[,c(input$selectInput,input$inCheckboxGroup)]
  })
  
  

  

  output$text1<-renderPrint({
    selected_data_set<-data_set()[,c(input$selectInput,input$inCheckboxGroup)]
    
    if (input$selecttrans1=="log(Y)"){selected_data_set[,1]<-log(selected_data_set[,1])}
    else if (input$selettrans1=="1/Y"){selected_data_set[,1]<-1/selected_data_set[,1]}
    else if (input$selettrans1=="Y^2"){selected_data_set[,1]<(selected_data_set[,1])^2}
    else if (input$selettrans1=="sqrt(Y)"){selected_data_set[,1]<-sqrt(selected_data_set[,1])}
    else {selected_data_set[,1]<-selected_data_set[,1]}
    
    if (input$selecttrans2=="log(X)"){selected_data_set[,2:ncol(selected_data_set)]<-log(selected_data_set[,2:ncol(selected_data_set)])}
    else if (input$selettrans2=="1/X"){selected_data_set[,2:ncol(selected_data_set)]<-1/selected_data_set[,2:ncol(selected_data_set)]}
    else if (input$selettrans2=="X^2"){selected_data_set[,2:ncol(selected_data_set)]<(selected_data_set[,2:ncol(selected_data_set)])^2}
    else if (input$selettrans2=="sqrt(X)"){selected_data_set[,2:ncol(selected_data_set)]<-sqrt(selected_data_set[,2:ncol(selected_data_set)])}
    else {selected_data_set[,2:ncol(selected_data_set)]<-selected_data_set[,2:ncol(selected_data_set)]}
    if (input$interaction=="yes"){
      model<-lm(as.formula(paste(colnames(selected_data_set[,1]),"~",
                                 paste(colnames(selected_data_set[,2:ncol(selected_data_set)]),collapse = "*"))),
                data=selected_data_set)
    } else {
      model<-lm(as.formula(paste(colnames(selected_data_set[,1]),"~",
                                 paste(colnames(selected_data_set[,2:ncol(selected_data_set)]),collapse = "+"))),
                data=selected_data_set)
    }
    
    if (input$intercept=="no"){update(moedl,.~.+0)}
    summary(model)
  
  })
  output$text2<-renderPrint({
    anova(model)
  })
})

shinyApp(UI, Server)
