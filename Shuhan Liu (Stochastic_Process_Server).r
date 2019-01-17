
server <- shinyServer(
  function(input, output){
    output$matrix <- renderUI({
      matrixInput("trans_prob", "Input the transition probablity matrix",
                  as.data.frame(diag(1, nrow = input$dimension, ncol = input$dimension)))
    })
    
    
    
    var <- reactive({
      stateNames <-  c()
      for (i in 1:input$dimension){
        stateNames[i] = paste("state",i, sep = " ")
      }
      stateNames
    }) 
    
    output$select <- renderUI({
      selectInput("start", h6("Select starting state: "), 
                  choices = var())
    })
    
    output$pas_prob <- renderPrint({
      trans_matrix <- matrix(input$trans_prob, nrow = input$dimension, 
                             ncol = input$dimension)
      matrix_row_sum <- rowSums(trans_matrix)
      norm_matrix <- round(trans_matrix / matrix_row_sum, 2)
      norm_matrix[,input$dimension] <- 1-rowSums(norm_matrix[,-input$dimension])
      stateNames = c()
      for (i in 1:input$dimension){
        stateNames[i] = paste("state",i, sep = " ")
      }
      dcmc <- new("markovchain", transitionMatrix = norm_matrix, 
                  states = stateNames)
      round(firstPassage(dcmc, input$start, input$step),3)
      
    })
    
    output$trans_plot <- renderPlot({
      trans_matrix <- matrix(input$trans_prob, nrow = input$dimension, 
                             ncol = input$dimension)
      matrix_row_sum <- rowSums(trans_matrix)
      norm_matrix <- round(trans_matrix / matrix_row_sum, 2)
      norm_matrix[,input$dimension] <- 1-rowSums(norm_matrix[,-input$dimension])
      stateNames = c()
      for (i in 1:input$dimension){
        stateNames[i] = paste("state",i, sep = " ")
      }
      dcmc <- new("markovchain", transitionMatrix = norm_matrix, 
                  states = stateNames)
      plot(dcmc, main = "State Space Diagram")
      mtext("same coulored dots indicate the states are in the same class")
    })
    
    output$summary <- renderPrint({
      trans_matrix <- matrix(input$trans_prob, nrow = input$dimension, 
                             ncol = input$dimension)
      matrix_row_sum <- rowSums(trans_matrix)
      norm_matrix <- round(trans_matrix / matrix_row_sum, 2)
      norm_matrix[,input$dimension] <- 1-rowSums(norm_matrix[,-input$dimension])
      stateNames = c()
      for (i in 1:input$dimension){
        stateNames[i] = paste("state",i, sep = " ")
      }
      dcmc <- new("markovchain", transitionMatrix = norm_matrix, 
                  states = stateNames)
      summary_dcmc <- summary(dcmc)
      
    })
    
  }
)

shinyApp(ui, server)



