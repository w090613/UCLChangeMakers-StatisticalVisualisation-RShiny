server <- shinyServer(function(input, output) {
  
  ### Create the normal distribution plot for the 'Plotting' page
  output$normal_plot <- renderPlot({
    
    ### Take input values and set up range of x for current parameters...
    mu <- input$mu_plot
    sigma <- input$sigma_plot
    lower_x <- qnorm(0.00001, mu, sigma)
    upper_x <- qnorm(0.00001, mu, sigma, lower.tail = FALSE)
    
    ### ... or take x-range from the values input by the user (if the relevant checkbox is checked)
    if (input$keep_limits){
      lower_x <- input$x_min
      upper_x <- input$x_max
    }
    
    ### Construct plot
    x <- seq(lower_x, upper_x, 0.0001)
    plot(x, dnorm(x, mu, sigma), type = "l", ylab = "Density", cex.lab = 1.25,
         main = "Probability density of normal distribution", cex.main = 1.25)
    legend("topright", legend = paste0("X~N(", mu, ", ", round(sigma^2, 1), ")"),
           lty = 1, lwd = 1)
  })
  
  
  ### Create a normal plot which has a shaded part, equivalent to the probability calculated; for 'Computing probabilities' page
  output$cdf_plot <- renderPlot({
    ### Take input values and set up range of x
    mu <- input$mu_cdf
    sigma <- sqrt(input$var_cdf)
    
    lower_x <- qnorm(0.00001, mu, sigma)
    upper_x <- qnorm(0.00001, mu, sigma, lower.tail = FALSE) 
    
    ### Construct plot
    x <- seq(lower_x, upper_x, 0.0001)
    plot(x, dnorm(x, mu, sigma), type = "l", ylab = "Density", cex.lab = 1.25,
         main = "Probability density of normal distribution", cex.main = 1.25)
    legend("topright", legend = paste0("X~N(", mu, ", ", round(sigma^2, 1), ")"),
           lty = 1, lwd = 1)
    
    ### Shade area based on which case is used (<, > or 'between')
    if (input$sign == "between"){
      smaller <- input$smaller_prob_value
      larger <- input$larger_prob_value
      
      ### Ensure that input is valid
      if (is.na(smaller) == FALSE && is.na(larger) == FALSE && smaller <= larger){
        lines(x = c(smaller, smaller),
              y = c(0, dnorm(smaller, mu, sigma)), lty = "dashed")
        lines(x = c(larger, larger),
              y = c(0, dnorm(larger, mu, sigma)), lty = "dashed")
        polygon(x = c(smaller, seq(smaller, larger, 0.0001), larger),
                y = c(0, dnorm(seq(smaller, larger, 0.0001), mu, sigma), 0),
                col = "gray")
      }
      
    } else if (input$sign == "<"){
      cut_off <- input$smaller_than_prob_value
      
      ### Ensure valid input
      if (is.na(cut_off) == FALSE){
        lines(x = c(cut_off, cut_off),
              y = c(0, dnorm(cut_off, mu, sigma)), lty = "dashed")
        if (cut_off > lower_x){ ### If cut_off < lower_x, no shaded area is needed
          polygon(x = c(lower_x, seq(lower_x, cut_off, 0.0001), cut_off),
                  y = c(0, dnorm(seq(lower_x, cut_off, 0.0001), mu, sigma), 0), 
                  col = "gray")
        }
      }
    } else if (input$sign == ">"){
      cut_off <- input$larger_than_prob_value
      
      ### Ensure valid input
      if (is.na(cut_off) == FALSE){
        lines(x = c(cut_off, cut_off),
              y = c(0, dnorm(cut_off, mu, sigma)), lty = "dashed")
        if (cut_off < upper_x){ ### Shaded area not needed if cut_off > upper_x
          polygon(x = c(cut_off, seq(cut_off, upper_x, 0.0001), upper_x),
                  y = c(0, dnorm(seq(cut_off, upper_x, 0.0001), mu, sigma), 0), 
                  col = "gray")
        }
      }
    }
  })
  
  ### Write down equation and computation of the cdf
  output$calculate_prob <- renderText({
    mu <- input$mu_cdf
    sigma <- sqrt(input$var_cdf)
    
    ### Case distinction
    if (input$sign == "between"){
      smaller <- input$smaller_prob_value
      larger <- input$larger_prob_value
      
      ### Ensure valid input
      if (is.na(smaller) == FALSE && is.na(larger) == FALSE && smaller <= larger){
        paste0("P(", smaller, " < X < ", larger, ") = ", round((pnorm(larger, mu, sigma) - pnorm(smaller, mu, sigma)), 4), " = area of shaded region") 
      } else if (smaller > larger){
        paste("Please make sure that the 'smaller' value is not larger than the 'larger' value.")
      }
    } else if (input$sign == "<"){
      cut_off <- input$smaller_than_prob_value
      paste0("P(X < ", cut_off, ") = ", round(pnorm(cut_off, mu, sigma), 4), " = area of shaded region")
    } else {
      cut_off <- input$larger_than_prob_value
      paste0("P(X > ", cut_off, ") = ", round(pnorm(cut_off, mu, sigma, lower.tail = FALSE), 4), " = area of shaded region")
    }
  })
  
  ### Create histogram and qq plot for 'Checking normality' page
  output$hist <- renderPlot({
    n <- input$sample_size
    x <- 0
    
    ### Check valid input, case distinction; sample from needed distribution
    if (input$distribution == "Normal" && is.na(input$mu_sample) == FALSE && is.na(input$var_sample) == FALSE && input$var_sample > 0){
      x <- rnorm(n, input$mu_sample, sqrt(input$var_sample))
    } else if (input$distribution == "Student t" && is.na(input$df) == FALSE && input$df > 0){
      x <- rt(n, input$df)
    } else if (input$distribution == "Exponential" && is.na(input$rate) == FALSE&&input$rate > 0){
      x <- rexp(n, input$rate)
    } else if (input$distribution == "Random number generator" && is.na(input$min) == FALSE && is.na(input$max) == FALSE && input$min < input$max){
      x <- sample(seq(input$min, input$max, 0.001), size = n, replace = TRUE)
    }
    
    ### If length(x) = 1 <=> x = 0 as set at the start, i.e. sampling has not gone through
    if (length(x) != 1){
      par(mfrow = c(1, 2))
      hist(x, freq = FALSE, cex.main = 1.25, cex.lab = 1.25)
      lines(x = seq(min(x), max(x), 0.0001), 
            y = dnorm(seq(min(x), max(x), 0.0001), mean(x), sd(x)),
            col = "blue", lwd = 2)
      mtext("Blue line = N(sample mean, sample variance)", line = .25)
      qqnorm(x)
      qqline(x, col = "red")
    }
  })
})

shinyApp(ui, server)
