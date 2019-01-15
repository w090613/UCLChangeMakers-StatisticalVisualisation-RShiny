# Define UI for application that draws a histogram
UI <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("Introduction to binomial distribution"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #header
            h4("parameters: n and p"),
            
            # select Input
            selectInput("cl",
                        label = "Choose a colour to display",
                        choices = c("darkgrey", 
                                    "black",
                                    "dark blue", 
                                    "red"),
                        selected = "grey"),
            
            # slider Input
            sliderInput("n","number of independent rounds",min=10,max=100,value=30),
            
            sliderInput("p","probability of success",min=0,max=1,value=0.5),
            
            sliderInput("sample_size","size of random samples",min=1,max = 500,value=30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("bin_plot")
        )
    )))

