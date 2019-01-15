library(shiny)
library(lattice)
library(shinydashboard)
library(plotly)
library(devtools)


myData <- read_xlsx("Xiaowen Huang & Sean Chen (icadata).xlsx")
#myData <- read.table("galapagos.dat")
myData <- iris
myData <- USArrests




# 
# # print the predicted values
# output$sum1 <- renderText({
#   glm1 <- glm(depVar ~ log(indVar), 
#                         family=input$family(link=input$link),
#                         data=myData)
#   sum1 <- summary(glm1)
#   sum1$aic
# })


#Side bar
sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem(text='DataTable, lm and ANOVA', tabName='Xiaowen'),
    menuItem(text='Generalised Linear Modelling (2D)',tabName='glm'),
    menuItem(text='High-Dimensional GLM',tabName='multi-G')
    
  )
)

#Body
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "glm",
      fluidRow(box(selectInput("x", label = "Choose an explanatory variable (x)",
                               choices = colnames(myData), selected = colnames(myData[1])),
                   selectInput("y", label = "Choose a response varialbe (y)",
                               choices = colnames(myData), selected = colnames(myData[2])),
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
                               label = "Change link function",
                               choices = c("Canonical Link",
                                           "()",
                                           "(link='log')",
                                           "(link='probit')",
                                           "(link='cauchit')", 
                                           "(link='cloglog')",
                                           "(link='identity')",
                                           "(link='logit')",
                                           "(link='sqrt')",
                                           "(link='1/mu^2')",
                                           "(link='inverse')"),
                               selected = "Canonical Link"),
                   
                   selectInput("t_x",
                               label = "Choose a certain transformation for the explanatory variable",
                               choices = c("None", 
                                           "log",
                                           "sqrt",
                                           "square",
                                           "third power"
                               ), 
                               selected = "log")),
               box(verbatimTextOutput("summary_glm1"))),
      
      
      # fluidRow(
      #   plot(6, box(plotOutput("plot_glm1"), width=12, title = "2-D glm")),
      #   column(6, box(plotOutput("diagnostic_plot1"), width=12, title = "2-D glm"))
      # ),
      
      # plotOutput("plot_glm1"), #plotting histogram and density plot for population

      fluidRow(box(plotOutput('plot_glm1')),
               box(plotOutput('diagnostic_plot1'))
    )),
    
    tabItem(tabName = 'multi-G',
      fluidRow(box(selectInput("family2",
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
                   selectInput("link2",
                               label = "Change link function",
                               choices = c("Canonical Link",
                                           "()",
                                           "(link='log')",
                                           "(link='probit')",
                                           "(link='cauchit')", 
                                           "(link='cloglog')",
                                           "(link='identity')",
                                           "(link='logit')",
                                           "(link='sqrt')",
                                           "(link='1/mu^2')",
                                           "(link='inverse')"),
                               selected = "Canonical Link")),
               box(plotOutput("diagnostic_plot2"))),
            verbatimTextOutput("summary_glm2")
               ),
    
    tabItem(tabName = "Xiaowen",
            # header
            fluidRow(box(h4("General linear model"), 
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
                         uiOutput("choose_columns")),
              box(tabsetPanel(type="tabs",
                                  tabPanel("data",tableOutput("contents") ),
                                  tabPanel("model summary",verbatimTextOutput("text1")),
                                  tabPanel("anova",verbatimTextOutput("text2")))) 
  ))
))


ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = sidebar,
                    body = body
)



