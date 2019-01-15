## ui file for the changemaker project

library(shiny)
library(shinydashboard)
library(plotly)
# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "ChangeMaker"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Survival Analysis', tabName = "Surv", icon = icon("bar-chart-o")))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Surv',
              h1('Survival Analysis'),
              selectInput('Dataset', 'Select a dataset',c('lung','cgd','pbc')),
              #  selectInput('Covariates','Select a covariate', )
              dataTableOutput('DataSetTable'),
              fluidRow(
                box(title = 'DataSet info',verbatimTextOutput('DataSetInfo'),collapsible = TRUE, width = 12),
                box(title = 'DataSet Summary',verbatimTextOutput("summary"),collapsible = TRUE, width = 12)
                
              ),
              
              fluidRow(box(title = 'plot',plotOutput('DataSetPlot1',
                                                     width = "500px", height = "500px")),
                       box('select Covariate',selectInput('covariate', 'Select a covariate', 'placeholder')) )
              
              
      ) # end of the tab item
      
    ) # end of tabItems
  ) # end of dashboard body
  
  
) # end of dashboardpage
