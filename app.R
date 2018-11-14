library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(caret)
library(randomForest)
################################################################################
# Data wrangling comes from Kaggle kernels
################################################################################
train<-read.csv('train.csv')
test<-read.csv('test.csv')
train$set<-"train"
test$set<-"test"
#Add response column in test so that they have equal number
#of columns
test$Survived<-NA

#Combine both datasets
full<-rbind(train,test)


full <- full %>%
  
  mutate(
    
    Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),
    
    Age_Group = case_when(Age < 13 ~ "Age.00_12", 
                          
                          Age >= 13 & Age < 18 ~ "Age.13_17",
                          
                          Age >= 18 & Age < 60 ~ "Age.18_59",
                          
                          Age >= 60 ~ "Age.60+"))

full_display=full

full$Embarked <- replace(full$Embarked, which(is.na(full$Embarked)), 'S')

names <- full$Name

title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)

full$title <- title


full$title[full$title == 'Mlle']        <- 'Miss' 

full$title[full$title == 'Ms']          <- 'Miss'

full$title[full$title == 'Mme']         <- 'Mrs' 

full$title[full$title == 'Lady']          <- 'Miss'

full$title[full$title == 'Dona']          <- 'Miss'


#In case of losing too much of predictive power, create a third category "Officer"

full$title[full$title == 'Capt']        <- 'Officer' 

full$title[full$title == 'Col']        <- 'Officer' 

full$title[full$title == 'Major']   <- 'Officer'

full$title[full$title == 'Dr']   <- 'Officer'

full$title[full$title == 'Rev']   <- 'Officer'

full$title[full$title == 'Don']   <- 'Officer'

full$title[full$title == 'Sir']   <- 'Officer'

full$title[full$title == 'the Countess']   <- 'Officer'

full$title[full$title == 'Jonkheer']   <- 'Officer' 

#Parch&SibSp
#Families are binned into a discretized feature based on family member count.

full$FamilySize <-full$SibSp + full$Parch + 1 

full$FamilySized[full$FamilySize == 1] <- 'Single' 

full$FamilySized[full$FamilySize < 5 & full$FamilySize >= 2] <- 'Small' 

full$FamilySized[full$FamilySize >= 5] <- 'Big' 

full$FamilySized=as.factor(full$FamilySized)

#ticket
ticket.unique <- rep(0, nrow(full))

tickets <- unique(full$Ticket)

for (i in 1:length(tickets)){
  current.ticket<-tickets[i]
  party.indexes<-which(full$Ticket==current.ticket)
  for(k in 1:length(party.indexes)){
    ticket.unique[party.indexes[k]]<- length(party.indexes)
  }
}
full$ticket.unique <- ticket.unique


full$ticket.size[full$ticket.unique == 1]   <- 'Single'

full$ticket.size[full$ticket.unique < 5 & full$ticket.unique>= 2]   <- 'Small'

full$ticket.size[full$ticket.unique >= 5]   <- 'Big'

full <- full %>%
  
  mutate(Survived = case_when(Survived==1 ~ "Yes", 
                              
                              Survived==0 ~ "No"))

features<-full[1:891,c("Pclass","title","Sex","Embarked","FamilySize","ticket.size")]
features$Survived=as.factor(train$Survived)

set.seed(500)

ind=createDataPartition(features$Survived,times=1,p=0.8,list=FALSE)

train_val=features[ind,]

train_val<-mutate_if(train_val,is.character, as.factor)
################################################################################################
################################################################################################
#Rshiny

#Server
server <- function(input, output) {
  output$table <- renderDataTable({
    full_filterd <- full_display %>%
      filter(Sex %in% input$sex & Pclass %in% input$class & Survived %in% input$survived & Age_Group %in% input$age_group) %>%
      select(PassengerId:Fare)
    datatable(data = full_filterd, 
              options = list(pageLength = 10), 
              rownames = FALSE)
    
  })
  output$rf<-renderPlot({
    
    set.seed(17)
    model=randomForest(x = train_val[, -7], y = train_val[, 7], ntree =input$ntree,nodesize =input$node,mtry=input$mtry) 
    plot(model)
    output$rfoutcome <- renderPrint({
      print(model, digits = 3, signif.stars = FALSE)
    })
})
  output$error1<-renderText({
    
    print('The green curve represents the error in predicting the passenger to be alive.')
  })
  output$error2<-renderText({
    
    print('The red curve represents the error in predicting the passenger to be dead.')
  })
  output$error3<-renderText({
    
    print('The black curve represents the out of bag error.')
  })
  output$error4<-renderText({
    
    print('More details of the model are listed below:')
  })

  
}  

#Body
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "ti_data",
      selectInput(
        inputId='sex',label='Gender',
        choices=c('male','female'),multiple=TRUE,
        selectize=TRUE
      ),
      selectInput(
        inputId='class',label='Ticket Class',
        choices=c(1,2,3),multiple=TRUE,
        selectize=TRUE
      ),
      selectInput(
        inputId='survived',label='Survival',
        choices=c(0,1),multiple=TRUE,
        selectize=TRUE
      ),
      selectInput(
        inputId='age_group',label='Age group',
        choices=c("Age.00_12", "Age.13_17","Age.18_59","Age.60+"),multiple=TRUE,
        selectize=TRUE
      ),
      dataTableOutput(outputId='table')
      
    ),
    tabItem(tabName = "RandomForest",
            
            sliderInput(inputId = 'ntree', 
                        label = 'number of trees', 
                        min =1, max =1000, 
                        value =100),
            sliderInput(inputId = 'mtry', 
                        label = 'Number of variables considered in choosing each split', 
                        min =1, max =6, 
                        value =3),
            sliderInput(inputId = 'node', 
                        label = 'Minimum number of samples in node', 
                        min =1, max =100, 
                        value =10),
            plotOutput(outputId = 'rf'),
            textOutput(outputId ='error1' ),textOutput(outputId ='error2' ),
            textOutput(outputId ='error3' ),textOutput(outputId ='error4' ),
            verbatimTextOutput(outputId ='rfoutcome')
    )
  )
)



#Side bar
sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem(text='Titanic data',tabName='ti_data'),
    menuItem(text='RandomForest',tabName='RandomForest')
  )
)




ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = sidebar,
                    body = body
)
shinyApp(ui, server)


