#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# library(shiny)

server <- function(input, output, session) {
  datasetInput <- reactive({
    switch(input$Dataset,
           "lung" = lung,
           "cgd" = cgd,
           "pbc" = pbc)
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  covariateInput <- reactive({
    input$covariate
  })
  
  
  output$DataSetTable <- renderDataTable({dataset <- datasetInput()
  datatable(dataset)})
  output$DataSetPlot1 <- renderPlot({
    dataset <- datasetInput()
    covariate <- as.character(noquote(covariateInput()))
    # print(class(covariate))
    fit <- survfit(Surv(time, status) ~ dataset[,covariate], data = dataset)
    # p <- ggsurvplot(fit)
    # print(dataset)
    p <- plot(fit)
    # p <- ggsurvplot(fit
    #                 # ,
    #                 # data = dataset,
    #                 # title = paste("Survival Curves"),
    #                 # pval = TRUE, pval.method = TRUE,    # Add p-value &  method name
    #                 # surv.median.line = "hv",            # Add median survival lines
    #                 # # legend.title = covariate,               # Change legend titles
    #                 # # legend.labs = c("Male", "female"),  # Change legend labels
    #                 # palette = "jco",                    # Use JCO journal color palette
    #                 # risk.table = TRUE,                  # Add No at risk table
    #                 # cumevents = TRUE,                   # Add cumulative No of events table
    #                 # tables.height = 0.15,               # Specify tables height
    #                 # tables.theme = theme_cleantable(),  # Clean theme for tables
    #                 # tables.y.text = FALSE               # Hide tables y axis text
    # )
    p
  })
  
  observe({
    updateSelectInput(session, "covariate", choices = names(datasetInput())) 
  })
  
  # fit <- survfit(Surv(time, status) ~ sex, data = lung)
  # p <- ggsurvplot(fit, data = lung,
  #                 title = "Survival Curves",
  #                 pval = TRUE, pval.method = TRUE,    # Add p-value &  method name
  #                 surv.median.line = "hv",            # Add median survival lines
  #                 legend.title = "Sex",               # Change legend titles
  #                 legend.labs = c("Male", "female"),  # Change legend labels
  #                 palette = "jco",                    # Use JCO journal color palette
  #                 risk.table = TRUE,                  # Add No at risk table
  #                 cumevents = TRUE,                   # Add cumulative No of events table
  #                 tables.height = 0.15,               # Specify tables height
  #                 tables.theme = theme_cleantable(),  # Clean theme for tables
  #                 tables.y.text = FALSE               # Hide tables y axis text
  # )
  # 
  # output$DataSetPlot1 <- renderPlot({
  #   p
  # })
  
  
  
  
  observe(if (input$Dataset == 'lung'){ output$DataSetInfo <- renderText({
    paste('NCCTG Lung Cancer Data',
          'Survival in patients with advanced lung cancer from the North Central Cancer Treatment Group. Performance scores rate how well the patient can perform usual daily activities.',
          
          '',
          
          'inst:	 Institution code',
          'time: Survival time in days',
          'status: censoring status 1 = censored, 2 = dead',
          'age: age in years',
          'sex: Male = 1 Female = 2',
          'ph.ecog: ECOG performance score(0 = good 5 = dead',
          'ph.karno: Karnofsky performance score (bad=0-good=100) rated by physician',
          'pat.karno: Karnofsky performance score as rated by patient',
          'meal.cal: Calories consumed at meals',
          'wt.loss: Weight loss in last six months',
          '',
          'Source: Terry Therneau',
          sep="\n")
  })
  
  })
  observe(if (input$Dataset == 'cgd'){output$DataSetInfo <- renderText({
    paste('Chronic Granulotomous Disease data',
          'Data are from a placebo controlled trial of gamma interferon in chronic granulotomous disease (CGD). Contains the data on time to serious infections observed through end of study for each patient.',
          '',
          'The cgd0 data set is in the form found in the references, with one line per patient and no recoding of the variables. The cgd data set (this one) has been cast into (start, stop] format with one line per event, and covariates such as center recoded as factors to include meaningful labels.',
          '',
          'id subject identification number',
          'center enrolling center',
          'random date of randomization',
          'treatment placebo or gamma interferon',
          'sex sex',
          'age age in years, at study entry',
          'height height in cm at study entry',
          'weight weight in kg at study entry',
          'inherit pattern of inheritance',
          'steroids use of steroids at study entry,1=yes',
          'propylac use of prophylactic antibiotics at study entry',
          'hos.cat a categorization of the centers into 4 groups',
          'tstart, tstop start and end of each time interval',
          'status 1=the interval ends with an infection',
          'enum observation number within subject',
          '',
          'source: Fleming and Harrington, Counting Processes and Survival Analysis, appendix D.2.',
          sep="\n")
  })})
  
  observe(if (input$Dataset == 'pbc'){output$DataSetInfo <- renderText({
    paste('Mayo Clinic Primary Biliary Cirrhosis Data',
          '',
          'This data is from the Mayo Clinic trial in primary biliary cirrhosis (PBC) of the liver conducted between 1974 and 1984. A total of 424 PBC patients, referred to Mayo Clinic during that ten-year interval, met eligibility criteria for the randomized placebo controlled trial of the drug D-penicillamine. The first 312 cases in the data set participated in the randomized trial and contain largely complete data. The additional 112 cases did not participate in the clinical trial, but consented to have basic measurements recorded and to be followed for survival. Six of those cases were lost to follow-up shortly after diagnosis, so the data here are on an additional 106 cases as well as the 312 randomized participants.',
          '',
          'age:	 in years',
          'albumin:	 serum albumin (g/dl)',
          'alk.phos:	 alkaline phosphotase (U/liter)',
          'ascites:	 presence of ascites',
          'ast:	 aspartate aminotransferase, once called SGOT (U/ml)',
          'bili:	 serum bilirunbin (mg/dl)',
          'chol:	 serum cholesterol (mg/dl)',
          'copper:	 urine copper (ug/day)',
          'edema:	 0 no edema, 0.5 untreated or successfully treated 1 edema despite diuretic therapy',
          'hepato:	 presence of hepatomegaly or enlarged liver',
          'id:	 case number',
          'platelet:	 platelet count',
          'protime:	 standardised blood clotting time',
          'sex:	 m/f',
          'spiders:	 blood vessel malformations in the skin',
          'stage:	 histologic stage of disease (needs biopsy)',
          'status:	 status at endpoint, 0/1/2 for censored, transplant, dead',
          'time:	 number of days between registration and the earlier of death,',
          'transplantion, or study analysis in July, 1986',
          'trt:	 1/2/NA for D-penicillmain, placebo, not randomised',
          'trig:	 triglycerides (mg/dl)',
          sep="\n")
  })})
  
  
  
  
  
  
}



