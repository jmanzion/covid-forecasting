library(reticulate)
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(dashboardthemes)
library(plotly)
library(zoo)
library(shinycssloaders)
library(shinydashboardPlus)
library(shinyWidgets)
library(lubridate)

#uncomment below if need to install python libraries
#py_install(c('pandas', 'numpy', 'scipy'))

#uncomment code below if needed for r shiny deployment
#reticulate::virtualenv_create("python35_env", python = "python3")
#reticulate::virtualenv_install("python35_env", packages = c("pandas", "numpy","scipy"))
#reticulate::use_virtualenv("python35_env", required = TRUE)

reticulate::source_python('generalized_growth.py')
source('generalized_growth.R')

# Get daily new cases from Our World In Data Repository
covid <- vroom::vroom('https://covid.ourworldindata.org/data/owid-covid-data.csv')

# remove non country rows e.g. continents and income classes
# remove unwanted columns
covid = covid %>% filter(!grepl("OWID_", iso_code)) %>% select(date, location, new_cases, new_cases_smoothed)

# format date
covid$date <- as.Date(covid$date, format =  "%Y-%m-%d")

# define sidebar tabs 
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("About this App", tabName = "about", icon = icon("info-circle")),
              menuItem("Data load & display", tabName = "data", icon = icon("chart-line")),
              menuItem("Model & parameter selection", tabName = "model", icon = icon("tools")),
              menuItem("Model forecast & display", tabName = "forecast", icon = icon("chart-line"))
  )
)

# define tab body contents
body <- dashboardBody(
  
  shinyDashboardThemes(
    # other theme options: https://github.com/nik01010/dashboardthemes
    #theme = "grey_light"  
    theme = "onenote"
  ),
  tabItems(
    tabItem(tabName = "about",
            h2("Accesible Infectious Disease Forecasting"),
            
            ####################################
            h3("Background"),
            p("Because of data sharing restrictions, lack of available modeling 
              expertise, or other limitations, it is not uncommon to have access
              to local infectious disease data without the means for effective
              modeling. This app provides the ability to model and forecast infectious diseases (e.g. COVID-19). 
              The user can select current COVID-19 data from a list of countries or upload their own data. 
              This data can be used to model and forecast using one of the available phenomenological 
              models described below."),
            
            ####################################
            h3("Instructions"),
            p("Instructions and guidelines for using this app are provided below."),
            
            ####################################
            h4("Data load & display"),
            p("As an initial step, the user has the option to 1) use preloaded global COVID-19 case data from 
              a list of countries or 2) upload a new CSV, TSV or TXT file of
              case data (must contain columns named 'date' and 'new_cases'). 
              The user can select the preferred data source and generate
              a visual for inspection. A 7-point (e.g. days) rolling average is computed and used as input to 
              the model. Chart options include zoom, pan and downloading the plot as a PNG file."),
            
            ####################################
            h4("Model & parameter selection"),
            p("In this tab, user can select the date range, type of model, model parameters and number of 
              datapoints (e.g. days in the future) to be used for modeling and forecast."),
            p("The default model parameters and their upper and lower bounds are pre-populated but 
              can be changed, as needed. Parameters required for each model are described below:"),
            strong("Exponential growth model:"), span("r = Intrinsic growth rate"), br(),
            strong("Generalized growth model:"), span("r = Intrinsic growth rate, p = Deceleration of growth"), br(),
            strong("Logistic growth model:"), span("r = Intrinsic growth rate, K = Maximum cumulative case incidence"), br(),
            strong("Generalized logistic growth model:"), span("r = Intrinsic growth rate, p = Deceleration of 
                                                               growth, K = Maximum cumulative case incidence"), br(),
            strong("Generalized Richards model:"), span("r = Intrinsic growth rate, p = Deceleration of growth, 
                                                        K = Maximum cumulative case incidence, a = Scaling parameter"), br(),
            
            ####################################
            h4("Guidelines for model selection:"),
            p("Based on our initial testing, we recommend the following:"),
            strong("Generalized growth model and exponential growth model"), span("are suitable for the early phase of 
                                                                                  an outbreak wave, when the 
                                                                                  incidence is going up."), br(),
            strong("Generalized logistic growth model and logistic growth model"), span("are suitable for the phase 
                                                                                        of a wave where the outbreak is 
                                                                                        slowing down or has passed 
                                                                                        its peak."), br(),
            strong("Generalized Richards model"), span("encompasses all of the four above-mentioned models when one or 
                                                       some parameters get fixed. This gives a curve similar to 
                                                       generalized logistic model but with a longer tail, allowing 
                                                       slower decline rate once the outbreak has passed its peak."), br(),
            strong("Sub-epidemics model"), span("is suitable for outbreaks with reasonable assumption of multiple 
                                                underlying sub-epidemics, and outbreaks with even slower decline 
                                                rate after the peak than the Generalized Richards model."), br(),
            strong("Disclaimer:"), span("This family of models may not work well for a concave curve. Although 
                                        the models can handle concave curve by allowing r<0, it is still un-clear 
                                        how to effectively switch r from positive to negative. We recommend starting 
                                        a new wave fitting once 5-7 data points indicate the rise."), br(),
            
            
            ####################################
            h4("Model forecast & display"),
            p("Once the model is finished running a plot of the results can be viewed. It includes 
              new cases from the input data and projected new cases including upper and lower 
              bound of the confidence interval. Options to pan, zoom, and download the chart as a 
              PNG file are also available."),
            
            ####################################
            h3("References"),
            tags$li(a(href="https://covid.ourworldindata.org/data/owid-covid-data.csv", 
                      "Our World In Data Repository (COVID-19 data)"))
    ),
    tabItem(tabName = "data", 
            fluidRow(
              box(radioButtons("dataSource","Data Source:", c("Use Pre-loaded Data", "Upload Data")),
                  uiOutput("ui"),
                  actionButton("update","Update Chart"),
                  actionButton("reset", "Reset Chart")
              ),
              
              box(uiOutput("selector2"), 
                  uiOutput("selector1"))
            ),
            
            fluidRow(
              textOutput("placeholder"),
              plotlyOutput("covidplot") %>% withSpinner(color="#0dc5c1",type=5),
              box(uiOutput("select"))
            )
    ),
    tabItem(tabName = 'model',
            fluidRow(
              box(title = 'Chose date range, or time range:',
                  dateRangeInput("daterange1", "Date range:",
                               start = "2019-12-01",
                               end   = Sys.Date()-1),
                  numericRangeInput(inputId = "daterange2", label = "Time range:",
                                    value = c(1, 400)),
                  radioButtons("preprocess", "Data Pre-processing:", c("Raw data", "7-day smoothing"))),
              box(title = 'Model inputs',
                  selectizeInput("modelChoice", "Model:", 
                                 choices = c("Exponential Growth",
                                             "Generalized Growth",
                                             "Logistic Growth",
                                             "Generalized Logistic Growth",
                                             "Generalized Richards")),
                  
                  #
                  textInput('inits', 'Initial Guess of Parameters', value = "0.5", placeholder = "Enter r"),
                  textInput('lb', 'Lower Bound of Parameters', value = "0", placeholder = "Enter r"),
                  textInput('ub', 'Upper Bound of Parameters', value = "100", placeholder = "Enter r"),
                  selectizeInput("optim", "Optimization Algorithm", 
                                 choices = c("trf", "dogbox", "lm")),
                  numericInput("nsim", "Number of simulations for uncertainty estimation:", value = 200, min = 0)
              ),
              box(title = 'Forecast inputs',
                  numericInput("fct", "Number of data points forcasted:", value = 5, min = 0)),
              
              box(actionButton('run', 'Run Model'))
            )
    ),
    
    tabItem(tabName = "forecast", 
            fluidRow(
              tabBox(width = 12,
                tabPanel("Parameter estimates", 
                         plotlyOutput("histograms") %>% withSpinner(color="#0dc5c1",type=5),
                         verbatimTextOutput('additionalParams')),
                tabPanel("Model goodness-of-fit", tableOutput('modelFit')),
                tabPanel("Forecasting", plotlyOutput("finalplot") %>% withSpinner(color="#0dc5c1",type=5))
                #tabPanel("Forecasting", verbatimTextOutput("preview1"))
              )#,
              #use_busy_spinner(spin = "fading-circle"),
              #plotlyOutput("finalplot") %>% withSpinner(color="#0dc5c1",type=5)
            )
    )
    
  )
)



# Combine elements of dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Covid Forecasting"),
  sidebar,
  body
)

server <- function(input,output,session){
  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$dataFile, {
    values$upload_state <- 'uploaded'
  })
  toListen <- reactive({
    list(input$dataSource,input$countryInput,input$reset)
  })
  observeEvent(toListen(), {
    values$upload_state <- 'reset'
    output$ui <- renderUI({
      if (is.null(input$dataSource)){
        return()
      }
      switch(input$dataSource,
             "Upload Data" = fileInput("dataFile","Choose csv, txt, or tsv File to Upload")
      )
    })
  })
  
  file_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$dataFile)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  output$ui <- renderUI({
    if (is.null(input$dataSource))
      return()
    
    switch(input$dataSource,
           "Upload Data" = fileInput("dataFile","Choose CSV File",accept = ".csv")
    )
  })
  
  dat <- reactive({
    if(input$dataSource == 'Use Pre-loaded Data'){
      covid
    }
    else{
      file <- file_input()
      validate(need(file,"Need to upload a file"))
      ext <- tools::file_ext(file$name)
      switch(ext,
             csv = vroom::vroom(file$datapath, delim = ","),
             tsv = vroom::vroom(file$datapath, delim = "\t"),
             txt = vroom::vroom(file$datapath, delim= "\t"),
             validate("Invalid file; Please upload a .csv, .tsv, or .txt file")
      )
      if(ext == 'csv'){
        read.csv(file$datapath,header = F, sep=',')
      }
      else{
        read.csv(file$datapath,header = F)
      }
      
    }
  })
  
  
  output$selector1 <-renderUI({
    if (input$dataSource == 'Use Pre-loaded Data'){
      selectizeInput("countryInput","Country:",
                     choices=unique(covid$location),
                     selected="United States", multiple=FALSE)
    } else {
      selectizeInput("caseColumn","Case incidence column:",
                     choices=colnames(dat()),
                     selected=colnames(dat())[1], multiple=FALSE)
      
    }
  })
  
  output$selector2 <-renderUI({
    if (input$dataSource != 'Use Pre-loaded Data'){
    
      selectizeInput("dateColumn","Time column:",
                     choices=colnames(dat()),
                     selected="None", multiple=FALSE)
    }
  })
  c <- eventReactive(input$update,{
    if(input$dataSource == 'Use Pre-loaded Data'){
      dat()  %>%
        filter(location == input$countryInput,
               date >= min(date),
               date <= max(date))
    }
    else{
      if (identical(input$dateColumn, "NULL")){
        n <- nrow(dat())
        dat() %>% rename(new_cases=input$caseColumn) %>%
          mutate(date=1:n) %>% select(date, new_cases)%>% mutate(new_cases_smoothed = zoo::rollmean(new_cases,k=7,fill=NA))
      } else{
        if (date_convertible(dat()[,input$dateColumn])){
          dat() %>% rename(new_cases=input$caseColumn, date = input$dateColumn) %>%
            mutate(date = as.Date(as.character(date),tryFormats = c("%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%Y-%m-%d"))) %>%
            select(date, new_cases)%>% mutate(new_cases_smoothed = zoo::rollmean(new_cases,k=7,fill=NA))
        } else {
          dat() %>% rename(new_cases=input$caseColumn, date = input$dateColumn)%>%
            select(date, new_cases)%>% mutate(new_cases_smoothed = zoo::rollmean(new_cases,k=7,fill=NA))
        }
      }
      
      
    }
    
  })
  
  
  observeEvent(input$update,{
    if (is.Date(dat()$date)){
      updateDateRangeInput(session, "daterange1", "Date range:",
                           start = min(c()$date+7),
                           end   = Sys.Date()-1)}
    # } else {
    #   updateNumericRangeInput(session, "daterange2", "Time range:",
    #                        start = min(c()$date),
    #                        end   = max(c()$date))
    # }
     
  })
  

  output$placeholder <- renderText({"You must select a country and data source and click 'Update Chart' to continue"})
  observeEvent(input$update,{
    output$placeholder <- NULL
    output$select <- renderUI({actionButton("setup","Select Model")})
  })
  
  output$covidplot <- renderPlotly({
    plot_ly(c(), x = ~date, y = ~new_cases, type = 'scatter', mode = 'lines', 
            name = "Daily New Cases") %>%
      add_trace(c(), x = ~date, y = ~new_cases_smoothed, type = 'scatter', 
                mode = 'lines', name = "7 Day Moving Avg.") %>%  
      layout(yaxis = list(title = 'Daily New Cases'), 
             xaxis = list(title = 'Date'), 
             hovermode = "x unified")
  })
  observeEvent(input$setup, {
    updateTabItems(session, "tabs", selected = "model")
  })
  
  dat_subset <- reactive({
    if (is.Date(c()$date)){
      filter(c(), between(date, input$daterange1[1], input$daterange1[2]))
    } else {
      filter(c(), between(date, input$daterange2[1], input$daterange2[2]))
    }
    
  })
  
  case_sum <- reactive({
    if (input$preprocess == "Raw data"){
      dat_subset() %>%
        summarise(sum(new_cases, na.rm=T)) %>% unlist()
    } else {
      dat_subset() %>%
        summarise(sum(new_cases_smoothed, na.rm=T)) %>% unlist()
    }
    
  })
  
  cases <- reactive({
    if (input$preprocess == "Raw data"){
      dat_subset()$new_cases
      } else {
      dat_subset()$new_cases_smoothed
        }
    })
  timefit <- reactive({1:length(cases())})
  
  observe({
    modelchoice = input$modelChoice
    
    if (modelchoice == "Exponential Growth"){
      updateTextInput(session, 'inits', 'Enter Initial Guess of Parameters r', value = "0.5", placeholder = "Enter r")
      updateTextInput(session, 'lb', 'Enter Lower Bound of Parameters r', value = "0", placeholder = "Enter r")
      updateTextInput(session, 'ub', 'Enter Upper Bound of Parameters r', value = "100", placeholder = "Enter r")
    }
    else if (modelchoice == 'Generalized Growth'){
      updateTextInput(session, 'inits', 'Enter Initial Guess of Parameters r, p', value = "0.5, 0.5", placeholder = "Enter r, p")
      updateTextInput(session, 'lb', 'Enter Lower Bound of Parameters r, p', value = "0, 0", placeholder = "Enter r, p")
      updateTextInput(session, 'ub', 'Enter Upper Bound of Parameters r, p', value = "100, 1", placeholder = "Enter r, p")
    }
    else if (modelchoice == 'Logistic Growth'){
      updateTextInput(session, 'inits', 'Enter Initial Guess of Parameters r, K', value = paste("0.5, ", round(case_sum())), placeholder = "Enter r, K")
      updateTextInput(session, 'lb', 'Enter Lower Bound of Parameters r, K', value = paste("0, ", round(case_sum()/2)), placeholder = "Enter r, K")
      updateTextInput(session, 'ub', 'Enter Upper Bound of Parameters r, K', value = "100, 100000000", placeholder = "Enter r, K")
    }
    else if (modelchoice == 'Generalized Logistic Growth'){
      updateTextInput(session, 'inits', 'Enter Initial Guess of Parameters r, p, K', value = paste("0.5, 0.5, ", round(case_sum())), placeholder = "Enter r, p, K")
      updateTextInput(session, 'lb', 'Enter Lower Bound of Parameters r, p, K', value = paste("0, 0, ", round(case_sum()/2)), placeholder = "Enter r, p, K")
      updateTextInput(session, 'ub', 'Enter Upper Bound of Parameters r, p, K', value = paste("100, 1, ", max(100000000, case_sum()*5)) , placeholder = "Enter r, p, K")
    } 
    else if (input$modelChoice == "Generalized Richards"){
      updateTextInput(session, 'inits', 'Enter Initial Guess of Parameters r, p, a, K', value = paste("0.5, 0.5, 1,", round(case_sum())), placeholder = "Enter r, p, a, K")
      updateTextInput(session, 'lb', 'Enter Lower Bound of Parameters r, p, a, K', value = paste("0, 0, 0, ", round(case_sum()/2)), placeholder = "Enter r, p, a, K")
      updateTextInput(session, 'ub', 'Enter Upper Bound of Parameters r, p, a, K', value = paste("100, 1, 10, ", max(100000000, case_sum()*5)), placeholder = "Enter r, p, K, a")
    }
    else {
      updateTextInput(session, 'inits', 'Enter Initial Guess of Parameters r, p, a, q, K', value = paste("0.2, 0.9, 1, 0.3, ", round(case_sum())), placeholder = "Enter r, p, K, a, q")
      updateTextInput(session, 'lb', 'Enter Lower Bound of Parameters r, p, a, q, K', value = paste("0, 0, 0, 0, ", round(case_sum()/2)), placeholder = "Enter r, p, K, a, q")
      updateTextInput(session, 'ub', 'Enter Upper Bound of Parameters r, p, a, q, K', value = paste("100, 1, 10, 5, ", max(100000000, case_sum()*5)), placeholder = "Enter r, p, a, q, K")
    }
    
  })

  
  flag <- reactive({
    if (input$modelChoice == "Exponential Growth")
      1
    else if (input$modelChoice == "Generalized Growth")
      2
    else if (input$modelChoice == "Logistic Growth")
      3
    else if (input$modelChoice == "Generalized Logistic Growth")
      4
    else if (input$modelChoice == "Generalized Richards")
      5
    else if (input$modelChoice == "Sub-epidemics")
      6
  })

  
  
  IC <- reactive({cases()[1]})
  
  inits <- reactive({as.numeric(unlist(strsplit(input$inits,",")))})
  
  lb <- reactive({as.numeric(unlist(strsplit(input$lb,",")))})
  
  ub <- reactive({as.numeric(unlist(strsplit(input$ub,",")))})
  
  bounds <- reactive({tuple(lb(), ub())})
  
  timefc <- reactive({1:(length(cases()) + input$fct)})
  ################################
  # 
  # Ptrue <- reactive(fittingSimpleGrowth(cases(), IC(), inits(), bounds(), timefit(), flag(), method=input$optim))
  # Phats <- reactive(confint(Ptrue(), cases(), IC(), inits(), bounds(), timefit(), flag(), input$nsim, method =input$optim))
  # 
  # date_fc <- reactive({as.Date(input$daterange[1]:(input$daterange[2] + input$fct))})
  # 
  # pred_date <- reactive(predint(Ptrue(), Phats(), cases(), IC(), timefc(), flag(), date_fc()))
  ########################################
  Ptrue1 <- reactive({
    if (flag() != 6)
      fittingSimpleGrowth(cases(), IC(), inits(), bounds(), timefit(), flag(), method=input$optim)
    else
      fittingModifiedLogisticGrowthPatch(cases(),inits(), bounds(), timefit(), method=input$optim)
    
    })
  n_patch <- reactive({if (flag()==6) Ptrue1()[[2]]})
  onset_thr <- reactive({if (flag()==6) Ptrue1()[[3]]})
  
  Ptrue <- reactive({if (flag()==6) Ptrue1()[[1]] else Ptrue1()})
  Phats <- reactive({
    if (flag() != 6)
      confint(Ptrue(), cases(), IC(), inits(), bounds(), timefit(), flag(), input$nsim, method =input$optim)
    else
      confint2(Ptrue(), n_patch(), onset_thr(), cases(), IC(), inits(), bounds(), timefit(),  input$nsim, method =input$optim)
    
    })
  
  date_fc <- reactive({
    if (is.Date(c()$date)){
      as.Date(input$daterange1[1]:(input$daterange1[2] + input$fct))
    } else {
      input$daterange2[1]:(input$daterange2[2] + input$fct)
    }
    })

  pred_date <- reactive({
    if (flag() != 6)
      predint(Ptrue(), Phats(), cases(), IC(), timefc(), flag(), date_fc())
    else
      predint2(Ptrue(), Phats(), cases(), IC(), timefc(), n_patch(), onset_thr(), date_fc())
    
    })
  
  bestfit <- reactive({
    if (flag() != 6){
      solvedSimpleGrowth(Ptrue(), IC(), timefit(), flag())
    } else {
      solvedModifiedLogisticGrowthPatch(Ptrue(),IC(),timefit(),npatch(),onset_thr())
    }
  })
  
  good_fit <- reactive({
    evaluation(cases(), bestfit())
  })

  
  observeEvent(input$run,{updateTabItems(session, "tabs", selected="forecast")})
  
  
    
  
  output$histograms <- renderPlotly({
    plot_hist(Ptrue(), Phats())
  })
  
  output$modelFit <- renderTable(good_fit())
  
  output$finalplot <- renderPlotly({
    plot_ly(pred_date(), x = ~date, y = ~observed, type = 'scatter', mode = 'markers',
            name = "Observed Daily New Cases") %>%
      
      add_trace(pred_date(), x = ~date, y = ~fitted, mode = 'lines', type = 'scatter', line = list(dash = 'dash'),
                name = "Modeled Daily New Cases") %>%
      
      add_trace(pred_date(), x = ~date, y = ~p975, type = 'scatter', 
                mode = 'lines', name = "Upper CI", showlegend = FALSE, line=list(color = 'transparent')) %>%  
      
      add_trace(pred_date(), x = ~date, y = ~p25, type = 'scatter', 
                mode = 'lines', name = "Lower CI", showlegend = FALSE, line=list(color = 'transparent'),
                fillcolor='rgba(68, 68, 68, 0.3)', fill='tonexty') %>%  
      
      layout(title = "Projected Daily New Cases from the Model",
             yaxis = list(title = 'Daily New Cases'), 
             xaxis = list(title = 'Date'),
             hovermode = "x unified")
  })
  
  output$additionalParams <- renderPrint({
    if (flag()==6){
      add_params <- reactive(data.frame(Parameter = c('n_epidemics', 'onset_threshold'), Estimate = c(n_patch(), onset_thr())))
      print(add_params())
    }
    
  })
}

shinyApp(ui = ui, server = server)