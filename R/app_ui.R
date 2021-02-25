#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyAce
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      dashboardHeader(title = "SurvivalHazardExtraction",titleWidth = 300),
      dashboardSidebar(
        width = 300,
        sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
        sidebarMenu(
          menuItem("Dashboard", tabName  = "dashboard", icon = icon("tachometer-alt")),
          menuItem("Data and Summary", tabName = "data", icon = icon("th")),
          menuItem("Model Specification", tabName = "model", icon = icon("th")),
          menuItem("Result", tabName = "results", icon = icon("th"),
                   menuSubItem('Model estimates', tabName = "mest"),
                   menuSubItem('Covariance & Correlation Matrix', tabName = "covmat")
                   )
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "dashboard",
            h2("Introduction"),
            fluidRow(
              box(
                h4('This is a dashboard for extracting survival model parameter estimates for health economic modeling'),
                h4("There are several important key points to remember"),
                h4(strong("Number 1:"),"User needs to have his/her own .csv file to input into the 'Data and Summary' Tab (Second tab on the left"),
                h4(strong("Number 2:"),"User needs to have enough domain knowledge and information regarding the relationship between variables to set up the model equation in the 'Model Specification' Tab (Third tab on the left"),
                h4(strong("Number 3:"),"In addition to the model specification, user needs to be familiar with the R syntax in setting up the equations for the survival analysis"),
                h4(strong("Number 4:"),"The outputs that are generated in the 'Result' tab (Bottom tab on the left) needs the user to be comfortable with the 'survival' package output. (There is a link in the model specification to the `survival` package vignette)")
                
                ,width = 13)
            )
            
          ),
          tabItem(
            tabName = "data",
            
            h2("Please Enter CSV Data"),
            
            fileInput('datafile', 'Choose CSV file',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain')),
            
            h3("Descriptive Statistics: Select variables"),
            uiOutput("Variables"),
            
            # View summary statistics
            h3("1. Summary Statistics"),
            
            fluidRow( verbatimTextOutput("sum")),
            
            # Create row to check the data
            h3("2. Data View"),
            fluidRow( dataTableOutput("filetable")),
            
            strong('R session info'),
            verbatimTextOutput("info")
          ),
          tabItem(
            tabName = "model",
            
            
            p('Estimation may take a few seconds to minutes depending on the dataset.'),
            
            h3("Analysis"),
            
            h3("Specify the model and/or variables"),
            
            p('For a vignette of survival analysis, see',
              a("survival", href="https://cran.r-project.org/web/packages/survival/vignettes/survival.pdf", target="_blank")
            ),
            
            fluidRow(
              box(
                width = 10,
                h4("Specify regression equation according to the survreg syntax below:"),
                aceEditor("model", mode="r", value="Surv(time, status) ~ age + sex + ph.ecog"
                          ,
                          height = "50px")
              ),
              box(
                width = 10,
                h4("Specify distribution of time to event below:"),
                h4('Options include: "weibull", "exponential", "gaussian", "logistic","lognormal" and "loglogistic"'),
                aceEditor("model_dist", mode="r", value="exp"
                          ,
                          height = "10px")
              )
              
            )
          ),
          tabItem(
            tabName = "mest",
            
            h2("Survival Regression Result"),
            
            fluidRow(
              box(
                h3("Regression Output"),
                # verbatimTextOutput("result1")
                DT::dataTableOutput("result1")
                ,width = 12
              )
            )
            
          ),
          tabItem(
            tabName = "covmat",
            
            h2("Variance-Covariance & Correlation Matrix"),
            
            fluidRow(
              box(
                h3("VCOV Output"),
                # verbatimTextOutput("result2")
                DT::dataTableOutput("result2")
                ,width = 12
              )
            ),
            fluidRow(
              box(
                h3("CORR Output"),
                # verbatimTextOutput("result3")
                DT::dataTableOutput("result3")
                ,width = 12
              )
            )
            
          )
        )
        
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'SurvivalHazardExtraction'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}
