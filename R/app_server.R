#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import DT
#' @import psych
#' @import dplyr
#' @import formattable
#' @import survival
#' @import broom
#' @noRd
app_server <- function( input, output, session ) {

  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
    
  })
  
  # This allows user to select variables in the filedata
  output$Variables <- renderUI({
    selectInput('vars', 'Variables for descriptive statistics', names(filedata()) , multiple = TRUE)
  })
  
  redfile <- reactive({
    dplyr::select(filedata(), input$vars)
  })
  
  #This previews the CSV data file
  output$filetable <- renderDataTable({
    redfile()
  })
  
  output$sum <- renderPrint({
    describe(redfile())
  })
  
  # R session info
  output$info <- renderPrint({
    sessionInfo
  })
  
  # output$Variablessurvfit <- renderUI({
  #   selectInput('vars2', 'Variables', names(filedata()) , multiple = TRUE)
  # })
  
  get.text <- reactive({
    input$model
  })
  
  get.text.dist <- reactive({
    input$model_dist
  })
  
  # Use Fit Model
  est <- reactive({
    
    dat <- filedata()
    
    mod_formula <- get.text()
    mod_dist <- get.text.dist()
    fit <- survreg(formula = formula(mod_formula), dist=mod_dist, data = dat)
    
    list(fit = fit)
  })
  
  ## HR Estimates
  res_1 <- reactive({
    # res_hr <- summary(est()$fit)
    res_hr <- est()$fit
    res_hr <- res_hr %>% tidy
    res_hr
  })
  
  ## VCOV and COR
  res_2 <- reactive({
    res_vcov <- vcov(est()$fit)
    res_corr <- cov2cor(res_vcov)
    
    # convert to dataframe
    vcovcolname <- dimnames(res_vcov)[[1]]
    res_vcov <- res_vcov %>%
      data.frame  
    colnames(res_vcov) <- vcovcolname
    
    res_corr <- res_corr %>%
      data.frame  
    colnames(res_corr) <- vcovcolname
    
    list(
      vcov = res_vcov,
      corr = res_corr
    )
  })
  
  output$result1 <- DT::renderDataTable({
    res_1()
  })
  output$result2 <- DT::renderDataTable({
    datatable(res_2()$vcov,rownames = TRUE)
  })
  output$result3 <- DT::renderDataTable({
    datatable(res_2()$corr,rownames=TRUE)
  })
}
