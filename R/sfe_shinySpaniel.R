#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @import tidyverse
#' @import tidyterra
#' @import SpatialFeatureExperiment
#' @import tidyverse
#' 
#' @include shinyModules.R
#' @include shinyDash.R


# library(tidyverse)
# library(shiny)
# library(shinydashboard)
# library(shinyjs)
# #library(terra)
# library(tidyterra)
# #library(SpatialExperiment)
# library(SpatialFeatureExperiment)
# library(Spaniel)
# source("appFuncs.R")
# source("shinyModules.R")
# source("shinyDash.R")



ui <- shinydashboard::dashboardPage(
  header,
  sidebar,
  dashBody, shinyjs::useShinyjs()
)

server <- function(input, output, session){
  options(shiny.maxRequestSize=100*1024^3)
  
  controlVars <- reactiveValues(fileUploaded = FALSE, dropdownsLoaded = FALSE)
  spatialObj <- reactiveVal()
  
  observeEvent(input$file1, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- readRDS(inFile$datapath) #%>% unwrapSFE()
    spatialObj(data)
    controlVars$fileUploaded <- TRUE
  })
  
  
  
  observeEvent(input$file1, {
    if(controlVars$fileUploaded){
      updateSelectizeInput(session, NS("test","gene"), 
                        choices = rowData(spatialObj())$symbol, selected = NULL, server = TRUE)
      updateSelectizeInput(session, NS("test","sample"), 
                        choices = unique(spatialObj()$sample_id), selected = NULL, server = TRUE)
      updateSelectizeInput(session, NS("test","group"), 
                           choices = grep("^grp_", names(colData(spatialObj())), value = TRUE), selected = NULL, server = TRUE)
      updateSelectizeInput(session, NS("qc1","sample1"), 
                        choices = unique(spatialObj()$sample_id), selected = NULL, server = TRUE)
      updateSelectizeInput(session, NS("qc1","metric1"), 
                        choices = grep("^qc_", names(colData(spatialObj())), value = TRUE), selected = NULL, server = TRUE)
      updateSelectizeInput(session, NS("qc2","sample1"), 
                        choices = unique(spatialObj()$sample_id), selected = NULL, server = TRUE)
      updateSelectizeInput(session, NS("qc2","metric1"), 
                        choices = grep("^qc_", names(colData(spatialObj())), value = TRUE), selected = NULL, server = TRUE)
    }
      controlVars$dropdownsLoaded <- TRUE
  }) 
  
  observeEvent(input$file1, {
    if(controlVars$dropdownsLoaded){
      qcPlotServer("qc1", spatialObj())
      qcPlotServer("qc2", spatialObj())
      spanielPlotServer("test", spatialObj())
      
    }
    
    })
  
   
}


#' Run shinySpaniel
#'
#'
#' @return
#' @export
#'
#' @examples runShinySpaniel_SFE()
runShinySpaniel_SFE <- function(UI = ui, Server = server) {
  require(shiny)
  shinyApp(UI, Server)
}



