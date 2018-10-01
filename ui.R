# ui.R

## Required packages
library(shiny)
library(DT)
library(koboloadeR)
library(highcharter)
library(devtools)
library(shinythemes)
install_github("mrdwab/koboloadeR")

## Create the user interface
## One row across the top for logging in and retrieving the datasets
##   available via those login credentials.
## A sidebar panel that lets you select a dataset to load.
## A main panel that has two tabs, one that shows the ID and names of
##   the datasets available, one that will show the contents of the
##   requested dataset.

x <- TRUE

shinyUI(fluidPage(theme = shinytheme("united"),
  headerPanel("MSNAIII Data Viewer"),
  fluidRow(
    column(3, textInput("username", label = "Username", value = "reach_initiative_ukraine")),
    column(3, passwordInput("password", label = "Password", value = "")),
    column(4, textInput("api", label = "API", value = "kobohr")),
    column(2, actionButton("listDatasets", "Access Datasets"))),
  hr(),
  conditionalPanel(
    condition = "input.listDatasets == 0",
    fluidRow(
      column(6, 
             h3("REACH MSNA"),
             h3("Usage"),
             p("Enter your", em("username"), ", ", em("password"), ", ", 
               "and the ", em("API"), " that you want to use and click",
               code("Access Datasets"), ". This will load the most recent data available."),
             p("To down load the dataset, scroll to the bottom and select the format (CSV, PDF, etc.) ", 
               code("OR"), " filter the data by date using the filter at the top of the table,
               It may briefly display an error while the dataset is 
               downloading.")
             )
             )
             ),
  conditionalPanel(
    condition = "input.listDatasets != 0",
    fluidRow(
      
      column(10,
             tabsetPanel(
               tabPanel( "Instructions", fluidRow(
                 column(6, 
                        h3("REACH MSNA"),
                        h3("Usage"),
                        p("Enter your", em("username"), ", ", em("password"), ", ", 
                          "and the ", em("API"), " that you want to use and click",
                          code("Access Datasets"), ". This will load the most recent data available."),
                        p("To down load the dataset, scroll to the bottom and select the format (CSV, PDF, etc.) ", 
                          code("OR"), " filter the data by date using the filter at the top of the table,
               It may briefly display an error while the dataset is 
               downloading.")
                 )
               )),
               tabPanel(
                 "R2P KII Data", h3("KII Statistics"),
                 h4("Statistics about the progress of the Key Informant MSNA survey. The target is a total of 240 surveys, 15 elderly and 15 adult in each settlement (8 settlements)"),
                 fluidRow(
                   column(6, highchartOutput("hcontainerKIIa", height = "250px"), highchartOutput("hcontainerKIIb", height = "250px"), highchartOutput("hcontainerKIIc", height = "250px"), highchartOutput("hcontainerKIId", height = "250px")), 
                   column(6, highchartOutput("hcontainerKIIe", height = "250px"), highchartOutput("hcontainerKIIf", height = "250px"), highchartOutput("hcontainerKIIg", height = "250px"), highchartOutput("hcontainerKIIh", height = "250px")),
                   column(12, highchartOutput("hcontainerKIIt"), align="center"),
                   column(12, highchartOutput("hcontainerKIIs"), align="center")
                 ),
                 dataTableOutput("datasetRequestedKII")), 
               tabPanel(
                 "R2P HH Data", h3("HH Statistics"),
                        h4("Statistics about the progress of the HouseHold MSNA survey. The target is a total of 975, 195 at each checkpoint (5 checkpoints)"),
                         fluidRow(
                           column(6, highchartOutput("hcontainerHHa", height = "200px"), highchartOutput("hcontainerHHb", height = "200px"), highchartOutput("hcontainerHHc", height = "200px")), 
                           column(6, highchartOutput("hcontainerHHd", height = "200px"),highchartOutput("hcontainerHHe", height = "200px"),highchartOutput("hcontainerHH", height = "200px")),
                           column(12, highchartOutput("hcontainerHHs"), align="center")
                         ),
                 dataTableOutput("datasetRequestedHH")
               ),
               tabPanel("Donbas SOS",h3("Donbas SOS Statistics"),
                        h4("1,560 household surveys - NGCA residents using hotlines for humanitarian assistance. The target is 195 per settlemnt area"),
                        fluidRow(
                          column(12, highchartOutput("hcontainerSOS"))
                        ),
                        dataTableOutput("datasetRequestedSOS"))
             )
      )
    )
  ),
  ## This is to fix the alignment of the "listDatasets" button with
  ##   the rest of the login details.
  tags$style(type='text/css', 
             "#listDatasets { width:100%; margin-top: 25px;}")
  )
  )