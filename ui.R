library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
require(reshape2)
library(rAmCharts)
library(rlang)
library(shinyjs)
library(rhandsontable)
source("Utils.R")
library(purrr)

ui <- dashboardPage( 
  
  dashboardHeader(title = "CRUD Application"),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(useShinyjs(),
                tags$head(tags$style(".content-wrapper {background-color: white;}")), tags$head(tags$style(".shiny-output-error { visibility: hidden; }")),
                tags$script(src = "myscript.js"),
                          fluidRow(offset = 0, 
                                   column(width = 3, div(selectInput("dbname", "Database",choices = c("","AdventureWorks2012"), width = 250),style = "padding:20px;"), offset = 0),
                                   column(width = 3, div(selectInput("tname", "Table",choices = c("",schema_table), width = 250),style = "padding:20px;")),
                                   br(),br(),
                                   column(width = 2, uiOutput("create_button")),
                                   column(width = 2, uiOutput("update_button")),
                                   column(width = 2, uiOutput("delete_button"))),
                          fluidRow(column(width = 6, div(actionButton("submit", "Submit",width = "100%"),offset = 3.5, style = "padding-left:20px;")),
                                   column(width = 6, div(actionButton("refresh", "Refresh",width = "100%"),offset = 3.5, style = "padding-right:20px;"))),
                          fluidRow(column(width = 8, div(dataTableOutput("input_table"),style = "padding:20px;")),
                                   column(width = 4, 
                                     fluidRow(column(width = 12, br(), br(),textOutput("key"))),
                                     fluidRow(column(width = 12, htmlOutput("Ikey"))),
                                     fluidRow(column(width = 4, div(rHandsontableOutput("table_data_type",height = 300),style = "padding:20px;")))
                                     ))
                          ))