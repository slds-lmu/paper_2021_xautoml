# get data and model
setwd("C:/Users/rherb/Documents/RProjects/paper_2020_xautoml")
library(iml)
library(mlrMBO)
library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)
library(gower)
library(corrplot)
source("R/data_prep.r")
param_list = data_btss$tasks



# Define UI 
ui <- fluidPage(
  
  # App title 
  titlePanel("xAutoml Test App"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for general inputs
    sidebarPanel(
      selectInput("dataset", "Choose Dataset", c("BTSS", "KC1")),
      selectInput("lambda", "Exploration factor lambda", c(0.5, 1, 2))
     
      
    ),
    
    # Main panel for displaying outputs 
    mainPanel(
      # define different tabs
      tabsetPanel(
        tabPanel("Descriptive", br(),
                 fluidRow(h3("Descriptive Overview")), 
                 br(),
                 fluidRow(column(5, plotOutput("corr_train")), column(5, plotOutput("corr_test"))),  
                 br(),
                 fluidRow(column(5, verbatimTextOutput("sum_train")), column(5, verbatimTextOutput("sum_test")))),
        tabPanel("Feature Effects", br(),
                 fluidRow(h3("1d Feature Effects")), 
                 br(),
                 fluidRow(column(4, uiOutput("params")), column(6, uiOutput("center"))),
                 fluidRow(column(4, plotOutput("ale")), column(6, plotOutput("pdp"))),
                 br(), br(),
                 fluidRow(h3("2d Feature Interactions / Effects")), 
                 br(),
                 fluidRow(column(4, uiOutput("params1")), column(4, uiOutput("params2"))),
                 fluidRow(column(5, plotOutput("hstat")), column(5, plotOutput("hstat2feat"))), 
                 br(),
                 fluidRow(column(5, plotOutput("ale2d")), column(5, plotOutput("pdp2d")))),
        tabPanel("Feature Importance", br(), 
                 fluidRow(h3("Permutation Feature Importance")), 
                 br(),
                 fluidRow(plotOutput("featImp"))),
        tabPanel("Local Interpretations", br(),
                 fluidRow(h3("Local Interpretation")), 
                 br(),
                 fluidRow(column(4, uiOutput("obs1")), column(4, uiOutput("obs2"))),
                 br(),
                 fluidRow(column(5, h4("LIME")), column(5, h4("Shapley"))),
                 fluidRow(column(5, plotOutput("lime")), column(5, plotOutput("shap"))))
      )
      
    )
  )
)