# get data and model
setwd("C:/Users/rherb/Documents/RProjects/paper_2020_xautoml")
#library(iml)
library(mlrMBO)
library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)
library(gower)
library(corrplot)
library(shinycssloaders)
library(plotly)
source("R/data_prep.r")
param_list = data_btss$tasks

devtools::load_all("../moc/iml", export_all = FALSE)
devtools::load_all("../moc/counterfactuals", export_all = FALSE)

library("mlr")
library("mlrCPO")
library("ggplot2")


# Define UI 
ui <- fluidPage(
  
  # App title 
  titlePanel("xAutoml Test App"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for general inputs
    sidebarPanel(width = 2,
      selectInput("dataset", "Choose Dataset", folder),
      selectInput("lambda", "Exploration factor lambda", c(0.5, 1, 2)),
      selectInput("iteration", "Number of iteration", c(1:30)),
      selectInput("iteration.all", "Show all iterations (only PDP)", c("No", "Yes")),
      br(),
      br(),
      
      # Parameter description
      HTML(
        paste(
          h4("Parameter description"),'<br/>',
          "<b>eta:</b> learning rate - step size shrinkage used in update to prevent overfitting",'<br/>',
          "<b>gamma:</b> Minimum loss reduction required to make a further partition on a leaf node of the tree",'<br/>',
          "<b>max_depth:</b> Maximum depth of a tree",'<br/>',
          "<b>subsample:</b> Subsample ratio of the training instances",'<br/>',
          "<b>colsample_bytree:</b> the subsample ratio of columns when constructing each tree",'<br/>',
          "<b>colsample_bylevel:</b> the subsample ratio of columns for each level",'<br/>',
          "<b>lambda:</b> L2 regularization term on weights",'<br/>',
          "<b>alpha:</b> L1 regularization term on weights",'<br/>',
          "<b>nrounds:</b> maximum number of iterations",'<br/>'
          )
      )
     
      
    ),
    
    # Main panel for displaying outputs 
    mainPanel(width = 9,
      # define different tabs
      tabsetPanel(
        tabPanel("Descriptive", br(),
                 fluidRow(h3("Descriptive Overview")), 
                 br(),
                 fluidRow(column(6, plotOutput("corr_train")), column(6, plotOutput("corr_test"))),  
                 br(),
                 fluidRow(column(6, verbatimTextOutput("sum_train")), column(6, verbatimTextOutput("sum_test"))),
                 fluidRow(column(6, plotlyOutput("box_train")), column(6, plotlyOutput("box_test")))),
        tabPanel("1d Feature Effects", br(),
                 fluidRow(h3("1d Feature Effects")), 
                 br(),
                 fluidRow(column(4, uiOutput("params"))),
                 fluidRow(column(4, selectInput("ale.abs", "Show abs. values of ALE?", c("No", "Yes"))), column(4, selectInput("show.obs", "Show actual observations?", c("No", "Yes"))), column(4, uiOutput("center"))),
                 fluidRow(h4("Effect Plots on training data")),
                 fluidRow(column(4, plotlyOutput("ale")), column(4, plotlyOutput("pdp") %>% withSpinner(color = "#0dc5c1")), column(4, plotlyOutput("ice"))),
                 br(), br(),
                 fluidRow(h4("Effect Plots on test data")),
                 fluidRow(column(4, plotlyOutput("ale.test")), column(4, plotlyOutput("pdp.test")), column(4, plotlyOutput("ice.test")))),
        tabPanel("2d Feature Effects", br(),
                 fluidRow(h3("2d Feature Interactions / Effects")), 
                 br(),
                 fluidRow(column(4, uiOutput("params1")), column(4, uiOutput("params2"))),
                 fluidRow(column(6, plotlyOutput("hstat") %>% withSpinner(color = "#0dc5c1")), column(6, plotlyOutput("hstat2feat") %>% withSpinner(color = "#0dc5c1"))), 
                 br(),
                 fluidRow(column(4, selectInput("showPdp", "Show 2d ALE/PDP?", c("No","Yes")))),
                 fluidRow(column(6, plotlyOutput("ale2d") %>% withSpinner(color = "#0dc5c1")), column(6, plotlyOutput("pdp2d")))),
        tabPanel("Feature Importance", br(), 
                 fluidRow(h3("Permutation Feature Importance")), 
                 br(),
                 fluidRow(column(4, uiOutput("holdout"))),
                 fluidRow(column(9, plotlyOutput("featImp") %>% withSpinner(color = "#0dc5c1")))),
        tabPanel("Local Interpretations", br(),
                 fluidRow(h3("Local Interpretation")), 
                 br(),
                 fluidRow(column(4, uiOutput("obs1")), column(4, uiOutput("obs2"))),
                 br(),
                 fluidRow(column(6, h4("LIME"))), 
                 fluidRow(column(6, plotlyOutput("lime.best")), column(6, plotlyOutput("lime.worst"))),
                 br(),
                 fluidRow(column(6, h4("Shapley"))),
                 fluidRow(column(6, plotlyOutput("shap.best")), column(6, plotlyOutput("shap.worst"))),
                 br(),
                 fluidRow(column(6, h4("Counterfactuals"))),#Todo: adjust hardcoding
                 fluidRow(column(4, selectInput("obs.counter", "Choose observation", 1:200, 200), column(4, selectInput("improve.counter", "Minimum improvement of classif error", c("2.5" = 0.025,"5%" = 0.05,"7.5%" = 0.075,"10%" = 0.1), 0.05))),
                 fluidRow(column(10, tableOutput("counter.table")))),
                 fluidRow(column(6, plotlyOutput("counter.plot1")), column(6, plotOutput("counter.plot2"))))
      )
      
    )
  )
)