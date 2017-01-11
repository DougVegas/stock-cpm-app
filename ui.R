#loads Shiny package
library(shiny)
#loads library for change points
library(cpm)
#loads library for technical indicators
library(TTR)
#loads package for forecasts
library(forecast)
#load package for data tables
library(DT)
#loads library for different Shiny themes
library(shinythemes)






shinyUI (   
  
  fluidPage(theme = shinytheme("flatly"),
    
    
    #add padding to prevent Navbar overlaying body conent
    tags$style(type="text/css", 
              "body {padding-top: 75px;}
              #stock_symbol_header {color: white; background-color: #FB8E1E;   padding: 5px; border-radius: 5px;
              border: 2px solid white; text-align: left;}
              #date_range_header {color: white; background-color: #FB8E1E;   padding: 5px; border-radius: 5px;
              border: 2px solid white; text-align: left;}
              #cpm_type_header {color: white; background-color: #FB8E1E;   padding: 5px; border-radius: 5px;
              border: 2px solid white; text-align: left;}
              #startup_header {color: white; background-color: #FB8E1E;   padding: 5px; border-radius: 5px;
              border: 2px solid white; text-align: left;}
              #DataTables_Table_0_info {color: white;}
              #DataTables_Table_1_info {color: white;}
               "),
    
    navbarPage(title = "Stock Price Change Point Model", position = "fixed-top", collapsible = TRUE,
               
        tabPanel(title = "CPM", icon = icon("area-chart", lib = "font-awesome") ,
                 
                 fluidRow(
                      h4("What is a Change Point Model?"),
                      h6("A change point is an instance in time where the statistical properties before and after the point differ. A Change Point Analysis is an effective tool in creating an alert for stock price changes."),
                      h6("The Change Point Model is based on the ", code("cpm"), " package in R which implements various change point models for sequential change detection, using both parametric and nonparametric methods."),
                      h6("Learn more about each Change Point Model and the ", code("cpm"), " package ", a(" here.", target='_blank', href = "http://gordonjross.co.uk/cpm.pdf"))
                          
                          ),
                 
                 fluidRow(
                      column(width = 3,
                             ###set to Twitter by default
                             h6(id = "stock_symbol_header", "Type in Stock Symbol"),
                             br(),
                             textInput("stock_symbol", label = NULL, value = "TWTR")
                            ),
                      column(width = 3,
                             h6(id = "date_range_header", "Select Date Range"),
                             br(),
                             dateRangeInput("date_range", label = NULL,  start = Sys.Date() - 60, end = Sys.Date())
                             ),
                      column(width = 3,
                             h6(id = "cpm_type_header", "Select Change Point Model"),
                             br(),
                             selectInput("cpm_type", label = NULL, choices = c("Student", 
                                                                               "Bartlett",
                                                                               "GLR",
                                                                               "Exponential",
                                                                               "FET",
                                                                               "Mann-Whitney",
                                                                               "Kolmogorov-Smirnov"),
                                         selected = "Student")
                             ),
                      column(width = 3,
                             h6(id = "startup_header", "Enter Monitoring Period (in Trading Days, min = 20)"),
                             br(),
                             numericInput("startup", label = NULL, value = 20, min = 20)
                             )
                          ),
                 fluidRow(
                    inputPanel(
                          checkboxInput("technical_indicators", label = "Add Technical Indicators", value = FALSE),
                           numericInput("periods", label = "Enter Periods for Technicals", value = 14, min = 0)
                           )
                          ),
                 fluidRow(
                           plotOutput("stock_chart")
                          ),
                 fluidRow(
                          DT::dataTableOutput("change_point_data")
                          )
                 
                 ),
        
        tabPanel(title = "Data", icon = icon("table", lib = "font-awesome"), 
                 downloadButton('download_stock_data', label ="Download Stock Price Data"),
                 DT::dataTableOutput("stockdata")
                 ),
        
        tabPanel(title = "Contact Me", icon = icon("user", lib = "font-awesome"),
                 h1(strong("Douglas Pestana")),
                 br(),
                 icon("linkedin-square",class = "fa-5x", lib = "font-awesome"),
                 h3(a("LinkedIn Profile", target='_blank', href = "http://www.linkedin.com/in/douglaspestana")),
                 br(),
                 icon("google-plus",class = "fa-5x", lib = "font-awesome"),
                 h3(a("douglas.pestana@remix.institute", target='_blank', href = "douglas.pestana@remix.institute"))
                 #br(),
                 #icon("mobile-phone",class = "fa-5x", lib = "font-awesome"),
                 #h3(a("702-401-4645", target='_blank', href = "702-401-4645"))
                 )
    
      
      
      
              )
    
  
            )
  
)  