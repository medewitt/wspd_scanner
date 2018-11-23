#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) 
library(shinydashboard)
library(leaflet)
library(tidyverse)
#library(shinyjs)
library(DT)
library(arm)
library(plotly)

crime_dat <- read_rds("data/crime_data_for_app.RDS") %>% 
  mutate(day_of_week = lubridate::wday(incident_date, label = TRUE),
         month = lubridate::month(incident_date, label = TRUE))

some_link <-"google.com"

# Define UI for application that draws a histogram
shinydashboard::dashboardPage(skin = "yellow",
                              
  #add title
  dashboardHeader(title = "Winston-Salem Crime Tracker", titleWidth = 800),
  
  #Add sidebar elements
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("archive")),
    menuItem("Data Dictionary", tabName = "dict", icon = icon("bars")),
    menuItem(
      "Descriptive Stats",
      tabName =  "descr",
      icon = icon("user")
    ),
    menuItem(
      "Days and Time",
      tabName = "daytime",
      icon = icon("calendar")
    ),
    menuItem("Geolocator", tabName = "geolocate", icon = icon("globe")),
    menuItem("Prediction Probabilities", 
             tabName = "preds", icon = icon("paper-plane")),
    menuItem("Predicting Calls", 
             tabName = "pred_poi", icon = icon("paper-plane")),
    menuItem("Exploratory", tabName = "eda", icon = icon("search"))
  )),
  # Add Body Elements
  
  dashboardBody(
    tabItems(
      # First Tab
      tabItem(tabName = "about",
              fluidRow(
                withMathJax(),
                h1("What this Application Does"),
                "The purpose of this applications is to complete some analysis on 
                   different events in which the police depart of Winston Salem is Involved.
                   This analysis includes using data from the police department itself to 
                   make summary statistics, review time and date of incidents, as well as
                   position these events within the citizens and the wards themselves.",
                h2("Method"),
                "Data were scraped for each day since 2018-01-01 from the Winston-Salem
                Police Department's incident tracker. The addresses were then geocoded with
                using the batch geocoder API provided by the US Census Bureau. Any address that could not
                be geolocated was removed from the data set. This typically included incidents 
                that occured at the intersection of two roads. These data
                were then geolocated to the Ward of interest within the City of Winston Salem.
                The data were then matched to demographic data available from the American Community Survey.",
                h2("Data Sources"),
                a("Winston-Salem Police Department Incident Tracker", 
                  target = "_blank", 
                  href = paste0("http://www.cityofws.org/crimestats/txt/WSPD0101.txt")),
                br(),
                a("Census Bureau Geocoder", 
                     target = "_blank", 
                     href = paste0("https://www.census.gov/geo/maps-data/data/geocoder.html")
                     ),
                br(),
                a("Census Bureau American Community Survey", 
                  target = "_blank", 
                  href = paste0("https://www.census.gov/programs-surveys/acs/")
                ),
                br(),
                a("Winston-Salem Ward Definitions", 
                     target = "_blank", 
                     href = paste0("http://www.cityofws.org/departments/planning/gis/gis-data-sets-for-download")
                ),
                h2("Thanks!"),
                box(uiOutput("wspd_logo"), width = 4),
                box(uiOutput("census_logo"), width = 8)
              )),
      

# data dictionary ---------------------------------------------------------

      tabItem(tabName = "dict",
              fluidRow(
              includeMarkdown("data_dictionary.md")
              )
      ),
      # Descriptive Statistics ------------------------------------
      tabItem(tabName ="descr",
              fluidRow(h2("This section investigates the different types of incidents that occur by Ward."), 
                       h3("These can then be compared to the overall Winston-Salem area."),
                       box(selectizeInput("month", h3("Month"), 
                                          selected = "", choices = levels(as.factor(crime_dat$month))),
                           width = 6),
                       box(selectizeInput("ward", h3("Ward"), 
                                      selected = "Northwest", choices = levels(as.factor(crime_dat$Ward))),
                           width = 6),
                       box(plotOutput("event_total"), width = 6),
                       box(plotOutput("map_by_month"), width = 6),
                       box(checkboxInput("demog", h4("See Demographics?")),width = 12),
                       conditionalPanel("input.demog",
                                        box(
                                          h4("Summary by Ward"),
                                          tableOutput("demog_table"), width = 4),
                                        box(h4("Select the Point Ranges for More Details"),
                                            plotOutput("demog_points", click = "plot_point",
                                                       brush = brushOpts(id = "plot_brush")), width = 8),
                                        box(h4("Selected Values"),
                                            DT::dataTableOutput("brush_info"), width = 12)
                       ),
                       
                       box(downloadButton("download_descriptive", label = "Download All?"), width = 6)
                       )),
      
      
      # Analysis of When Things Happen
      tabItem(tabName = "daytime",
              fluidRow(h1("Analysis By Dates and Times"),
                       box(plotOutput("day_of_week"), width = 4),
                       box(plotOutput("day_of_week_trend"), width = 8),
                       box(plotOutput("month_fig"), width = 12),
                       box(plotOutput("ward_month_fig"), width = 12),
                       box(plotOutput("plot_time"), width = 12),
                       box(h4("Month Zoom"), width = 12),
                       box(selectizeInput("time_month", h3("Month"), 
                                              selected = "Jan", 
                                          choices = levels(as.factor(crime_dat$month))),
                               width = 6),
                       box(sliderInput("date_slide", label = "Select Day",
                                       value = 1,
                                       min = 1,
                                       max = 31,
                                       step = 1), width =6),
                       box(plotOutput("day_plot"), width = 12),
                       br(),
                       box(h4("Want to save the plots or the data?"), width = 12),
                       box(downloadButton("download_all_date_time", label = "Plots"), width = 6),
                       box(downloadButton("download_date_time_csv", label = "Data")), width = 6)),
      # Analysis of Locations
      tabItem(tabName = "geolocate",
              fluidRow(
                h1("Winston-Salem Ward Map"),
                box(checkboxInput("points", h4("Add Incidents to Map", style = "color:red;")),
                    width = 6),
                conditionalPanel("input.points",
                box(selectizeInput("map_month", h3("Month"), 
                                   selected = "", choices = levels(as.factor(crime_dat$month))),
                    width = 6)),
                leafletOutput("map"),
                h3("Below Find the Events That Have Occured in The Map Window"),
                DT::dataTableOutput("table")#,
                #downloadButton("downloadCsv", "Download as CSV")
              )
      ),
      
      # Predictive Modeling ----
      tabItem(tabName = "preds",
              fluidRow(
                withMathJax(),
                h1("Preditive Modeling"),
                box(withMathJax("This section requires some modeling. This section allows you to predict the likelhood of a given case type occuring in Ward. 
                                The data on the demographics have been geocoded based on data from the American Community Survey.",
                                em("Caution in that there is definitely colinearity and spatial autocorrelation in the predictors and the Wards.")),
                    width = 12),
                br(),
                h2("Prediction of Probability of a Given Call Type"),
                box(h4("Population Level Effects/ Predictors"),
                tags$ul(
                  tags$li("Percent White - The Percentage of Citizens Who are White"), 
                  tags$li("Percent Poverty - The Percentage of Citizens Who are in Poverty")
                ), width =6),
                box(h4("Optional Group Effects"),
                tags$ul(
                  tags$li("Ward - The Ward in which the incident occured"), 
                  tags$li("Tract - The census tract in which the incident occured")
                ), width = 6),
                box(selectizeInput("call", "Call Type", 
                               selected = "ALARMS", choices = levels(as.factor(crime_dat$call_type))),
                    width = 3),
                box(selectizeInput("predictors", "Predictors", selected = "pctpov",
                                   choices = c("Pct Poverty" = "pctpov", 
                                               "Pct White"= "pctwhite",
                                               "hour" = "hour"),
                                   multiple = TRUE), width = 3),
                box(checkboxInput("addgroupeffects", "Add Group Effects?"), width = 3),
                box(conditionalPanel("input.addgroupeffects",
                                 selectizeInput("groups", "Group Effects",
                                                selected = "Ward",
                                                choices = c("Ward" = "Ward",
                                                            "Tract" = "tract"),
                                                multiple = TRUE)), width = 3),
                box(h3("Model Outputs"), width = 12),
                tags$head(tags$style(HTML("
                                #equation {
                                  text-align: center;
                                }
                                div.box-header {
                                  text-align: center;
                                }
                                "))),
                box(uiOutput("equation"), width = 12),
                box(verbatimTextOutput("model_fit"), width =6),
                box(plotOutput("model_graph"), width =6),
                box(h3("Model Data"), width =12),
                box(DT::dataTableOutput("data_to_model"), width = 12)
              )
      ),
# Poisson Regression ----
     tabItem(tabName = "pred_poi",
             fluidRow(
               h2("Predicting Calls"),
               p("This section attempts to predict the number of calls
                by grouping variable in order to understand the difference
                between the actual number of calls and the actual observed
                number of calls. This analysis uses a Poisson distribution
                to model the number of calls in a given area. The population
                 is used to offset the Poisson regression."),
               br(),
               box(selectizeInput("poi_preds", "Predictors", 
                                  selected = "pctpov",
                                  choices = c("Pct Poverty" = "pctpov", 
                                              "Pct White"= "pctwhite"
                                              ),
                                  multiple = TRUE), width = 6),
               box(selectizeInput("poi_group", "Grouping Variable",
                                  choices = c("Ward" = "Ward",
                                              "Tract" = "tract",
                                              "Block" = "block"),
                                  selected = "Ward"), width = 6),
               box(verbatimTextOutput("poi_fit"), width =6),
               box(plotlyOutput("poi_plot"), width = 6),
               box(downloadButton("poi_plot_pdf"), width = 3)
             )),
# exploratory analysis ----------------------------------------------------
        tabItem(tabName = "eda",
                fluidRow(
                  h1("Exploratory Modeling"),
                  br(),
                  h2("Principal Components Analysis"),
                  "This section utilises Principal Components Analysis (PCA) in 
                  order to perform some exploratory data analysis. 
                  PCA Can be used to establish a narrative around the different variables. 
                  Here summary statistics
                  have been computed for each census block within Winston-Salem. 
                  These data include
                  the per-capita top 10 calls, as well as demographics and the number of weekend events.
                  These metrics are are scaled before they are supplied to the PCA analysis.",
                  br(),
                  br(),
                  box(selectizeInput("pca_group", "Select PCA Group",
                                     selected = "Ward",
                                     choices = c("Ward" = "Ward",
                                                 "Tract" = "tract",
                                                 "Block" = "block")), width = 4),
                  box(numericInput("pca1", "First Component for Biplot", 
                                   min = 1, 
                                   max = 13, 
                                   value = 1, step = 1), width = 4),
                  box(numericInput("pca2", "Second Component for Biplot", 
                                   min = 1, 
                                   max = 13, 
                                   value = 2, step = 1), width = 4),
                  box(plotOutput("biplot"), width = 6),
                  box(plotOutput("pca_cum"), width = 6),
                  h2("Cluster Analysis"),
                  br(),
                  "The purpose of k-means clustering is to
                  use Euclidean distances and a pre-specified number of cluster
                  centers to group data points. This can be used as a means of 
                  unsupervised learning.",
                  br(),
                  box(selectInput('xcol', 'X Variable', NULL), width = 3),
                  box(selectInput('ycol', 'Y Variable', NULL),width = 3),
                  box(numericInput('clusters', 'Cluster count', 3,
                               min = 1, max = 9), width = 3),
                  box(selectizeInput("kmeans_algo", "Algorithm", 
                                     choices = c("Hartigan-Wong", 
                                                 "Lloyd", "Forgy","MacQueen"),
                                     selected = "Hartigan-Wong"), width=3),
                  box(plotOutput("kmeans_plot"), width = 6),
                  box(dataTableOutput("kmeans_table"), width = 6),
                  box(downloadButton("eda_plots", label = "Data"), width = 12)
                ))

    )
  )
)
