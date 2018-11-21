#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# general stuff needed outside of app -------------------------------------

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(htmltools)
library(lme4)
library(sjPlot)
#library(DT)

# add ward shape files
wards <- st_read("data/wards_20150327.shp")

wards_2 <- st_transform(wards, crs = 4326)

wards_2 %>% 
  mutate(
    lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~st_centroid(.x)[[2]])
  )->wards_3

content <- wards_3 %>%
  dplyr::select(Ward, lon, lat) %>% 
  mutate(Ward = as.character(Ward))
# bring in crime data
crime_dat <- read_rds("data/crime_data_for_app.RDS") %>% 
  mutate(day_of_week = lubridate::wday(incident_date, label = TRUE),
         month = lubridate::month(incident_date, label = TRUE))

all_winston <- crime_dat %>% 
  group_by(Ward) %>% 
  summarise(pctpoverty = mean(pctpov, na.rm = TRUE),
            pctwhite = mean(pctwhite, na.rm = TRUE)) %>% 
  filter(!is.na(Ward)) %>% 
  mutate_if(is.numeric, round, 0)

# start the server --------------------------------------------------------

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
     
  # WSPD Logo
  
  output$wspd_logo <- renderUI({
    tags$img(src= "http://www.cityofws.org/portals/0/images/marketing-and-communications/w-s-logos/W-S_logo_580.jpg",
             width="250", height="250")
  })
  
  output$census_logo <- renderUI({
    tags$img(src= "https://upload.wikimedia.org/wikipedia/commons/thumb/9/96/U.S._Census_Bureau_logo_post-2011.svg/800px-U.S._Census_Bureau_logo_post-2011.svg.png", 
             width="600", height="250")
  })
  
  # Get Reactive Data Based on Filters
  get_data <- reactive({
    get_data <- crime_dat %>% filter(Ward == input$ward, month == input$month)
  })
  
  crime_dat_reduced <- reactive({
    crime_dat_reduced <- crime_dat %>% 
      dplyr::select(case, call_type, address, Ward, indcident_date, incident_time)
  })
  
  # Descriptive Stats Tab
  # Total Top 10
  event_total <- function(){
    crime_dat %>% 
      group_by(call_type) %>%
      summarise(n = n()) %>% 
      top_n(10) %>% 
      ggplot(aes(reorder(call_type,n), n))+
      geom_col()+
      coord_flip()+
      labs(
        title = paste0("Top 10 Incidents for Winston-Salem NC in ", input$month),
        caption = "Data from Winston-Salem Police Department",
        x = NULL
      )+
      theme_minimal()+
      theme(plot.title = element_text(size =16))
  }
  
  output$event_total <- renderPlot({
    event_total()
  })
  # Graph 2
  map_by_month <- function(){
    get_data() %>% 
      group_by(call_type) %>% 
      summarise(n = n()) %>% 
      top_n(10) %>% 
      ggplot(aes(reorder(call_type,n), n))+
      geom_col()+
      coord_flip()+
      labs(
        title = paste0("Top 10 Incidents for the ", input$ward, " in ", input$month),
        caption = "Data from Winston-Salem Police Department",
        x= NULL
      )+
      theme_minimal()+
      theme(plot.title = element_text(size =16))
  }
  
  output$map_by_month <- renderPlot({
    map_by_month()
  })
  
  # Download Graphs
  output$download_descriptive <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"WSPD-Descriptive-Analysis.pdf")
    },
    content = function(file) {
      pdf(file)
      print( map_by_month() )
      print(event_total())
      print(demog_points())
      dev.off()
    }
  )
  
  
  # Demographics Table
  
  output$demog_table <- renderTable({
    if(input$demog){
      print(all_winston %>% 
              rename(`Pct Poverty` = pctpoverty,
                     `Pct White` = pctwhite))
    } else{
      NULL
    }
  }
  )
  
  demog_points <- function(){
    crime_dat %>% 
      ggplot(aes(pctpov, pctwhite))+
      geom_point()+
      theme_minimal()+
      labs(
        title = "Demographic Information",
        subtitle = "Comparison of Poverty and White Populations",
        caption = "Data from American Community Survey and \n Winston Salem Police Department"
      )
  }
  
  # Demographics LineChart
  
  output$demog_points <- renderPlot({
    if(input$demog){
      demog_points()
    } else{
      NULL
    }
  })
  
  # Make Reduced Data Set
  
  brushed_points <- function(){
    brushedPoints(crime_dat, input$plot_brush)
  }
  
  #Brushed Points
  output$brush_info <- DT::renderDataTable(
    brushed_points(), extensions = "Buttons",
    options = list(dom = "Bfrtip",pageLength = 50,
                   buttons = c("copy", "csv", "excel"))
    
  )

# dates and times ---------------------------------------------------------

  # Days and Time Tabs
  
  # Plot 1 function
  day_of_week_plot <- function(){
    crime_dat %>% 
      group_by(day_of_week) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(day_of_week, n))+
      geom_col()+
      labs(
        title = "Number of Incidents by Day of the Week",
        caption = "Data from Winston-Salem Police Department",
        x=NULL
      )+
      theme(plot.title = element_text(size =16))
  }
  # Make Plot
  output$day_of_week <- renderPlot(
    day_of_week_plot()
  )
  # Make Download
  output$download_day_of_week <- downloadHandler(
    filename = function() { paste("day_of_week", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = day_of_week_plot(), device = "png")
    }
  )
  
  # Make Day of Week Plot
  
  day_of_week_trend <- function(){
    crime_dat %>% 
      group_by(month, day_of_week) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(day_of_week, n, color = month, group = month))+
      geom_line()+
      theme_minimal()+
      labs(
        title = "Incidents over Time",
        color = "Month",
        caption = "Data from Winston-Salem Police Department",
        x = NULL
      )+
      theme(plot.title = element_text(size =16))
  }
  
  output$day_of_week_trend <- renderPlot(
    day_of_week_trend()
  )
  
  # Make Month Figures Plot
  
  month_fig <-function(){
    crime_dat %>% 
      group_by(month) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(month, n))+
      geom_col()+
      theme_minimal()+
      labs(
        title = "Number of Incidents by Month",
        caption = "Data from Winston-Salem Police Department",
        x = NULL
      )+
      theme(plot.title = element_text(size =16))
  }
  
  output$month_fig <- renderPlot(
    month_fig()
  )
  
  # Facet by Ward by Month Figures Plot
  
  ward_month_fig <-function(){
    crime_dat %>% 
      group_by(Ward, month) %>% 
      summarise(n = n()) %>% 
      ggplot(aes(month, n))+
      geom_line(aes(group=1))+
      facet_wrap(~Ward)+
      theme_minimal()+
      labs(
        title = "Number of Incidents by Month by Ward",
        caption = "Data from Winston-Salem Police Department",
        x = NULL
      )+
      theme(plot.title = element_text(size =16))
  }
  
  output$ward_month_fig <- renderPlot(
    ward_month_fig()
  )
  
  # Event By Time
  
  plot_time <- function(){
    crime_dat %>% 
      mutate(hour = lubridate::hour(incident_time),
             hour = factor(hour)) %>% 
      count(incident_date, hour) %>% 
      ggplot(aes(hour, n))+
      geom_boxplot()+
      geom_jitter(alpha = .1)+
      theme_minimal()+
      labs(
        title = "Distribution of Incidents by Hour of Day",
        subtitle = "Grouped by Day and Hour",
        caption = "Data from Winston- Salem Police Department",
        y = "Count of Incidents",
        x = "Hour of The Day"
      )
  }
  
  # Plot Time
  output$plot_time<- renderPlot({
    plot_time()
  })
  
  
  # Date Zoom
  day_data <- reactive({
    crime_dat %>% 
      mutate(day_num = lubridate::day(incident_date)) %>% 
      filter(month == input$time_month,
             day_num == input$date_slide)
  })
  
  day_plot <- function(){
    day_data() %>% 
      group_by(call_type) %>% 
      summarise( n = n()) %>% 
      top_n(10) %>% 
      ggplot(aes(reorder(call_type,n), n))+
      geom_col()+
      labs(
        x = NULL,
        title = paste0("Top 10 Incident Types in Winston-Salem  on ",input$time_month, " ",input$date_slide),
        caption = "Data from the Winston-Salem Police Department"
      )+
      theme_minimal()+
      coord_flip()
  }
  
  output$day_plot <- renderPlot({
    day_plot()
  })
  
  observe({
    dat <- crime_dat %>% 
      mutate(day_num = lubridate::day(incident_date)) %>% 
      filter(month == input$time_month)
    max_day <- max(dat$day_num)
    updateSliderInput(session, "date_slide", max = max_day)
  })
  
  # Download All for the tab
  output$download_all_date_time <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),"WSPD-Date-Time-Analysis.pdf")
    },
    content = function(file) {
      pdf(file)
      print( day_of_week_plot() )
      print( day_of_week_trend() )  
      print( month_fig() )
      print(ward_month_fig())
      print(plot_time())
      print(day_plot())
      dev.off()
    }
  )
  # Download Data for this tab
  output$download_date_time_csv <- downloadHandler(
    filename = "crime_by_ward_date_time.csv",
    content = function(file) {
      write.csv(crime_dat_reduced(), file)
    },
    contentType = "text/csv"
  )

# leaflet geo mapping -----------------------------------------------------

  # Leaflet Mapping for Geolocator
  events_in_bounds <- reactive({
    if (is.null(input$map_bounds))
      return(crime_dat[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(crime_dat,
           lat >= latRng[1] & lat <= latRng[2] &
             lon >= lngRng[1] & lon <= lngRng[2]) %>% 
      dplyr::select(case:incident_time)
  })
    
    output$map <- renderLeaflet({
      leaflet(wards_3) %>% 
        addTiles() %>% 
        addPolygons(opacity = .5, fillOpacity = .1) %>% 
        addMarkers(lng = ~lon,lat = ~lat)
    })
    
    observe({
      if(input$points){
        leafletProxy("map", data = crime_dat %>% filter(month == input$map_month)) %>% 
          leaflet::clearGroup(group = "crimedots") %>% 
          addCircles(lng = ~lon,lat = ~lat, group = "crimedots")
      } else{
        leafletProxy("map", data = wards_3) %>% 
          clearGroup("crimedots")
      }
      
    })
    
    output$table <- DT::renderDataTable(
      events_in_bounds(), extensions = "Buttons",
      options = list(dom = "Bfrtip",pageLength = 50,
                     buttons = c("copy", "csv", "excel", "pdf", "print")))
    
    output$download_csv <- downloadHandler(
      filename = "crime_by_ward.csv",
      content = function(file) {
        write.csv(events_in_bounds(), file)
      },
      contentType = "text/csv"
    )
    

# predictive modeling -----------------------------------------------------

    # Set Up Equation
    output$equation <- renderUI({
      main_effects <- input$predictors
      p(
        withMathJax(
      if(!input$addgroupeffects){
        if(length(main_effects) == 2){
          sprintf("$$inv log(Y_{%s}) = \\beta_{%s} + \\beta_{%s} + \\alpha$$",
                  input$call, main_effects[[1]], main_effects[[2]])
        } else if (length(main_effects) == 3){
          sprintf("$$inv log(Y_{%s}) = \\beta_{%s} + \\beta_{%s} +  \\beta_{%s}+ \\alpha$$",
                  input$call, main_effects[[1]], main_effects[[2]], main_effects[[3]])
        } else{
          sprintf("$$inv log(Y_{%s}) = \\beta_{%s} + \\alpha$$",input$call,
                  main_effects[[1]])
        }
                      
      } else {
        if(length(input$groups)==2){
          if(length(input$predictors) == 3){
            sprintf("$$inv log(Y_{%s}) = \\beta_{%s} + \\beta_{%s} + \\beta_{%s} + \\delta_{%s}+\\gamma_{%s}+\\alpha$$",
                    input$call, main_effects[[1]], main_effects[[2]], main_effects[[3]],
                    input$groups[[1]], input$groups[[2]])
          } else if (length(input$predictors) == 2){
            sprintf("$$inv log(Y_{%s}) = \\beta_{%s} + \\beta_{%s} + \\delta_{%s}+\\gamma_{%s}+\\alpha$$",
                    input$call, main_effects[[1]], main_effects[[2]],
                    input$groups[[1]], input$groups[[2]])
          } else {
            sprintf("$$inv log(Y_{%s}) = \\beta_{%s} + \\delta_{%s} + \\gamma_{%s} + \\alpha$$",
                    input$call, main_effects[[1]], 
                    input$groups[[1]], input$groups[[2]])
          }} else{
            if(length(input$predictors) == 3){
              sprintf("$$inv log(Y_{%s}) = \\beta_{%s} + \\beta_{%s} + \\beta_{%s} + \\delta_{%s}+\\alpha$$",
                      input$call, main_effects[[1]], main_effects[[2]], 
                      main_effects[[3]],input$groups[[1]])
            } else if (length(input$predictors) == 2){
              sprintf("$$inv log(Y_{%s}) = \\beta_{%s} + \\beta_{%s} + \\delta_{%s}+\\alpha$$",
                      input$call, main_effects[[1]], main_effects[[2]],input$groups[[1]])
            } else {
              sprintf("$$inv log(Y_{%s}) = \\beta_{%s}  + \\delta_{%s}+\\alpha$$",
                      input$call, main_effects[[1]], input$groups[[1]])
            }
        }
      }
        ))
    }
    )
    
    # Create the Desired Dataframe for Modeling
    model_data <- reactive({
      crime_dat %>% 
        mutate(hour = lubridate::hour(incident_time)) %>% 
        dplyr::select(input$predictors, input$groups, call_type) %>% 
        mutate(y = ifelse(call_type == input$call,1,0))
    })
    
    fit <- reactive({
      if(!input$addgroupeffects){
        fit <- glm(paste0("y~", paste0(input$predictors, collapse = "+")),
                   data = model_data(), family = "binomial")
      } else if (length(input$groups)==1){
        fit <-lmer(paste0("y~", paste0(input$predictors, collapse = "+"),
                          paste0("+ (1|",input$groups,")")),
                   data = model_data(), family = "binomial")
      } else if (length(input$groups)==2){
        fit <-lmer(paste0("y~", paste0(input$predictors, collapse = "+"),
                          paste0("+ (1|",input$groups[[1]],")","+ (1|",input$groups[[2]],")")),
                   data = model_data(), family = "binomial")
      } else{
        NULL
      }
      
    })
    
    # Output model fit
    output$model_fit <- renderPrint({
      summary(fit())
    })
    # Output model fit
    output$model_graph <- renderPlot({
      sjPlot::plot_model(fit(), show.values = TRUE, value.offset = .3, 
                         title ="Parameter Estimates")+
        theme(plot.title = element_text(size =16))
    })
    
    # Output the dataframe for modeling for inspection
    output$data_to_model <- renderDataTable({
      model_data()
    }
    )
    
})
