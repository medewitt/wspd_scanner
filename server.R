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
#Top 10 Calls

crime_dat %>% 
  group_by(call_type) %>% 
  summarise(n = n()) %>% 
  top_n(10) %>% 
  pull(call_type)->top_10

# By block data

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
      ggplot(aes(day_of_week, n))+
      geom_boxplot()+
      facet_wrap(~month)+
      theme_minimal()+
      labs(
        title = "Top Incident Days of the Week",
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
      arm::display(fit())
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
    
    #poisson regression---
    # Create the data to be used for modeling
    poi_dat <- reactive({
      my_group <- sym(input$poi_group)
      crime_dat %>% 
        group_by(!!my_group) %>% 
        summarise(calls = n(),
                  pctpov = median(pctpov),
                  pctwhite = median(pctwhite),
                  population = median(population))
    })
    
    poi_fit <- reactive({
      
      fit <- glm(paste0("calls~", paste0(input$poi_preds, collapse = "+"),
                        "+offset(log(population))"),
                 data = poi_dat(), 
                 family = "poisson"
                 )
      fit
    })
    
    # Output Model Fit
    output$poi_fit <- renderPrint(
      summary(poi_fit())
    )
    
    # Predictions vs Actuals
    make_poi_graph <- function(){
      dat <- poi_dat()
      my_group <- sym(input$poi_group)
      dat$prediction <- round(predict(poi_fit(), 
                                      newdata = dat, 
                                      type = "response"),0)
      plot_min <- min(dat$prediction, dat$calls)
      plot_max <- max(dat$prediction, dat$calls)
      dat %>% 
        ggplot(aes(calls, prediction, color = !!my_group))+
        geom_point(aes(size = population))+
        geom_abline(slope = 1, lty = "dashed", color = "orange")+
        theme_minimal()+
        theme(legend.position = "none")+
        labs(
          title = "Actual Calls vs Predicted\n(Bubble Proportional to Population)",
          #subtitle = "Bubble Size Proportional to Population",
          x = "Actual Number of Calls",
          y = "Predicted Number of Calls",
          size = "Population",
          caption = "Data: WSPD + US Census"
        )+
        scale_x_continuous(limits = c(plot_min, plot_max))+
        scale_y_continuous(limits = c(plot_min, plot_max))
    }
    
    # Generate Graph for Output
    
    output$poi_plot <- renderPlotly(
      plotly::ggplotly(make_poi_graph())
    )
# eda ---------------------------------------------------------------------
    # Handle non-integer and users putting in the same values
    observe({
      # Correct for integers
      pca_1_slider <- round(input$pca1,0)
      pca_2_slider <- round(input$pca2,0)
      # Make sure that pca1 != pca2
      if(pca_1_slider==pca_2_slider){
        if(pca_2_slider+1 < 13){
          pca_2_slider <- pca_2_slider+1
        } else{
          pca_2_slider <- 1
        }
      }
      #Update the UI with the new corrected values
      updateSliderInput(session, inputId = "pca1", value = pca_1_slider)
      updateSliderInput(session, inputId = "pca2", value = pca_2_slider)
    })
    
    # Generate the PCA
    # Top Ten Calls by Block
    pca_data <- reactive({
      
      grouping_var <- sym(input$pca_group)
      crime_dat %>% 
        filter(call_type %in% top_10) %>% 
        group_by(!!grouping_var, call_type) %>% 
        summarise(n = n(),
                  population = mean(population)) %>%
        mutate(perc_capita = n/population) %>% 
        select(-n, -population) %>% 
        spread(call_type, perc_capita, fill = 0) %>% 
        setNames(gsub(" ", "_", names(.)))-> top_10_calls
      
      by_block <- crime_dat %>%
        mutate(week_day_event = ifelse(day_of_week %in% 
                                         c("Sun", "Fri", "Sat"),"we","wd")) %>% 
        group_by(!!grouping_var, week_day_event) %>% 
        summarise(events = n()) %>% 
        mutate(perc = events/sum(events)*100) %>% 
        select(-events) %>% 
        spread(week_day_event, perc, fill = 0) %>% 
        # Brink in Crime Data for Summaries
        left_join(crime_dat) %>% 
        group_by(!!grouping_var) %>% 
        summarise(pctpov = median(pctpov, na.rm = T),
                  pctwhite = median(pctwhite, na.rm = T),
                  we = mean(we, na.rm = T),
                  #wd = mean(wd, na.rm = T)
        ) %>% 
        left_join(top_10_calls) %>% 
        mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
    })
    
    pca_fit <- reactive({
      grouping_var <- sym(input$pca_group)
      x <- select(pca_data(), -!!grouping_var) %>% 
        as.matrix()
      # Assign Row Names
      rownames(x) <- pca_data() %>% 
        pull(!!grouping_var)
      
      pca_fit <- prcomp(x, scale. = TRUE)
      
      pca_fit
    })
    
    
    # Make Cum Variance Graph Function
    make_cum_explained <- function(){
      a<-summary(pca_fit())[["importance"]] %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "parameter") %>% 
        gather(component, value, -parameter) %>% 
        filter(grepl("Cum", parameter ))
      a %>% 
        mutate(component = factor(component),
               component = fct_inorder(component)) %>% 
        ggplot(aes(component, value, group = 1))+
        geom_step(size = 2)+
        scale_y_continuous(labels = scales::percent, limits = c(0,1))+
        labs(
          title = "Cummulative Variance Explained",
          y = "Variance Explained",
          x= "Component"
        )+
        theme_minimal()+
        geom_hline(yintercept = .8, lty = "dashed", color = "orange")
    }
    
    
    # Now make a biplot
    my_biplot_values <- reactive({
      c(input$pca1, input$pca2)
    })
    
    # Function to Generate Graphs
    make_biplot <- function(){
      par(mar = c(5.1, 4.1, 0, 1))
      biplot(pca_fit(),  choices = my_biplot_values(),
             main = "Biplot of PCA")
    }
    #Output graph
    output$biplot <- renderPlot(
      if(is.null(input$pca1)|is.null(input$pca2)){
        NULL
      } else{
        make_biplot()
      }
      
    )
    output$pca_cum <- renderPlot(
      make_cum_explained()
    )
     # KNN ----
    observe({
      dat <- pca_data() %>% 
        select(-!!sym(input$pca_group))
      
      available_option <- names(dat)
      if(!is.na(dim(dat)[[1]])){
        updateSelectInput(session, "xcol", choices = available_option)
        updateSelectInput(session, "ycol",
                          choices = available_option, 
                          selected = available_option[[2]])
      }
    })
    # Combine the selected variables into a new data frame
     selectedData <- reactive({
       if(!is.null(input$xcol)|!is.null(input$ycol)){
         pca_data() %>% 
           select(!!sym(input$xcol), !!sym(input$ycol))
       } else{
         NULL
       }
       
     })
    # Make Clusters
    clusters <- reactive({
      kmeans(selectedData(), input$clusters)
    })
    
    # Function to make cluster graph
    make_knn_graph <- function(){
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    }
    
    # Generate Output dataset
    output$knn_plot <- renderPlot({
      make_knn_graph()
    })
    # Generate Output Data Table
    
    output$knn_table <- renderDataTable({
      dat <- pca_data() %>% 
        pull(!!sym(input$pca_group))
      
      data_frame(Group = dat, Cluster = clusters()$cluster)
    })
})
