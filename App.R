########################################################
# R Shiny App -- Sammy Rogaczewski -- Decemeber 5 2024#
#######################################################


library(dataRetrieval)
library(jsonlite)
library(httr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(dplyr)
library(clock)
library(curl)
###############################
#Shiny Web App for Stream flow
##############################


##Packages
library(shinythemes)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(bslib)
library(plotly)
library(zoo)
library(packrat)
library(rsconnect)
library(dplyr)
library(leaflet)


#location <-  ("C:/Users/srogacz1/Dropbox/RShiny Apps/Pass Creek") -- good to go
#filenames <- c("") --need to add data files for each gauge

#setwd("C:/Users/srogacz1/Dropbox/RShiny Apps/Pass Creek/R Web App")


#####################
##Pass Creek

#Read in CSV files

PCSEO <- read.csv("PassCreekSEO.csv")
PCPC1 <- read.csv("PassCreekPC1.csv")
PCPC4 <- read.csv("PassCreekPC4.csv")


#Making new column for Site name -- hopefully to be displayed later in output table

PCSEO$Site <- "Pass Creek: SEO" #if do for loop, this will be site_names(i)?
PCPC1$Site <- "Pass Creek: PC 1"
PCPC4$Site <- "Pass Creek: PC 4"

#Making new column for Gauge label -- used for Map labels later on

PCSEO$Gauge <- "SEO"
PCPC1$Gauge <- "PC1"
PCPC4$Gauge <- "PC4"

#Adding Latitude and Longitude values --> need to actually add  CORRECT values

PCSEO$Latitude <- 41.586389
PCSEO$Longitude <- -106.611667

PCPC1$Latitude <- 41.677081
PCPC1$Longitude <- -106.724230

PCPC4$Latitude <- 41.603240
PCPC4$Longitude <- -106.734616

#Changing column name "Value" to "Streamflow (cfs)"

colnames(PCSEO)[colnames(PCSEO) == "Value"] <- "Streamflow (cfs)"
colnames(PCPC1)[colnames(PCPC1) == "value"] <- "Depth"
colnames(PCPC4)[colnames(PCPC4) == "value"] <- "Depth"
colnames(PCPC1)[colnames(PCPC1) == "timestamp"] <- "TimeStamp"
colnames(PCPC4)[colnames(PCPC4) == "timestamp"] <- "TimeStamp"
colnames(PCPC1)[colnames(PCPC1) == "Flow..cfs."] <- "Streamflow (cfs)"
colnames(PCPC4)[colnames(PCPC4) == "Flow..cfs."] <- "Streamflow (cfs)"

#Changing decimal places on Streamflow (cfs)
PCSEO$`Streamflow (cfs)` <- round(PCSEO$`Streamflow (cfs)`, 1)
PCPC1$`Streamflow (cfs)` <- round(PCPC1$`Streamflow (cfs)`, 1)
PCPC4$`Streamflow (cfs)` <- round(PCPC4$`Streamflow (cfs)`, 1)

#Making sure midnight times are appended

PCSEO$TimeStamp <- ifelse(
  nchar(PCSEO$TimeStamp) == 10,  # Detect dates without a time
  paste0(PCSEO$TimeStamp, " 00:00:00"),  # Append '00:00:00'
  PCSEO$TimeStamp
)

PCPC1$TimeStamp <- ifelse(
  nchar(PCPC1$TimeStamp) == 10,  # Detect dates without a time
  paste0(PCPC1$TimeStamp, " 00:00:00"),  # Append '00:00:00'
  PCPC1$TimeStamp
)

PCPC4$TimeStamp <- ifelse(
  nchar(PCPC4$TimeStamp) == 10,  # Detect dates without a time
  paste0(PCPC4$TimeStamp, " 00:00:00"),  # Append '00:00:00'
  PCPC4$TimeStamp
)

#Making sure it is in proper Date 

PCSEO$TimeStamp <- as.POSIXct(PCSEO$TimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
PCPC1$TimeStamp <- as.POSIXct(PCPC1$TimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
PCPC4$TimeStamp <- as.POSIXct(PCPC4$TimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


# Get the current date

current_date <- as.POSIXct(Sys.time()-60*60*6,tz="UTC") #USE for ALL Gauges
#current_date <- as.POSIXct("2024-10-01 3:00:00", tz = "America/Denver") #Just to test out table and map with different dates and times

#####################
#R Shiny Portion

#################
#UI

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel(HTML("<h2> Pass Creek Streamflow Data</h2>")),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Select Gauge Site on Map"),
      leafletOutput("main_map", height = "400px")
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(id = "tabs",
                  
                  tabPanel("SEO Gauge", value = "SEO",
                           tableOutput(outputId = "table_SEO"),
                           tags$br(), tags$br(),
                           
                           selectInput(inputId = "datetime", label = "Select Date Range:", 
                                       choices = list("Current Date and Time" = "current", "Last 7 Days" = "last_7", "Last 14 Days" = "last_14", 
                                                      "Last Month" = "last_month", "Last 6 Months" = "last_6month", "Last Year" = "last_year", "Custom" = "custom"), selected = "current"),
                           #bottom panel only if custom is selected
                           conditionalPanel(
                             condition = "input.datetime == 'custom'",
                             dateRangeInput(inputId = "custom_range", label = "Select Date Range",start = current_date - 365, end = current_date)
                           ),
                           
                           tags$br(), tags$br(),
                           plotOutput(outputId = "plot_SEO", hover = TRUE)),
                  #tags$br(), tags$br(),
                  #leafletOutput(outputId = "map_SEO")),
                  
                  tabPanel("PC #1 Gauge", value = "PC1",
                           tableOutput(outputId = "table_PC1"),
                           tags$br(), tags$br(),
                           
                           selectInput(inputId = "datetime", label = "Select Date Range:", 
                                       choices = list("Current Date and Time" = "current", "Last 7 Days" = "last_7", "Last 14 Days" = "last_14", 
                                                      "Last Month" = "last_month", "Last 6 Months" = "last_6month", "Last Year" = "last_year", "Custom" = "custom"), selected = "current"),
                           #bottom panel only if custom is selected
                           conditionalPanel(
                             condition = "input.datetime == 'custom'",
                             dateRangeInput(inputId = "custom_range", label = "Select Date Range",start = current_date - 365, end = current_date)
                           ),
                           
                           tags$br(), tags$br(),
                           plotOutput(outputId = "plot_PC1", hover = TRUE)),
                  #tags$br(), tags$br(),
                  #leafletOutput(outputId = "map_PC1")),
                  
                  tabPanel("PC #4 Gauge", value = "PC4",
                           tableOutput(outputId = "table_PC4"),
                           tags$br(), tags$br(),
                           
                           selectInput(inputId = "datetime", label = "Select Date Range:", 
                                       choices = list("Current Date and Time" = "current", "Last 7 Days" = "last_7", "Last 14 Days" = "last_14", 
                                                      "Last Month" = "last_month", "Last 6 Months" = "last_6month", "Last Year" = "last_year", "Custom" = "custom"), selected = "current"),
                           #bottom panel only if custom is selected
                           conditionalPanel(
                             condition = "input.datetime == 'custom'",
                             dateRangeInput(inputId = "custom_range", label = "Select Date Range",start = current_date - 365, end = current_date)
                           ),
                           
                           tags$br(), tags$br(),
                           plotOutput(outputId = "plot_PC4", hover = TRUE))
                  #tags$br(), tags$br(),
                  #eafletOutput(outputId = "map_PC4"))
      ))))


#####################
# SERVER

server <- function(input, output, session){
  
  PCSEO$TimeStamp <- as.POSIXct(PCSEO$TimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  PCPC1$TimeStamp <- as.POSIXct(PCPC1$TimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  PCPC4$TimeStamp <- as.POSIXct(PCPC4$TimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  reactive_date <- reactive({
    if (input$datetime == "current"){
      return(c(current_date - 3600*24*4, current_date)) #last 4 days
    } else if (input$datetime == "last_7"){
      return(c(current_date - 3600*24*7, current_date)) #last 7 days
    } else if (input$datetime == "last_14"){ 
      return(c(current_date - 3600*24*14, current_date)) #last 14 days
    } else if (input$datetime == "last_month"){
      return(c(current_date - 3600*24*30, current_date)) #last month
    } else if (input$datetime == "last_6month"){
      return(c(current_date - 3600*24*182, current_date))
    } else if (input$datetime == "last_year"){
      return(c(current_date - 3600*24*365, current_date))
    } else if (input$datetime == "custom"){
      return(as.POSIXct(input$custom_range))
    } 
  })
  
  ###Info for Map 
  
  #Need to combine the 3 datasets for Map later on
  
  map_data <- bind_rows(PCSEO, PCPC1, PCPC4)
  
  #Create new variable for map
  map_data$Last_Reading <- sapply(map_data$TimeStamp, function(ts) {
    as.numeric(difftime(current_date, ts, units = "hours"))})
  
  #Create color specifics for map markers
  map_data$Color <- case_when(
    map_data$Last_Reading < 2 ~ "green",
    between(map_data$Last_Reading, 2, 4) ~ "orange",
    map_data$Last_Reading > 4 ~ "red",
    is.na(map_data$Last_Reading) ~ "gray")
  
  
  ####Map Output on SideBar Panel 
  
  output$main_map <- renderLeaflet({
    leaflet(map_data) %>% 
      addTiles() %>%
      fitBounds(-106.9,41.8,-106.7,41.5)%>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~Longitude, 
        lat = ~Latitude,
        color = ~Color, fillOpacity = 0.8,
        radius = 6,
        label = ~`Streamflow (cfs)`,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          #textOnly = TRUE,
          style = list("font-weight" = "bold", "color" = "black", "font-size" = "12px")),
        popup = ~paste0("<b>Site:</b> ", "Pass Creek", "<br>",
                        "<b>Gauge:</b> ", Gauge, "<br>",
                        "<b>Streamflow:</b> ", round(`Streamflow (cfs)`, 1), " cfs<br>",
                        "<b>Last Updated:</b> ", format(TimeStamp, "%Y-%m-%d %H:%M:%S")),
        layerId = ~Gauge) %>%
      
      addLegend(
        position = "topright",
        colors = c("green", "orange", "red"),
        labels = c("Less than 2 hours ago", "2-4 hours ago", "Over 4 hours ago"),
        title = "Legend"
      )
  })
  
  ###Creating Interactive part of MAP
  
  observeEvent(input$main_map_marker_click, {
    selected_site <- input$main_map_marker_click$id
    print(paste("Clicked site: ", selected_site))  # Debugging print statement
    
    if (!is.null(selected_site)) {
      updateTabsetPanel(session, "tabs", selected = selected_site)
      print(paste("Tab switched to: ", selected_site))  # Debugging print statement
    }
  })
  
  ####For SEO Gauge TAB
  
  ##Plot
  
  output$plot_SEO <- renderPlot({
    
    date_range <- reactive_date()
    
    filtered_SEO <- PCSEO[PCSEO$TimeStamp >= reactive_date()[1] & 
                            PCSEO$TimeStamp <= reactive_date()[2], ]
    
    filtered_SEO$TimeStamp<- as.POSIXct(filtered_SEO$TimeStamp, format = "%Y-%m-%d", tz = "UTC")
    
    filtered_SEO$Date <- as.Date(filtered_SEO$TimeStamp)
    
    x_breaks <- NULL
    x_labels <- NULL
    
    if (input$datetime == "current" || input$datetime == "last_7"){
      x_breaks <- "1 day"
      x_labels <- "%Y-%m-%d"
    } else if (input$datetime == "last_14" || input$datetime == "last_month"){
      x_breaks <- "3 days"
      x_labels <- "%Y-%m-%d"
    } else if (input$datetime == "last_6month"){
      x_breaks <- "1 month"
      x_labels <- "%Y-%m-%d"
    } else if (input$datetime == "last_year"){
      x_breaks <- "3 months"
      x_labels <- "%Y-%m-%d"
    } else if (input$datetime == "custom"){
      x_breaks <- "1 week"
      x_labels <- "%Y-%m-%d"
    }
    
    #Need to create 24 hour daily means lm for graph, ensure it is for each start of each day 00:00:00
    filtered_SEO <- filtered_SEO %>%
      group_by(Date) %>%
      mutate(Daily_Mean = mean(`Streamflow (cfs)`, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(filtered_SEO, aes(x = TimeStamp, y = `Streamflow (cfs)`)) +
      geom_point(aes(color = "Observed Streamflow (cfs)"), size = 1) +
      geom_line(aes(color = "Observed Streamflow (cfs)"),linewidth = 1) +
      geom_line(aes(x = as.POSIXct(Date), y = Daily_Mean, 
                    color = "Daily Mean Streamflow (cfs)"), 
                linetype = "dashed", linewidth = 1) +
      scale_color_manual(
        values = c("Observed Streamflow (cfs)" = "dodgerblue4",
                   "Daily Mean Streamflow (cfs)" = "red"),
        labels = c("Daily Mean Streamflow (cfs)", "Observed Streamflow (cfs)") #"Daily Average Streamflow (cfs)", this goes first
      ) +
      theme_classic() + 
      labs(title = "SEO Gauge", 
           subtitle = "Streamflow (cfs) Over Time", 
           x = "Date & Time", 
           y = "Streamflow (cfs)",
           color = "Legend") +
      #scale_y_continuous(limits = c(0, max(filtered_data$`Streamflow (cfs)`, na.rm = TRUE)))+
      
      scale_x_datetime(date_labels = x_labels, 
                       date_breaks = x_breaks,
                       limits = c(date_range[1], current_date),
                       expand = c(0,0)) +
      
      theme(plot.title = element_text(size = 20, face = "bold", family = "serif", hjust = 0.5, margin = margin(b=5)),
            plot.subtitle = element_text(size = 18, face = "italic", family = "serif", hjust = 0.5, margin = margin(b=14, t = 1)),
            axis.title.x = element_text(size = 18, face = "bold", family = "serif", margin = margin(t=10)),
            axis.title.y = element_text(size = 18, face = "bold", family = "serif", margin = margin(r=10)), 
            axis.text.x = element_text(size = 16, face = "bold", family = "serif", angle = 90, hjust = 1),
            axis.text.y = element_text(size = 16, face = "bold", family = "serif"),
            legend.position = "left",
            legend.justification = c(0,1),
            legend.box = "vertical",
            legend.text = element_text(size = 16, family = "serif"),
            legend.title = element_text(size = 18, face = "bold", family = "serif"), 
            legend.background = element_rect(color = "black", fill = NA),
            legend.box.margin = margin(0,20,0,0),
            plot.margin = margin(t=10, r=34, b=15, l=15)
      )
    
  })
  
  ##Table 
  
  output$table_SEO <- renderTable({
    
    #most_recent <- max(PCSEO$TimeStamp, na.rm = TRUE)
    
    PCSEO$Last_Recording <- sapply(PCSEO$TimeStamp, function(ts) {
      timediff_min <- as.numeric(difftime(current_date, ts, units = "min"))
      
      if (timediff_min < 60){
        minutes <- round(timediff_min)
        paste0(minutes, " minute", ifelse(minutes == 1, "", "s"), " ago")
      } else if (timediff_min < 1440) {
        hours <- round(timediff_min / 60)
        paste0(hours, " hour", ifelse(hours == 1, "", "s"), " ago")
      } else {days <- round(timediff_min / 1440)
      paste0(days, " day", ifelse(days == 1, "", "s"), " ago")
      }
    })
    
    
    #most_recent2 <- max(PCPC1$TimeStamp, na.rm = TRUE)
    
    PCPC1$Last_Recording <- sapply(PCPC1$TimeStamp, function(ts) {
      timediff_min2 <- as.numeric(difftime(current_date, ts, units = "min"))
      
      if (timediff_min2 < 60){
        minutes <- round(timediff_min2)
        paste0(minutes, " minute", ifelse(minutes == 1, "", "s"), " ago")
      } else if (timediff_min2 < 1440) {
        hours <- round(timediff_min2 / 60)
        paste0(hours, " hour", ifelse(hours == 1, "", "s"), " ago")
      } else {days <- round(timediff_min2 / 1440)
      paste0(days, " day", ifelse(days == 1, "", "s"), " ago")
      }
    })
    
    PCPC4$Last_Recording <- sapply(PCPC4$TimeStamp, function(ts) {
      timediff_min2 <- as.numeric(difftime(current_date, ts, units = "min"))
      
      if (timediff_min2 < 60){
        minutes <- round(timediff_min2)
        paste0(minutes, " minute", ifelse(minutes == 1, "", "s"), " ago")
      } else if (timediff_min2 < 1440) {
        hours <- round(timediff_min2 / 60)
        paste0(hours, " hour", ifelse(hours == 1, "", "s"), " ago")
      } else {days <- round(timediff_min2 / 1440)
      paste0(days, " day", ifelse(days == 1, "", "s"), " ago")
      }
    })
    
    #Building Table with last recorded for each location
    current_row <- PCSEO[which.max(PCSEO$TimeStamp),]
    current_row2 <- PCPC1[which.max(PCPC1$TimeStamp),]
    current_row3 <- PCPC4[which.max(PCPC4$TimeStamp),]
    
    current_row$TimeStamp <- format(current_date, "%Y-%m-%d %H:%M:%S", tz="UTC")
    current_row2$TimeStamp <- format(current_date, "%Y-%m-%d %H:%M:%S", tz="UTC")
    current_row3$TimeStamp <- format(current_date, "%Y-%m-%d %H:%M:%S", tz="UTC")
    
    colnames(current_row)[colnames(current_row) == "TimeStamp"] <- "Current Date & Time"
    colnames(current_row)[colnames(current_row) == "Last_Recording"] <- "Last Recorded"
    colnames(current_row)[colnames(current_row) == "Streamflow..cfs."] <- "Streamflow (cfs)"
    
    colnames(current_row2)[colnames(current_row2) == "TimeStamp"] <- "Current Date & Time"
    colnames(current_row2)[colnames(current_row2) == "Last_Recording"] <- "Last Recorded"
    colnames(current_row2)[colnames(current_row2) == "Streamflow..cfs."] <- "Streamflow (cfs)"
    
    colnames(current_row3)[colnames(current_row3) == "TimeStamp"] <- "Current Date & Time"
    colnames(current_row3)[colnames(current_row3) == "Last_Recording"] <- "Last Recorded"
    colnames(current_row3)[colnames(current_row3) == "Streamflow..cfs."] <- "Streamflow (cfs)"
    
    filtered_tableSEO <- bind_rows(
      current_row %>%
        select(`Current Date & Time`, `Streamflow (cfs)`, Site, `Last Recorded`),
      current_row2 %>%
        select(`Current Date & Time`, `Streamflow (cfs)`, Site, `Last Recorded`),
      current_row3 %>%
        select(`Current Date & Time`, `Streamflow (cfs)`, Site, `Last Recorded`)
    )
    
    filtered_tableSEO
    
    
  }, striped = TRUE, bordered = TRUE, spacing = "m", digits = 1, na = "NA")
  
  
  
  ####For PC 1 Gauge TAB
  
  output$plot_PC1 <- renderPlot({
    
    date_range <- reactive_date()
    
    filtered_PC1 <- PCPC1[PCPC1$TimeStamp >= reactive_date()[1] & 
                            PCPC1$TimeStamp <= reactive_date()[2], ]
    
    filtered_PC1$TimeStamp<- as.POSIXct(filtered_PC1$TimeStamp, format = "%Y-%m-%d", tz = "UTC")
    
    filtered_PC1$Date <- as.Date(filtered_PC1$TimeStamp)
    
    x_breaks2 <- NULL
    x_labels2 <- NULL
    
    if (input$datetime == "current" || input$datetime == "last_7"){
      x_breaks2 <- "1 day"
      x_labels2 <- "%Y-%m-%d"
    } else if (input$datetime == "last_14" || input$datetime == "last_month"){
      x_breaks2 <- "3 days"
      x_labels2 <- "%Y-%m-%d"
    } else if (input$datetime == "last_6month"){
      x_breaks2 <- "1 month"
      x_labels2 <- "%Y-%m-%d"
    } else if (input$datetime == "last_year"){
      x_breaks2 <- "3 months"
      x_labels2 <- "%Y-%m-%d"
    } else if (input$datetime == "custom"){
      x_breaks2 <- "1 week"
      x_labels2 <- "%Y-%m-%d"
    }
    
    
    filtered_PC1 <- filtered_PC1 %>%
      group_by(Date) %>%
      mutate(Daily_Mean = mean(`Streamflow (cfs)`, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(filtered_PC1, aes(x = TimeStamp, y = `Streamflow (cfs)`)) +
      geom_point(aes(color = "Observed Streamflow (cfs)"), size = 1) +
      geom_line(aes(color = "Observed Streamflow (cfs)"),linewidth = 1) +
      geom_line(aes(x = as.POSIXct(Date), y = Daily_Mean, 
                    color = "Daily Mean Streamflow (cfs)"), 
                linetype = "dashed", linewidth = 1) +
      scale_color_manual(
        values = c("Observed Streamflow (cfs)" = "dodgerblue4",
                   "Daily Mean Streamflow (cfs)" = "red"),
        labels = c("Daily Mean Streamflow (cfs)", "Observed Streamflow (cfs)") #"Daily Average Streamflow (cfs)", this goes first
      ) +
      theme_classic() + 
      labs(title = "PC #1 Gauge", 
           subtitle = "Streamflow (cfs) Over Time", 
           x = "Date & Time", 
           y = "Streamflow (cfs)",
           color = "Legend") +
      #scale_y_continuous(limits = c(0, max(filtered_data$`Streamflow (cfs)`, na.rm = TRUE)))+
      
      scale_x_datetime(date_labels = x_labels2, 
                       date_breaks = x_breaks2,
                       limits = c(date_range[1], current_date),
                       expand = c(0,0)) +
      
      theme(plot.title = element_text(size = 20, face = "bold", family = "serif", hjust = 0.5, margin = margin(b=5)),
            plot.subtitle = element_text(size = 18, face = "italic", family = "serif", hjust = 0.5, margin = margin(b=14, t = 1)),
            axis.title.x = element_text(size = 18, face = "bold", family = "serif", margin = margin(t=10)),
            axis.title.y = element_text(size = 18, face = "bold", family = "serif", margin = margin(r=10)), 
            axis.text.x = element_text(size = 16, face = "bold", family = "serif", angle = 90, hjust = 1),
            axis.text.y = element_text(size = 16, face = "bold", family = "serif"),
            legend.position = "left",
            legend.justification = c(0,1),
            legend.box = "vertical",
            legend.text = element_text(size = 16, family = "serif"),
            legend.title = element_text(size = 18, face = "bold", family = "serif"), 
            legend.background = element_rect(color = "black", fill = NA),
            legend.box.margin = margin(0,20,0,0),
            plot.margin = margin(t=10, r=34, b=15, l=15)
      )
    
  })
  
  output$table_PC1 <- renderTable({
    
    #most_recent <- max(PCSEO$TimeStamp, na.rm = TRUE)
    
    PCSEO$Last_Recording <- sapply(PCSEO$TimeStamp, function(ts) {
      timediff_min <- as.numeric(difftime(current_date, ts, units = "min"))
      
      if (timediff_min < 60){
        paste0(round(timediff_min), " min ago")
      } else if (timediff_min < 1440) {
        paste0(round(timediff_min/60), " hours ago")
      } else {paste0(round(timediff_min/1440), " days ago")
      }
    })
    
    
    #most_recent2 <- max(PCPC1$TimeStamp, na.rm = TRUE)
    
    PCPC1$Last_Recording <- sapply(PCPC1$TimeStamp, function(ts) {
      timediff_min2 <- as.numeric(difftime(current_date, ts, units = "min"))
      
      if (timediff_min2 < 60){
        paste0(round(timediff_min2), " min ago")
      } else if (timediff_min2 < 1440) {
        paste0(round(timediff_min2/60), " hours ago")
      } else {paste0(round(timediff_min2/1440), " days ago")
      }
    })
    
    
    PCPC4$Last_Recording <- sapply(PCPC4$TimeStamp, function(ts) {
      timediff_min2 <- as.numeric(difftime(current_date, ts, units = "min"))
      
      if (timediff_min2 < 60){
        paste0(round(timediff_min2), " min ago")
      } else if (timediff_min2 < 1440) {
        paste0(round(timediff_min2/60), " hours ago")
      } else {paste0(round(timediff_min2/1440), " days ago")
      }
    })
    
    #Building Table with last recorded for each location
    
    current_row <- PCSEO[which.max(PCSEO$TimeStamp),]
    current_row2 <- PCPC1[which.max(PCPC1$TimeStamp),]
    current_row3 <- PCPC4[which.max(PCPC4$TimeStamp),]
    
    
    current_row$TimeStamp <- format(current_row$TimeStamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    current_row2$TimeStamp <- format(current_row2$TimeStamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    current_row3$TimeStamp <- format(current_row3$TimeStamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    colnames(current_row)[colnames(current_row) == "TimeStamp"] <- "Current Date & Time"
    colnames(current_row)[colnames(current_row) == "Last_Recording"] <- "Last Recorded"
    colnames(current_row)[colnames(current_row) == "Streamflow..cfs."] <- "Streamflow (cfs)"
    
    colnames(current_row2)[colnames(current_row2) == "TimeStamp"] <- "Current Date & Time"
    colnames(current_row2)[colnames(current_row2) == "Last_Recording"] <- "Last Recorded"
    colnames(current_row2)[colnames(current_row2) == "Streamflow..cfs."] <- "Streamflow (cfs)"
    
    colnames(current_row3)[colnames(current_row3) == "TimeStamp"] <- "Current Date & Time"
    colnames(current_row3)[colnames(current_row3) == "Last_Recording"] <- "Last Recorded"
    colnames(current_row3)[colnames(current_row3) == "Streamflow..cfs."] <- "Streamflow (cfs)"
    
    
    filtered_tablePC1 <- bind_rows(
      current_row %>%
        select(`Current Date & Time`, `Streamflow (cfs)`, Site, `Last Recorded`),
      current_row2 %>%
        select(`Current Date & Time`, `Streamflow (cfs)`, Site, `Last Recorded`),
      current_row3 %>%
        select(`Current Date & Time`, `Streamflow (cfs)`, Site, `Last Recorded`)
    )
    
    filtered_tablePC1
    
    
  }, striped = TRUE, bordered = TRUE, spacing = "m", digits = 1, na = "NA")
  
  
  
  ####For PC 4 Gauge TAB
  
  output$plot_PC4 <- renderPlot({
    
    date_range <- reactive_date()
    
    filtered_PC4 <- PCPC4[PCPC4$TimeStamp >= reactive_date()[1] & 
                            PCPC4$TimeStamp <= reactive_date()[2], ]
    
    filtered_PC4$TimeStamp<- as.POSIXct(filtered_PC4$TimeStamp, format = "%Y-%m-%d", tz = "UTC")
    
    filtered_PC4$Date <- as.Date(filtered_PC4$TimeStamp)
    
    x_breaks3 <- NULL
    x_labels3 <- NULL
    
    if (input$datetime == "current" || input$datetime == "last_7"){
      x_breaks3 <- "1 day"
      x_labels3 <- "%Y-%m-%d"
    } else if (input$datetime == "last_14" || input$datetime == "last_month"){
      x_breaks3 <- "3 days"
      x_labels3 <- "%Y-%m-%d"
    } else if (input$datetime == "last_6month"){
      x_breaks3 <- "1 month"
      x_labels3 <- "%Y-%m-%d"
    } else if (input$datetime == "last_year"){
      x_breaks3 <- "3 months"
      x_labels3 <- "%Y-%m-%d"
    } else if (input$datetime == "custom"){
      x_breaks3 <- "1 week"
      x_labels3 <- "%Y-%m-%d"
    }
    
    filtered_PC4 <- filtered_PC4 %>%
      group_by(Date) %>%
      mutate(Daily_Mean = mean(`Streamflow (cfs)`, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(filtered_PC4, aes(x = TimeStamp, y = `Streamflow (cfs)`)) +
      geom_point(aes(color = "Observed Streamflow (cfs)"), size = 1) +
      geom_line(aes(color = "Observed Streamflow (cfs)"),linewidth = 1) +
      geom_line(aes(x = as.POSIXct(Date), y = Daily_Mean, 
                    color = "Daily Mean Streamflow (cfs)"), 
                linetype = "dashed", linewidth = 1) +
      scale_color_manual(
        values = c("Observed Streamflow (cfs)" = "dodgerblue4",
                   "Daily Mean Streamflow (cfs)" = "red"),
        labels = c("Daily Mean Streamflow (cfs)", "Observed Streamflow (cfs)") #"Daily Average Streamflow (cfs)", this goes first
      ) +
      theme_classic() + 
      labs(title = "PC #4 Gauge", 
           subtitle = "Streamflow (cfs) Over Time", 
           x = "Date & Time", 
           y = "Streamflow (cfs)",
           color = "Legend") +
      #scale_y_continuous(limits = c(0, max(filtered_data$`Streamflow (cfs)`, na.rm = TRUE)))+
      
      scale_x_datetime(date_labels = x_labels3, 
                       date_breaks = x_breaks3,
                       limits = c(date_range[1], current_date),
                       expand = c(0,0)) +
      
      theme(plot.title = element_text(size = 20, face = "bold", family = "serif", hjust = 0.5, margin = margin(b=5)),
            plot.subtitle = element_text(size = 18, face = "italic", family = "serif", hjust = 0.5, margin = margin(b=14, t = 1)),
            axis.title.x = element_text(size = 18, face = "bold", family = "serif", margin = margin(t=10)),
            axis.title.y = element_text(size = 18, face = "bold", family = "serif", margin = margin(r=10)), 
            axis.text.x = element_text(size = 16, face = "bold", family = "serif", angle = 90, hjust = 1),
            axis.text.y = element_text(size = 16, face = "bold", family = "serif"),
            legend.position = "left",
            legend.justification = c(0,1),
            legend.box = "vertical",
            legend.text = element_text(size = 16, family = "serif"),
            legend.title = element_text(size = 18, face = "bold", family = "serif"), 
            legend.background = element_rect(color = "black", fill = NA),
            legend.box.margin = margin(0,20,0,0),
            plot.margin = margin(t=10, r=34, b=15, l=15)
      )
    
  })
  
  output$table_PC4 <- renderTable({
    
    #most_recent <- max(PCSEO$TimeStamp, na.rm = TRUE)
    
    PCSEO$Last_Recording <- sapply(PCSEO$TimeStamp, function(ts) {
      timediff_min <- as.numeric(difftime(current_date, ts, units = "min"))
      
      if (timediff_min < 60){
        paste0(round(timediff_min), " min ago")
      } else if (timediff_min < 1440) {
        paste0(round(timediff_min/60), " hours ago")
      } else {paste0(round(timediff_min/1440), " days ago")
      }
    })
    
    
    #most_recent2 <- max(PCPC1$TimeStamp, na.rm = TRUE)
    
    PCPC1$Last_Recording <- sapply(PCPC1$TimeStamp, function(ts) {
      timediff_min2 <- as.numeric(difftime(current_date, ts, units = "min"))
      
      if (timediff_min2 < 60){
        paste0(round(timediff_min2), " min ago")
      } else if (timediff_min2 < 1440) {
        paste0(round(timediff_min2/60), " hours ago")
      } else {paste0(round(timediff_min2/1440), " days ago")
      }
    })
    
    
    PCPC4$Last_Recording <- sapply(PCPC4$TimeStamp, function(ts) {
      timediff_min2 <- as.numeric(difftime(current_date, ts, units = "min"))
      
      if (timediff_min2 < 60){
        paste0(round(timediff_min2), " min ago")
      } else if (timediff_min2 < 1440) {
        paste0(round(timediff_min2/60), " hours ago")
      } else {paste0(round(timediff_min2/1440), " days ago")
      }
    })
    
    #Building Table with last recorded for each location
    
    current_row <- PCSEO[which.max(PCSEO$TimeStamp),]
    current_row2 <- PCPC1[which.max(PCPC1$TimeStamp),]
    current_row3 <- PCPC4[which.max(PCPC4$TimeStamp),]
    
    
    current_row$TimeStamp <- format(current_row$TimeStamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    current_row2$TimeStamp <- format(current_row2$TimeStamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    current_row3$TimeStamp <- format(current_row3$TimeStamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    colnames(current_row)[colnames(current_row) == "TimeStamp"] <- "Current Date & Time"
    colnames(current_row)[colnames(current_row) == "Last_Recording"] <- "Last Recorded"
    colnames(current_row)[colnames(current_row) == "Streamflow..cfs."] <- "Streamflow (cfs)"
    
    colnames(current_row2)[colnames(current_row2) == "TimeStamp"] <- "Current Date & Time"
    colnames(current_row2)[colnames(current_row2) == "Last_Recording"] <- "Last Recorded"
    colnames(current_row2)[colnames(current_row2) == "Streamflow..cfs."] <- "Streamflow (cfs)"
    
    colnames(current_row3)[colnames(current_row3) == "TimeStamp"] <- "Current Date & Time"
    colnames(current_row3)[colnames(current_row3) == "Last_Recording"] <- "Last Recorded"
    colnames(current_row3)[colnames(current_row3) == "Streamflow..cfs."] <- "Streamflow (cfs)"
    
    
    filtered_tablePC4 <- bind_rows(
      current_row %>%
        select(`Current Date & Time`, `Streamflow (cfs)`, Site, `Last Recorded`),
      current_row2 %>%
        select(`Current Date & Time`, `Streamflow (cfs)`, Site, `Last Recorded`),
      current_row3 %>%
        select(`Current Date & Time`, `Streamflow (cfs)`, Site, `Last Recorded`)
    )
    
    
    
    filtered_tablePC4
    
    
  }, striped = TRUE, bordered = TRUE, spacing = "m", digits = 1, na = "NA")
  
  
}


####################
#  Create Shiny App

shinyApp(ui=ui, server=server)
