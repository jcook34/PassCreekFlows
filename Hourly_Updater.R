library(httr)    # Use httr for API calls
library(tidyverse)  # Efficient data manipulation with dplyr

# Function to calculate PC1 flow
PC1Rating <- function(depth){
  if(depth < 0.6){
    24.787 * depth^2 + 0.9622 * depth - 0.0162
  } else {
    1.7004 * exp(2.8282 * depth)
  }
}

# Function to calculate PC4 flow
PC4Rating <- function(depth) {
  97.797 * depth - 196.44
}

# SEO Flow Extraction - only retrieves data not already in CSV
SEO_Flow_Extraction <- function(SEO_Num, date_start, date_end) {
  RawData <- POST(paste("https://seoflow.wyo.gov/Data/DatasetGrid?dataset=", SEO_Num, sep=""),
                  body = list(
                    "sort" = "TimeStamp-Desc",
                    "page" = "1",
                    "pageSize" = "10000",
                    "interval" = "Custom",
                    "timezone" = "1",
                    "date" = date_start,
                    "endDate" = date_end,
                    "calendar" = "0",
                    "alldata" = "false"), 
                  encode = "form")
  data <- content(RawData, "parsed")[[1]]
  data <- data[, c(10, 11)] %>%
    mutate(TimeStamp = as.POSIXct(TimeStamp, format = "%Y-%m-%dT%H:%M:%S", tz = "America/Denver"))
  
  return(data)
}

# Efficiently append new data to the existing CSV file
update_csv <- function(new_data, csv_file) {
  if (file.exists(csv_file)) {
    old_data <- read.csv(csv_file)
    # Find the most recent timestamp in the existing data
    last_timestamp <- max(as.POSIXct(old_data$TimeStamp))
    new_data <- filter(new_data, TimeStamp > last_timestamp)
  } else {
    old_data <- new_data  # If no file, start fresh
  }
  
  combined_data <- bind_rows(old_data, new_data)
  write.csv(combined_data, csv_file, row.names = FALSE)
}

# HydroVu data fetching function
fetch_data <- function(site, datainterval, timeoffset, n) {
  token_response <- POST("https://www.hydrovu.com/public-api/oauth/token", 
                         body = list(
                           grant_type = "client_credentials",
                           client_id = Sys.getenv("HYDROVU_CLIENT_ID"),
                           client_secret = Sys.getenv("HYDROVU_CLIENT_SECRET"),
                           scope = "read:locations"
                         ), encode = "form")
  
  token <- content(token_response)$access_token
  start_time <- as.numeric(Sys.Date() - 7) * 86400 + timeoffset * 3600  # convert to seconds
  end_time <- as.numeric(Sys.Date() + 2) * 86400 + timeoffset * 3600  # convert to seconds
  
  times <- seq(start_time, end_time, by = 60 * datainterval)
  
  # Initialize an empty data frame to store flows
  Flows <- data.frame(Date = numeric(), Flow = numeric())
  
  for (i in 1:ceiling(length(times)/40)) {
    url <- paste("https://www.hydrovu.com/public-api/v1/locations/", site, "/data?endTime=", times[i * 40], "&startTime=", times[1 + (i - 1) * 40], sep = "")
    headers <- c("accept" = "application/json", "authorization" = paste("Bearer ", token, sep = ""))
    response <- GET(url, add_headers(.headers = headers))
    
    # Parse the response based on n
    response_data <- content(response, "parsed")
    if (length(response_data) > 1) {
      Flows <- rbind(Flows, response_data[[2]][[1]][[4]][[n]])
    }
  }
  
  Flows$Date <- as.POSIXct(Flows$Date, origin = "1970-01-01", tz = "America/Denver")
  return(Flows)
}

# Run SEO data extraction and update the CSV file
PassCreekSEO <- SEO_Flow_Extraction("4989", Sys.Date() - 7, Sys.Date() + 2)
PassCreekSEO$TimeStamp <- PassCreekSEO$TimeStamp - 6 * 60 * 60  # Adjust for time zone
update_csv(PassCreekSEO, "PassCreekSEO.csv")

# Run HydroVu data extraction and update the CSV files for PC1 and PC4
PC1Depth <- fetch_data(6102784244711424, 15, 0, 3)  # Depth in meters from level troll
PC1Depth$Staff <- PC1Depth$Flow * 3.28084 - 1.18  # Convert depth to staff height in feet
PC1Depth$`Flow (cfs)` <- sapply(PC1Depth$Staff, PC1Rating)

PC4Depth <- fetch_data(5569481418735616, 15, 0, 3)  # Depth in feet
PC4Depth$`Flow (cfs)` <- sapply(PC4Depth$Flow * 3.28084, PC4Rating)  # Convert to feet

update_csv(PC1Depth, "PassCreekPC1.csv")
update_csv(PC4Depth, "PassCreekPC4.csv")
