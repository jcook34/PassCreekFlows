#######################################
#  Automated Gauge Data Collection    #
#######################################

library(jsonlite)
library(httr)
library(httr2)
library(tidyverse)
library(curl)

### Rating Curves
PC1Rating <- function(depth) {
  if (depth < 0.6) {
    24.787 * depth^2 + 0.9622 * depth - 0.0162
  } else {
    1.7004 * exp(2.8282 * depth)
  }
}
PC4Rating <- function(depth) {
  97.797 * depth - 196.44
}

### SEO Function
SEO_Flow_Extraction <- function(SEO_Num, date_start, date_end) {
  RawData <- POST(
    paste0("https://seoflow.wyo.gov/Data/DatasetGrid?dataset=", SEO_Num),
    config = list(
      "cookie" = "ASP.NET_SessionId=s03zmbrzalgfl3xgj4smuvme",
      "ss-pid" = "bCVfzc06TRDOhn0EVvQo",
      "ss-id" = "ZUAcR12zag8dSs8UsEt1"
    ),
    body = list(
      "sort" = "TimeStamp-Desc",
      "page" = "1",
      "pageSize" = "10000",
      "group" = "",
      "filter" = "",
      "interval" = "Custom",
      "timezone" = "1",
      "date" = date_start,
      "endDate" = date_end,
      "calendar" = "0",
      "alldata" = "false"
    ),
    encode = "form"
  )
  
  RawData <- (fromJSON(content(RawData, as = "text"))[[1]])[, c(10:11)] %>%
    mutate(TimeStamp = as.POSIXct(TimeStamp, format = "%Y-%m-%dT%H:%M:%S", tz = "America/Denver"))
  
  return(RawData)
}

update_seo_data <- function(file_path, seo_id) {
  if (file.exists(file_path)) {
    existing <- read_csv(file_path, show_col_types = FALSE)
    last_time <- max(existing$TimeStamp)
  } else {
    existing <- tibble()
    last_time <- Sys.Date() - 7
  }
  
  new_data <- SEO_Flow_Extraction(seo_id, last_time, Sys.Date() + 2) %>%
    mutate(TimeStamp = TimeStamp - 6 * 60 * 60) # Adjust timezone
  
  updated <- bind_rows(existing, new_data) %>%
    distinct(TimeStamp, .keep_all = TRUE) %>%
    arrange(TimeStamp)
  
  write_csv(updated, file_path)
}

### HydroVu Function
fetch_data <- function(site, datainterval, timeoffset, n) {
  r <- POST("https://www.hydrovu.com/public-api/oauth/token",
            body = list(
              grant_type = "client_credentials",
              client_id = Sys.getenv("HYDROVU_CLIENT_ID"),
              client_secret = Sys.getenv("HYDROVU_CLIENT_SECRET"),
              scope = "read:locations"
            ),
            encode = "form")
  warn_for_status(r)
  tok <- content(r)$access_token
  
  strttime <- as.numeric(Sys.Date() - 7) * 86400 + timeoffset * 3600
  endtime  <- as.numeric(Sys.Date() + 2) * 86400 + timeoffset * 3600
  times <- seq(strttime, endtime, by = 60 * datainterval)
  
  Flows <- tibble(Date = numeric(), Flow = numeric())
  
  for (i in 1:ceiling(length(times) / 40)) {
    url <- paste0("https://www.hydrovu.com/public-api/v1/locations/", site,
                  "/data?endTime=", times[i * 40],
                  "&startTime=", times[1 + (i - 1) * 40])
    
    headers <- c(
      "accept" = "application/json",
      "authorization" = paste("Bearer", tok)
    )
    
    h <- new_handle()
    handle_setopt(h, customrequest = "GET")
    handle_setheaders(h, .list = headers)
    
    response <- curl_fetch_memory(url, handle = h)
    res_txt <- rawToChar(response$content)
    if (fromJSON(res_txt)[[1]] != site) next
    
    response_data <- switch(as.character(n),
                            "1" = fromJSON(res_txt)[[2]][[1]][[4]][[1]][[1]],
                            "2" = fromJSON(res_txt)[[2]][[1]][[4]][[1]][[2]],
                            "3" = fromJSON(res_txt)[[2]][[1]][[4]][[1]][[3]])
    
    if (length(response_data) > 1) {
      Flows <- bind_rows(Flows, response_data)
    }
  }
  
  Flows$Date <- as.POSIXct(Flows$Date)
  return(Flows)
}

update_hydrovu_data <- function(file_path, site, conversion_fn, rating_fn, n) {
  if (file.exists(file_path)) {
    existing <- read_csv(file_path, show_col_types = FALSE)
    last_time <- max(existing$Date)
  } else {
    existing <- tibble()
    last_time <- Sys.Date() - 7
  }
  
  new_data <- fetch_data(site, 15, 0, n) %>%
    filter(Date > last_time)
  
  if (n == 1) {
    new_data <- new_data %>%
      mutate(Staff = new_data[, 2] * 3.28084 - 1.18,
             `Flow (cfs)` = sapply(Staff, rating_fn))
  } else {
    new_data <- new_data %>%
      mutate(`Flow (cfs)` = sapply(new_data[, 2] * 3.28084, rating_fn))
  }
  
  updated <- bind_rows(existing, new_data) %>%
    distinct(Date, .keep_all = TRUE) %>%
    arrange(Date)
  
  write_csv(updated, file_path)
}

#######################################
#        Run All Updates              #
#######################################

update_seo_data("PassCreekSEO.csv", "4989")
update_hydrovu_data("PassCreekPC1.csv", 6102784244711424, function(x) x * 3.28084 - 1.18, PC1Rating, 3)
update_hydrovu_data("PassCreekPC4.csv", 5569481418735616, function(x) x * 3.28084, PC4Rating, 3)
