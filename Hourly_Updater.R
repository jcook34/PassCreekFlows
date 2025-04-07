# Required libraries
library(httr2)      # API requests
library(jsonlite)   # JSON parsing

# --- Rating Curves ---
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

# --- SEO Flow Extraction ---
SEO_Flow_Extraction <- function(SEO_Num, date_start, date_end) {
  req <- request(paste0("https://seoflow.wyo.gov/Data/DatasetGrid?dataset=", SEO_Num)) |>
    req_body_form(
      sort = "TimeStamp-Desc",
      page = "1",
      pageSize = "10000",
      interval = "Custom",
      timezone = "1",
      date = date_start,
      endDate = date_end,
      calendar = "0",
      alldata = "false"
    ) |>
    req_headers("Content-Type" = "application/x-www-form-urlencoded") |>
    req_method("POST")

  resp <- req_perform(req)
  data <- resp_body_json(resp)[[1]][, c(10, 11)]
  data$TimeStamp <- as.POSIXct(data$TimeStamp, format = "%Y-%m-%dT%H:%M:%S", tz = "America/Denver")
  return(data)
}

# --- Append only new rows ---
update_csv <- function(new_data, csv_file) {
  if (file.exists(csv_file)) {
    old_data <- read.csv(csv_file)
    last_timestamp <- max(as.POSIXct(old_data$TimeStamp))
    new_data <- new_data[new_data$TimeStamp > last_timestamp, ]
    combined <- rbind(old_data, new_data)
  } else {
    combined <- new_data
  }
  write.csv(combined, csv_file, row.names = FALSE)
}

# --- HydroVu Data Fetch ---
fetch_data <- function(site, datainterval, timeoffset, n) {
  auth <- request("https://www.hydrovu.com/public-api/oauth/token") |>
    req_body_form(
      grant_type = "client_credentials",
      client_id = Sys.getenv("HYDROVU_CLIENT_ID"),
      client_secret = Sys.getenv("HYDROVU_CLIENT_SECRET"),
      scope = "read:locations"
    ) |>
    req_method("POST") |>
    req_perform()

  token <- resp_body_json(auth)$access_token

  start_time <- as.numeric(Sys.Date() - 7) * 86400 + timeoffset * 3600
  end_time   <- as.numeric(Sys.Date() + 2) * 86400 + timeoffset * 3600
  times <- seq(start_time, end_time, by = 60 * datainterval)

  Flows <- data.frame(Date = numeric(), Flow = numeric())

  for (i in 1:ceiling(length(times) / 40)) {
    url <- paste0("https://www.hydrovu.com/public-api/v1/locations/", site,
                  "/data?endTime=", times[min(i * 40, length(times))],
                  "&startTime=", times[1 + (i - 1) * 40])

    req <- request(url) |>
      req_headers("Authorization" = paste("Bearer", token), "accept" = "application/json") |>
      req_perform()

    resp <- resp_body_json(req)
    if (length(resp) > 1) {
      flows_part <- resp[[2]][[1]][[4]][[n]]
      Flows <- rbind(Flows, flows_part)
    }
  }

  Flows$Date <- as.POSIXct(Flows$Date, origin = "1970-01-01", tz = "America/Denver")
  return(Flows)
}

# --- Extract and Save SEO Data ---
PassCreekSEO <- SEO_Flow_Extraction("4989", Sys.Date() - 7, Sys.Date() + 2)
PassCreekSEO$TimeStamp <- PassCreekSEO$TimeStamp - 6 * 60 * 60  # Adjust for local time
update_csv(PassCreekSEO, "PassCreekSEO.csv")

# --- Extract HydroVu for PC1 ---
PC1Depth <- fetch_data(6102784244711424, 15, 0, 3)
PC1Depth$Staff <- PC1Depth$Flow * 3.28084 - 1.18
PC1Depth$`Flow (cfs)` <- sapply(PC1Depth$Staff, PC1Rating)
colnames(PC1Depth)[which(names(PC1Depth) == "Date")] <- "TimeStamp"
update_csv(PC1Depth, "PassCreekPC1.csv")

# --- Extract HydroVu for PC4 ---
PC4Depth <- fetch_data(5569481418735616, 15, 0, 3)
PC4Depth$`Flow (cfs)` <- sapply(PC4Depth$Flow * 3.28084, PC4Rating)
colnames(PC4Depth)[which(names(PC4Depth) == "Date")] <- "TimeStamp"
update_csv(PC4Depth, "PassCreekPC4.csv")
