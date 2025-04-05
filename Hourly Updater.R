#######################################
#Code for Extracting Recent Gauge Data#
#######################################

SEO_Flow_Extraction <- function(SEO_Num,date_start,date_end) {
  
  library(jsonlite)
  library(httr)#2)
  library(tidyverse)
  
  RawData <- POST(paste("https://seoflow.wyo.gov/Data/DatasetGrid?dataset=",SEO_Num,sep=""),
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
                    "alldata" = "false"), 
                  encode = "form")
  RawData <- (fromJSON(content(RawData, as="text"))[[1]])[,c(10:11)] %>% 
    mutate(TimeStamp = as.POSIXct(TimeStamp, format = "%Y-%m-%dT%H:%M:%S", tz = "America/Denver"))
  
  return(RawData)
}

PassCreekSEO <- data.frame(SEO_Flow_Extraction("4989",Sys.Date()-1,Sys.Date()+1))
PassCreekSEO$TimeStamp <- PassCreekSEO$TimeStamp-6*60*60

#setwd("C:/Users/srogacz1/Dropbox/RShiny Apps/Pass Creek/R Web App")
write.csv(PassCreekSEO,"PassCreekSEO.csv", row.names = FALSE)



#For HydroVu PC1 and PC4 -- PC4 won't be available until January

#In-SITU Download of latest data
fetch_data <- function(site,datainterval,timeoffset,n) {
  
  r <- POST("https://www.hydrovu.com/public-api/oauth/token",
            config = list(),
            body = list(
              grant_type="client_credentials",
              client_id="irrigationfallowingcurtailment",
              client_secret="cac0e8b984c84095b65af92af711309c",
              scope="read:locations"#api_suburbperformance_read"
            ),
            encode = "form"
  )
  warn_for_status(r)          
  #content(r)
  tok <- content(r)$access_token
  
  strttime = as.numeric(Sys.Date()-1)*86400+timeoffset*3600 # start time of data retrieval: datre in days converted to seconds with time offset for timedifferences
  endtime = as.numeric(Sys.Date()+2)*86400+timeoffset*3600 # end time of data retrieval: datre in days converted to seconds with time offset for timedifferences
  times <- seq(strttime, endtime, by = 60*datainterval) #data timestamps
  
  Flows <- data.frame(Date=numeric(),Flow=numeric())
  
  for (i in 1:ceiling(length(times)/40)){
    
    # Define the URL
    url <- paste("https://www.hydrovu.com/public-api/v1/locations/",site,"/data?endTime=",times[i*40],"&startTime=",times[1+(i-1)*40],sep="")
    
    # Set up the headers
    headers <- c(
      "accept" = "application/json",
      "authorization" = paste("Bearer ",tok,sep="")
    )
    
    # Fetch the first page of data
    h <- new_handle()
    
    # Set the options for the handle
    handle_setopt(h, customrequest = "GET")
    handle_setheaders(h, .list = headers)
    
    # Perform the GET request
    response <- curl_fetch_memory(url, handle = h)
    if(fromJSON(rawToChar(response$content))[1][[1]]!=site){next}
    if(n==1){response <- fromJSON(rawToChar(response$content))[2][[1]][4][[1]][[1]]}
    if(n==2){response <- fromJSON(rawToChar(response$content))[2][[1]][4][[1]][[2]]}
    if(n==3){response <- fromJSON(rawToChar(response$content))[2][[1]][4][[1]][[3]]}
    if(length(response)>1){Flows <- rbind(Flows,response)}
  }
  Flows[,1] <- as.POSIXct(Flows[,1])
  return(Flows)
}

#PC1FLow <- fetch_data(5002084488052736,15,0,1) #convert to cfs before displaying
PC4Depth <- fetch_data(5569481418735616,15,0,3) #depth in feet

PC1Depth <- fetch_data(6102784244711424,15,0,3) #depth in meters from level troll
PC1Depth$Staff <- PC1Depth[,2]*3.28084-1.18 #convert depth of troll in m to staff height in ft

PC1Rating <- function(depth){
  if(depth<0.6){
    24.787*depth^2 + 0.9622*depth - 0.0162
  }else{
    1.7004* exp(2.8282*depth)
  }
}

PC1Depth$`Flow (cfs)` <- sapply(PC1Depth$Staff,PC1Rating)

#For PC4 -- Does not have Staff
PC4Depth <- fetch_data(5569481418735616,15,0,3)
PC4Rating <-function(depth){97.797*depth - 196.44}
PC4Depth$`Flow (cfs)` <-sapply(PC4Depth[,2]*3.28084,PC4Rating) #need to change from meters to feet

#setwd("C:/Users/srogacz1/Dropbox/RShiny Apps/Pass Creek/R Web App")
write.csv(PC1Depth,"data/PassCreekPC1.csv", row.names = FALSE)
write.csv(PC4Depth,"data/PassCreekPC4.csv", row.names = FALSE)