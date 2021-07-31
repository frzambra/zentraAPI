library(httr)
library(lubridate)
library(tibble)

getDataZentra <- function(token,var='Water Content',port,time_span=c(now(),now()-days(5))){
  url = "https://zentracloud.com/api/v3/get_readings/"
  resp <- GET(url,
              add_headers("Authorization" =token ),
              query = list(
                device_sn = "z6-10293",
                end_date =format(time_span[1],"%m-%d-%Y %H:%M"),
                start_date = format(time_span[2],"%m-%d-%Y %H:%M")
              ))

  data <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  varPos <- which(var == names(data[['data']]))
  ports <- unlist(sapply(data[['data']][[varPos]],'[[',1)[3,])
  idPort <- which(ports == port)
  time <- sapply(data[['data']][[varPos]][[idPort]][[2]],'[[',"datetime")
  value <- sapply(data[['data']][[varPos]][[idPort]][[2]],'[[',"value")
  units <- sapply(data[['data']][[varPos]],'[[',1)[6,idPort]
  
  tibble(time,value,units$units)
}

#variables 

# [1] "Air Temperature"       "Atmospheric Pressure"  "Battery Percent"      
# [4] "Battery Voltage"       "Gust Speed"            "Lightning Activity"   
# [7] "Lightning Distance"    "Logger Temperature"    "Max Precip Rate"      
# [10] "Precipitation"         "RH Sensor Temp"        "Reference Pressure"   
# [13] "Saturation Extract EC" "Soil Temperature"      "Solar Radiation"      
# [16] "VPD"                   "Vapor Pressure"        "Water Content"        
# [19] "Wind Direction"        "Wind Speed"            "X-axis Level"         
# [22] "Y-axis Level"         

#testing function
token <- "Token e8357468a4eda4fdc97d480fbd0ba0cfac3c0e4b"
getDataZentra(token,var = "Water Content",port =2)
