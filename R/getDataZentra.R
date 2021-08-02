#' Function to get data from the API of zentracloud.com
#'
#' @param token token get from zentracloud API
#' @param var which variable you want to get
#' @param port the port number of sensor
#' @param time_span vector of two having time range for which the data is required
#'
#' @return
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET add_headers content
#' @importFrom lubridate now days
#' @importFrom tibble tibble
#' @examples
#' # Get data of VPD for the last five days
#' # token <- my_token
#' # VPD_data <- getDataZentra(token,var = "VPD",port =1)

getDataZentra <- function(token,var='Water Content',port,time_span=c(now(),now()-days(5))){
  url = "https://zentracloud.com/api/v3/get_readings/"
  resp <- GET(url,
              add_headers("Authorization" =token ),
              query = list(
                device_sn = "z6-10293",
                end_date =format(time_span[1],"%m-%d-%Y %H:%M"),
                start_date = format(time_span[2],"%m-%d-%Y %H:%M")
              ))

  data <- fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  if (names(data)[1] == 'detail') stop(data$detail)
  
  varPos <- which(var == names(data[['data']]))
  ports <- unlist(sapply(data[['data']][[varPos]],'[[',1)[3,])
  idPort <- which(ports == port)
  time <- .parseZENTRA(data,varPos,idPort,"datetime")
  value <- .parseZENTRA(data,varPos,idPort,"value")
  units <- sapply(data[['data']][[varPos]],'[[',1)[6,idPort]
  
  tibble(time,value,units$units)
}

#' @export
.parseZENTRA <- function(x,varPos,idPort,reading){
  sapply(x[['data']][[varPos]][[idPort]][[2]],'[[',reading)
}
