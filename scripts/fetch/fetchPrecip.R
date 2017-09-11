#fetch precip data w/geoknife
fetch.precip <- function(viz = as.viz('precip-data')){
  if(!viz[['refetch']]){
    return(NULL)
  }
  required <- c("location","start.date", "end.date")
  checkRequired(viz, required)
  
  getPrecip <- function(sb.url, sb.geom, sb.attribute, startDate, endDate, sb.attribute_values = NULL){
    
    stencil <- geoknife::webgeom(url=sb.url,
                                 geom = sb.geom,
                                 attribute = sb.attribute,
                                 values = sb.attribute_values)
    
    fabric <- webdata(url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined', 
                      variables = "Total_precipitation_surface_1_Hour_Accumulation", 
                      times = c(startDate, endDate))
    
    job <- geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
    check(job)
    precipData <- result(job, with.units=TRUE) %>% 
      select(-variable, -statistic, -units) %>% 
      gather(key = fips, value = precipVal, -DateTime) 
    
    return(precipData)
  }
  
  library(dplyr)
  library(geoknife)
  library(tidyr)
  startDate <- as.POSIXct(paste(viz[["start.date"]],"12:00:00"), tz="America/New_York")
  endDate <- as.POSIXct(paste(viz[["end.date"]],"22:00:00"), tz="America/New_York")
  attr(startDate, 'tzone') <- "UTC"
  
  sb.url <- viz[["sb.url"]]
  sb.geom <- viz[["sb.geom"]]
  sb.attribute <- viz[["sb.attribute"]]
  
  counties <- names(readDepends(viz)[['counties']])
  
  counties_fips <- maps::county.fips 

  split_n_drop <- function(x) strsplit(x, ":")[[1]][1]
  
  counties_fips$polyname <- as.character(sapply(counties_fips$polyname, split_n_drop))

  counties_fips <- counties_fips %>%
    dplyr::filter(polyname %in% counties) %>% 
    mutate(fips = sprintf('%05d', fips))# fips need 5 digits to join w/ geoknife result
  
  sb.attribute_values <- unique(counties_fips$fips)
  
  precip <- getPrecip(sb.url, sb.geom, sb.attribute, startDate, endDate, sb.attribute_values = sb.attribute_values)
  
  attr(precip$DateTime, 'tzone') <- "America/New_York" #back to eastern
  location <- viz[['location']]
  write.csv(precip, file=location, row.names = FALSE)
}



