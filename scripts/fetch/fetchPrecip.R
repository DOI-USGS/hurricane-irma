#fetch precip data w/geoknife
fetch.precip <- function(viz){
  
  required <- c("location","start.date", "end.date")
  checkRequired(viz, required)
  
  getPrecip <- function(states, startDate, endDate){
    
    counties_fips <- maps::county.fips %>% 
      mutate(statecounty=as.character(polyname)) %>% # character to split into state & county
      tidyr::separate(polyname, c('statename', 'county'), ',') %>%
      mutate(fips = sprintf('%05d', fips)) %>% # fips need 5 digits to join w/ geoknife result
      filter(statename %in% states) 
    
    stencil <- webgeom(geom = 'derivative:US_Counties',
                       attribute = 'FIPS',
                       values = unique(counties_fips$fips))
    
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
  
  states <- viz[['states']]
  
  precip <- getPrecip(states, startDate, endDate)
  attr(precip$DateTime, 'tzone') <- "America/New_York" #back to eastern
  location <- viz[['location']]
  write.csv(precip, file=location, row.names = FALSE)
}



