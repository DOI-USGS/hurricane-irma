#fetch precip data w/geoknife
fetch.precipCellData <- function(viz = as.viz('precip-cell-data')){
  if(!viz[['refetch']]){
    return(NULL)
  }
  required <- c("location","start.date", "end.date", 
                "sb.url", "sb.geom", "sb.attribute")
  checkRequired(viz, required)
  
  getPrecip <- function(sb.url, sb.geom, sb.attribute, startDate, endDate){
    
    stencil <- geoknife::webgeom(url=sb.url,
                                 geom = sb.geom,
                                 attribute = sb.attribute)
    
    fabric <- geoknife::webdata(url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined', 
                                variables = "Total_precipitation_surface_1_Hour_Accumulation", 
                                times = c(startDate, endDate))
    
    job <- geoknife::geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
    geoknife::check(job)
    precipData <- geoknife::result(job, with.units=TRUE) %>% 
      dplyr::select(-variable, -statistic, -units) %>% 
      tidyr::gather(key = id, value = precipVal, -DateTime) 
    
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
  
  precip <- getPrecip(sb.url, sb.geom, sb.attribute, startDate, endDate)
  attr(precip$DateTime, 'tzone') <- "America/New_York" #back to eastern
  
  location <- viz[['location']]
  write.csv(precip, file=location, row.names = FALSE)
}



