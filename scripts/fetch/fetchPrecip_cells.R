#fetch precip data w/geoknife
fetch.precipCellData <- function(viz = as.viz('precip-cell-data')){
  deps <- readDepends(viz)
  required <- c("location","start.date", "end.date")
  checkRequired(viz, required)
  
  
  library(dplyr)
  library(geoknife)
  library(tidyr)
  startDate <- as.POSIXct(paste(viz[["start.date"]]), tz="America/New_York")
  endDate <- as.POSIXct(paste(viz[["end.date"]]), tz="America/New_York")
  attr(startDate, 'tzone') <- "UTC"
  
  sb.url <- viz[["sb.url"]]
  sb.geom <- viz[["sb.geom"]]
  sb.attribute <- viz[["sb.attribute"]]
  
  stencil <- geoknife::simplegeom(deps$`precip-spatial`$cells)
  
  fabric <- geoknife::webdata(url = 'https://cida.usgs.gov/thredds/dodsC/stageiv_combined', 
                              variables = "Total_precipitation_surface_1_Hour_Accumulation", 
                              times = c(startDate, endDate))
  
  job <- geoknife::geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
  geoknife::check(job)
  precip <- geoknife::result(job, with.units=TRUE) %>% 
    dplyr::select(-variable, -statistic, -units) %>% 
    tidyr::gather(key = id, value = precipVal, -DateTime) 
  
  
  attr(precip$DateTime, 'tzone') <- "America/New_York" #back to eastern
  
  location <- viz[['location']]
  write.csv(precip, file=location, row.names = FALSE)
}



