
fetch.nws_conversion <- function(viz = as.viz("nws-conversion")){
  states <- viz[['states']]
  
  conversion.table.all <- data.frame()
  for(s in states) {
    conversion.url <- sprintf("http://www.nws.noaa.gov/oh/hads/USGS/%s_USGS-HADS_SITES.txt", s)
    conversion.table.state <- readr::read_delim(conversion.url,
                                   delim = "|",skip = 4,col_names = FALSE)
    conversion.table.all <- dplyr::bind_rows(conversion.table.all, conversion.table.state)
  }
  

  names(conversion.table.all) <- c("NWS","USGS","GOES","NWS HSA","lat","lon","name")
  conversion.table.all$USGS <- gsub(" ","", conversion.table.all$USGS)
  
  location <- viz[['location']]
  saveRDS(conversion.table.all, file=location)
}

fetchTimestamp.nws_conversion <- vizlab:::fetchTimestamp.file
