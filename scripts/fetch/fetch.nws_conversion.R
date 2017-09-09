library(readr)

fetch.nws_conversion <- function(viz = as.viz("nws-conversion")){
  states <- viz[['states']]
  
  message('to do: code this so we can give it multiple states')
  stopifnot(length(states) == 1)
  conversion.url <- sprintf("http://www.nws.noaa.gov/oh/hads/USGS/%s_USGS-HADS_SITES.txt", states)
  conversion.table <- read_delim(conversion.url,
                                 delim = "|",skip = 4,col_names = FALSE)

  names(conversion.table) <- c("NWS","USGS","GOES","NWS HSA","lat","lon","name")
  conversion.table$USGS <- gsub(" ","", conversion.table$USGS)
  
  location <- viz[['location']]
  saveRDS(conversion.table, file=location)
}