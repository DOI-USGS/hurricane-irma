process.timesteps <- function(viz = as.viz('timesteps')){
  #"classifyBins",'storm-location'
  library(dplyr)
  times <- readDepends(viz)[['precip-classify']] %>% select(DateTime) %>% 
    unique() %>% .$DateTime %>% as.POSIXct %>% format('%b %d %I:%M %p')
  cat(jsonlite::toJSON(list(times=times)), file = viz[['location']])
}