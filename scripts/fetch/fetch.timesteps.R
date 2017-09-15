fetch.timesteps <- function(viz = as.viz('timesteps')){
  #"classifyBins",'storm-location'
  library(dplyr)
  times <- readDepends(viz)[[1L]] %>% select(DateTime) %>% 
    unique() %>% .$DateTime %>% as.POSIXct %>% format('%b %d %I:%M %p')
  cat(jsonlite::toJSON(list(times=times)), file = viz[['location']])
}