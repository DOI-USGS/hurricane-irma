process.precip_classify <- function(viz = as.viz('precip-classify')){
  library(dplyr)
  
  #need to get bins
  deps <- readDepends(viz)
  precip_breaks <- deps[['precip-breaks']] 
  precipData <- deps[['precip-data']] 

  precipData$precipVal <- precipData$precipVal/25.4 #convert mm to inches
  
  precipData <- precipData %>% mutate(cols = cut(precipVal, breaks = precip_breaks, labels = FALSE)) %>% 
    mutate(cols = ifelse(precipVal > tail(precip_breaks,1), length(precip_breaks), cols)) %>% 
    mutate(cols = ifelse(is.na(cols), 1, cols), cols = as.character(cols)) %>% select(fips, DateTime, cols)
    
  #want to cut down precipData to only relevant info?
  
  saveRDS(object = precipData, file = viz[['location']])
}

process.precip_breaks <- function(viz = as.viz("precip-breaks")){
  colSteps <- readDepends(viz)[['precip-colors']] #vector of actual color palette codes
  precip_breaks <- seq(0, viz[['stepSize']], length.out =length(colSteps))
  saveRDS(object = precip_breaks, file = viz[['location']])
}
