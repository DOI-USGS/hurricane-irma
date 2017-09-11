process.precip_classify <- function(viz = as.viz('precip-classify')){
  library(dplyr)
  
  #need to get bins
  deps <- readDepends(viz)
  precip_breaks <- deps[['precip-breaks']] 
  precipData <- deps[['precip-data']] 
  fips.data <- maps::county.fips
  fips.data$fips <- as.character(fips.data$fips)
  fips.data$fips <- ifelse(nchar(fips.data$fips) == 4, paste0("0", fips.data$fips), fips.data$fips)
  precipData$precipVal <- precipData$precipVal/25.4 #convert mm to inches
  
  precipData <- precipData %>% mutate(cols = cut(precipVal, breaks = precip_breaks, labels = FALSE)) %>% 
    mutate(cols = ifelse(precipVal > tail(precip_breaks,1), length(precip_breaks), cols)) %>% 
    mutate(cols = ifelse(is.na(cols), 1, cols), cols = as.character(cols)) %>% select(fips, DateTime, cols) %>% left_join(fips.data)
    
  polynames <- unique(precipData$polyname)
  data.out <- data.frame(polyname = polynames, class = NA_character_, stringsAsFactors = FALSE)
  
  for (i in 1:length(polynames)){
    polyname <- polynames[i]
    fip.data <- precipData[precipData$polyname == polyname, ] %>% arrange(DateTime)
    changed <- which(as.logical(c(TRUE, diff(as.numeric(fip.data$cols)))))
    data.out$class[i] <- paste(' p-', changed, '-', fip.data$cols[changed], sep = '', collapse = '')
  }
  #want to cut down precipData to only relevant info?
  saveRDS(object = data.out, file = viz[['location']])
}

process.precip_breaks <- function(viz = as.viz("precip-breaks")){
  colSteps <- readDepends(viz)[['precip-colors']] #vector of actual color palette codes
  precip_breaks <- seq(from = 0, to = viz[['stepSize']] * (length(colSteps) - 1), length.out =length(colSteps))
  saveRDS(object = precip_breaks, file = viz[['location']])
}
