process.node_precip <- function(viz = as.viz('node-precip')){
  
  deps <- readDepends(viz)
  checkRequired(deps, c("precip-spatial", "precip-cell-data", 'precip-breaks'))
  
  sp.cells <- deps[["precip-spatial"]]$points
  cell.precip <- deps[["precip-cell-data"]]
  precip.breaks <- deps[['precip-breaks']] 
  
  stopifnot(all(cell.precip$id %in% sp.cells$id))
  library(dplyr)

  precipData <- cell.precip %>% group_by(id) %>% 
    mutate(precipVal = precipVal/25.4, summ = cumsum(precipVal)) %>% #convert mm to inches, do cumulative
    select(DateTime, id, precipVal = summ) %>% 
    mutate(cols = cut(precipVal, breaks = precip.breaks, labels = FALSE)) %>% 
    mutate(cols = ifelse(precipVal > tail(precip.breaks,1), length(precip.breaks), cols)) %>% 
    mutate(cols = ifelse(is.na(cols), 1, cols), cols = as.character(cols)) %>% select(id, DateTime, cols)
  
  sp.cells$class = NA_character_
  
  
  for (i in 1:length(sp.cells$id)){
    ID <- sp.cells$id[i]
    ID.data <- precipData[precipData$id == ID, ] %>% arrange(DateTime)
    changed <- which(as.logical(c(TRUE, diff(as.numeric(ID.data$cols)))))
    sp.cells$class[i] <- paste(' p-', changed, '-', ID.data$cols[changed], sep = '', collapse = '')
  }
  #want to cut down precipData to only relevant info?
  
  saveRDS(object = sp.cells, file = viz[['location']])
  
}
