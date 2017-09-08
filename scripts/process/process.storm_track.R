
process.storm_track <- function(viz = as.viz('storm-track')){
  library(rgeos)
  library(sp)
  depends <- readDepends(viz)
  view.lims <- depends[['view-limits']]
  track <- depends[[2]]

  track <- spTransform(track, sp::CRS(view.lims$proj.string))
  
  # here do "over" analysis for masking?
  
  saveRDS(track, viz[['location']])
}

