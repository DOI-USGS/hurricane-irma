process.precip_colors <- function(viz = as.viz('precip-colors')){
  cols <- RColorBrewer::brewer.pal(viz[['bins']], viz[['pallete']])
  # writing a comment in here to force Jenkins to rebuild this.  
  # the change I made was to the yaml params, which aren't recognized as a change by make. 
  cat(jsonlite::toJSON(cols), file = viz[['location']])
}
