process.precip_colors <- function(viz = as.viz('precip-colors')){
  cols <- RColorBrewer::brewer.pal(viz[['bins']], viz[['pallete']])
  
  cat(jsonlite::toJSON(cols), file = viz[['location']])
}
