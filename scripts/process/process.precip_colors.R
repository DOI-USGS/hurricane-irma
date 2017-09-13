process.precip_colors <- function(viz = as.viz('precip-colors')){
  cols <- RColorBrewer::brewer.pal(viz[['bins']], viz[['pallete']])
  cat(jsonlite::toJSON(cols), file = viz[['location']])
}

process.precip_breaks <- function(viz = as.viz("precip-breaks")){
  colSteps <- readDepends(viz)[['precip-colors']] #vector of actual color palette codes, now used for cumulative
  precip_breaks <- seq(from = 0, to = viz[['stepSize']] * (length(colSteps) - 1), length.out =length(colSteps))
  saveRDS(object = precip_breaks, file = viz[['location']])
}
