#' function for placing add-ons to the svg base map
#' 
visualize.hurricane_map <- function(viz = as.viz('hurricane-map')){
  library(xml2)
  svg <- readDepends(viz)[[1]]
  
  write_xml(svg, file = viz[['location']])
}