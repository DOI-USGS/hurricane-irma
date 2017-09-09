#' function for placing add-ons to the svg base map
#' 
visualize.hurricane_map <- function(viz = as.viz('hurricane-map')){
  library(xml2)
  svg <- readDepends(viz)[[1]]
  
  xml_attr(svg, "id") <- viz[['id']]
  
  vb <- strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]]
  
  xml_add_sibling(xml_children(svg)[[1]], 'rect', .where='before', width=vb[3], height=vb[4], class='ocean-water')
  xml_add_sibling(xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml_add_sibling(xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])
  
  xml_add_child(svg, 'text', ' ', id='timestamp-text', class='time-text svg-text', x="400", y="550", 'text-anchor'="middle")
  
  write_xml(svg, file = viz[['location']])
}