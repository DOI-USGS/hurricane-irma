readData.svg_map <- function(viz){
  svg <- xml2::read_xml(viz[['location']])
  return(svg)
}