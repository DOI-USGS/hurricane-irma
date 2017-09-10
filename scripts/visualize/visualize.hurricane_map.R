#' function for placing add-ons to the svg base map
#' 
visualize.hurricane_map <- function(viz = as.viz('hurricane-map')){
  library(xml2)
  
  depends <- readDepends(viz)
  checkRequired(depends, c("base-map",'discharge-sparks'))
  svg <- depends[["base-map"]]
  sparks <- depends[["discharge-sparks"]]
  
  xml_attr(svg, "id") <- viz[['id']]
  
  vb <- strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]]
  
  xml_add_sibling(xml_children(svg)[[1]], 'rect', .where='before', width=vb[3], height=vb[4], class='ocean-water')
  xml_add_sibling(xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml_add_sibling(xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])
  
  
  # sparklines:
  g.spark <- xml_add_child(svg, 'g', transform='translate(420,115)')
  xml_add_child(g.spark, 'rect', x='-6', y = '-0.8em', width="147", height='2em', class='legend-box')
  xml_add_child(g.spark, 'text', x='67', 'Featured USGS gages', dy="0.1em", 'text-anchor'='middle', class='svg-text')
  xml_add_child(g.spark, 'text', x='67', '(normalized discharge)', dy='1.1em', 'text-anchor'='middle', class='svg-text smallprint-text')
  
  ys <- seq(20, 300, length.out = nrow(sparks))
  for (i in 1:nrow(sparks)){ 
    g.single <- xml_add_child(g.spark, 'g', transform=sprintf('translate(-5,%s)', ys[i])) 
    do.call(xml_add_child, append(list(.x = g.single, .value = 'polyline'), sparks[i, ]))
  }

  xml_add_child(svg, 'text', ' ', id='timestamp-text', class='time-text svg-text', x="400", y="550", 'text-anchor'="middle")
  
  # sparkline masks:
  d <- xml_add_child(svg, 'defs', .where='before') 
  m = xml_add_child(d, 'mask', id="spark-opacity", x="0", y="-1", width="1", height="3", maskContentUnits="objectBoundingBox")
  xml_add_child(m, 'rect', x="0", y="-1", width="1", height="3", style="fill-opacity: 0.18; fill: white;", id='spark-light-mask')
  xml_add_child(m, 'rect', x="0", y="-1", width="0", height="3", style="fill-opacity: 1; fill: white;", id='spark-full-mask')
  
  
  write_xml(svg, file = viz[['location']])
}