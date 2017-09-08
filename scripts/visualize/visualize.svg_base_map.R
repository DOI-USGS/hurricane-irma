

visualize.svg_base_map <- function(viz = as.viz('base-map')){
  
  depends <- readDepends(viz)
  
  # not good to assume order here, but: 
  view.limits <- depends[[1L]]
  geoms <- depends[-1L]
  g.ids <- viz[['depends']][-1L]
  view.limits$proj.string <- NULL
  
  # 1) set up shell svg, w/ proper size and aspect
  svg <- init.svg(width = view.limits$width, height = view.limits$height)
  
  # 2) add basic groups etc, including <defs><g id="template-geoms"/></defs> and <g id="styled-geoms"/>
  # 3) read in depends geoms
  # 4) loop through depends, trim, then add geoms to <use/> elements (in id="template-geoms"), with id="u-{id}"
  for (g in geoms){
    g.node <- xml2::xml_add_child(svg, do.call(get_svg_geoms, append(list(sp = g), view.limits)))
    xml2::xml_attr(g.node, 'id') <- g.ids[1L]
    g.ids <- tail(g.ids, -1L)
  }
  
  xml2::write_xml(x = svg, viz[['location']])
  # 6) create geoms to mirror ids in <use/> elements, add attributes
}