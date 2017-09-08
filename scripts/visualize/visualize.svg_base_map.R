

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
    if ("data" %in% slotNames(g)){
      # add svg attributes based on data.frame that the sp object is carrying
      add_attrs(xml2::xml_children(g.node), data = as.data.frame(g), plot.order = g@plotOrder)
    }
    g.ids <- tail(g.ids, -1L)
  }
  
  g.tool <- xml_add_child(svg,'g',id='tooltip-group')
  xml_add_child(g.tool, 'rect', id="tooltip-box", height="24", class="tooltip-box")
  xml_add_child(g.tool, 'path', id="tooltip-point", d="M-6,-11 l6,10 l6,-11", class="tooltip-box")
  xml_add_child(g.tool, 'text', id="tooltip-text", dy="-1.1em", 'text-anchor'="middle", class="tooltip-text-label svg-text", " ")
  
  xml2::write_xml(x = svg, viz[['location']])
  # 6) create geoms to mirror ids in <use/> elements, add attributes
}

add_attrs <- function(nodes, data, plot.order){
  for (d.i in seq_len(length(nodes))){
    for (value in names(data)){
      xml2::xml_attr(nodes[d.i], value) <- data[[value]][plot.order[d.i]]
    }
  }
}