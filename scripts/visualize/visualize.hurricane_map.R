#' function for placing add-ons to the svg base map
#' 
visualize_hurricane_map <- function(viz, height, width, mode, ...){
  library(xml2)
  
  depends <- readDepends(viz)
  checkRequired(depends, c("base-map",'discharge-sparks', 'precip-colors', "precip-breaks", "flood-sparks"))
  svg <- depends[["base-map"]]
  sparks <- depends[["discharge-sparks"]]
  fl.sparks <- depends[["flood-sparks"]]
  color.meta <- getContentInfo('precip-colors')
  break.meta <- getContentInfo('precip-breaks')
  xml_attr(svg, "id") <- viz[['id']]
  
  # get the big dog that has all the stuff that is geo:
  map.elements <- xml2::xml_find_first(svg, "//*[local-name()='g'][@id='map-elements']") 
  precip.centroids <- xml2::xml_find_first(svg, "//*[local-name()='g'][@id='precip-centroids']") 
  xml_attr(precip.centroids, "clip-path") <- "url(#state-clip)"
  precip.dots <- xml_children(precip.centroids)
  xs <- sort(as.numeric(sapply(precip.dots, xml_attr, attr = 'cx')))
  diameter <- unique(sort(round(diff(xs))))[2]
  for (dot in precip.dots){
    xml_name(dot) <- 'path'
    x <- as.numeric(xml_attr(dot, 'cx')) - diameter/2
    y <- as.numeric(xml_attr(dot, 'cy')) - diameter/2
    xml_attr(dot, 'd') <- sprintf("M%1.1f,%1.1f h%s v%s h-%sZ", x, y, diameter, diameter, diameter)
    xml_attr(dot, 'r') <- NULL
    xml_attr(dot, 'cx') <- NULL
    xml_attr(dot, 'cy') <- NULL
  }
  
  xml2::xml_attr(map.elements, 'id') <- paste(xml2::xml_attr(map.elements, 'id'), sep = '-', mode)

  
  side.panel <- 145
  xml_attr(svg, 'viewBox') <- sprintf("0 0 %s %s", width, height)
  vb <- strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]]
  
  non.geo <- xml_add_sibling(xml_children(svg)[[1]], 'g', 'id' = 'non-geo', .where='before')
  
  
  xml_add_sibling(xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml_add_sibling(xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])
  
  # overlays 
  g.overlays <- xml_add_child(map.elements, 'g', id = 'map-overlays')
  xml_add_child(g.overlays, 'text', "Atlantic Ocean", class=sprintf('svg-text viz-pause ocean-name-%s',mode), id="atlantic-ocean", transform="translate(280,290)")
  xml_add_child(g.overlays, 'text', "Gulf of Mexico", class=sprintf('svg-text viz-pause ocean-name-%s',mode), id="gulf-of-mexico", transform="translate(20,343)")
  xml_add_child(g.overlays, 'text', toupper("Florida"), class='svg-text state-name', id="florida", transform="translate(80,282)")
  xml_add_child(g.overlays, 'text', toupper("Georgia"), class='svg-text state-name', id="georgia", transform="translate(110,210)")
  xml_add_child(g.overlays, 'text', toupper("Alabama"), class='svg-text state-name', id="alabama", transform="translate(18,190)")
  xml_add_child(g.overlays, 'text', toupper("South Carolina"), class='svg-text state-name', id="south-carolina", transform="translate(155,150)")
  non.geo.top <- xml_add_child(svg, 'g', 'id' = 'non-geo-top')
  
  
  xml_add_child(non.geo, 'rect', width="100%", height="100%", class='ocean-water viz-pause', id='ocean-background')
  g.rain <- xml_add_child(non.geo.top, 'g', id='legend', transform=sprintf("translate(10,%s)", as.numeric(vb[4])-50))
  g.backing <- xml_add_child(non.geo, 'g', id='legend0-back', transform=sprintf("translate(10,%s)", as.numeric(vb[4])-50))
  
  # lower left legend:
  xml_add_child(g.rain, 'text', "Total rainfall amount (inches)", class='svg-text legend-text', dy="-1em",
                transform="translate(0,35)")
  g.rains <- xml_add_child(g.rain, 'g', id = 'rain-legend')
  g.irma <- xml_add_child(g.rain, 'g', id = 'irma-legend', transform="translate(15,-65)")
  g.gage_isFlood <- xml_add_child(g.rain, 'g', id = 'gage-legend', transform="translate(15,-10)")
  g.gage_predFlood <- xml_add_child(g.rain, 'g', id = 'gage-legend', transform="translate(15,-25)")
  g.rains.bn <- xml_add_child(g.rains, 'g', id = 'rain-legend-bin', transform="translate(0,35)")
  g.rains.tx <- xml_add_child(g.rains, 'g', id = 'rain-legend-text', transform="translate(0,35)")
  xml_add_child(g.irma, 'circle', r="8", class="storm-dot-legend")
  xml_add_child(g.gage_isFlood, 'circle', r="4", class="nwis-flooding-legend")
  xml_add_child(g.irma, 'text', "Hurricane Irma", class='svg-text legend-text', dx='20', dy="0.33em")
  xml_add_child(g.gage_isFlood, 'text', "Above flood stage", class='svg-text legend-text', dx='20', dy="0.33em")
  xml_add_child(g.gage_predFlood, 'circle', r="4", class="nwis-dot-legend")
  xml_add_child(g.gage_predFlood, 'text', "Below flood stage", class='svg-text legend-text', dx='20', dy="0.33em")
  
  g.main_gage_text <- xml_add_child(g.rain, 'text', "USGS Stream Gages", class='svg-text legend-text', dy="-1em",
                                    transform="translate(0,-23)")
  g.main_gage_text <- xml_add_child(g.rain, 'text', "(< 1% of US total)", class='svg-text smallprint-text legend-text', dy="-1.33em",
                                    transform="translate(135,-23)")
  
  
  rain.w <- 28 # width of a rain legend bin
  rain.h <- 14
  x0 <- 0
  n.bins <- color.meta$bins
  col.breaks <- seq(0, length.out = color.meta$bins, by = break.meta$stepSize)
  col.rng <- paste(head(col.breaks, -1L), tail(col.breaks, -1L), sep='-')
  col.txt <- c(col.rng, sprintf('%s+', tail(col.breaks, 1)))
  cols <- RColorBrewer::brewer.pal(n.bins, color.meta$pallete)
  for (i in 1:n.bins){
    text.class <- ifelse(any(col2rgb(cols[i]) < 100), 'svg-text light-rain-legend', 'svg-text dark-rain-legend')
    xml_add_child(g.rains.bn, 'rect', x=as.character(x0), y="-10", 
                  height = as.character(rain.h), width = as.character(rain.w), 
                  class='rain-box', style=sprintf("fill:%s;",cols[i]))
    if (i == 1 | i == n.bins){
      # only do the extreme values
      xml_add_child(g.rains.tx, 'text', col.txt[i], class = text.class, x= as.character(x0+rain.w/2), 'text-anchor'='middle')  
    }
    
    x0 <- x0+rain.w
  }
  
  d <- xml_add_child(svg, 'defs', .where='before') 
  
  # sparkline container:
  g.spark <- xml_add_child(non.geo.top, 'g', id = 'sparkline-container', transform=sprintf('translate(%s,0)', as.numeric(vb[3])-side.panel))
  xml_add_child(g.spark, 'rect', width = as.character(side.panel), height='100%', class='legend-box')
  # sparklines within container:
  g.sparkle.blck <- xml_add_child(g.spark, 'g', id = sprintf('sparkline-squiggle-block-%s', mode))
  xml_add_child(g.sparkle.blck, 'text', x=as.character(side.panel/2), 'Featured USGS gages', dy="1.5em", 'text-anchor'='middle', class='svg-text legend-text')
  xml_add_child(g.sparkle.blck, 'text', x=as.character(side.panel/2), '(normalized stage)', dy='3em', 'text-anchor'='middle', class='svg-text smallprint-text legend-text')
  g.sparkles <- xml_add_child(g.sparkle.blck, 'g', id = sprintf('sparkline-squiggles-%s', mode))
  
  ys <- seq(45, height - 120, length.out = nrow(sparks))
  
  for (i in 1:nrow(sparks)){ 
    g.single <- xml_add_child(g.sparkles, 'g', transform=sprintf('translate(0,%s)', ys[i])) 
    do.call(xml_add_child, append(list(.x = g.single, .value = 'polyline'), sparks[i, ]))
    fl.spark <- fl.sparks[i,]
    cp <- xml_add_child(d, "clipPath", id=sprintf("flood-clip-%s", strsplit(fl.spark$id, '[-]')[[1]][2]))
    xml_add_child(cp, 'rect', width ='100%', height = fl.spark$y, y = "0")
    fl.spark$y <- NULL
    fl.spark$points <- sparks[i, ]$points
    do.call(xml_add_child, append(list(.x = g.single, .value = 'polyline'), fl.spark))
    # now add flood spark
  }
  
  xml_add_child(g.sparkle.blck, 'text', ' ', id='timestamp-text', class='time-text svg-text legend-text', 
                y=as.character(ys[i]+50), x = as.character(side.panel/2), 'text-anchor'='middle')

  map.elements.mid <- xml_add_child(svg, 'g', id=sprintf('map-elements-%s-mid', mode))
  g.tool <- xml_add_child(svg,'g',id='tooltip-group')
  map.elements.top <- xml_add_child(svg, 'g', id=sprintf('map-elements-%s-top', mode))
  
  xml_add_child(g.tool, 'rect', id="tooltip-box", height="24", class="tooltip-box")
  xml_add_child(g.tool, 'path', id="tooltip-point", d="M-6,-11 l6,10 l6,-11", class="tooltip-box")
  xml_add_child(g.tool, 'text', id="tooltip-text", dy="-1.1em", 'text-anchor'="middle", class="tooltip-text-label svg-text", " ")
  
  g.watermark <- xml_add_child(non.geo.top, 'g', id='usgs-watermark',
                               transform=sprintf('translate(%s,%s)scale(0.20)', 
                                                 as.character(as.numeric(vb[3])-110), 
                                                 as.character(as.numeric(vb[4])-40)))
  usgs.d="m234.95 15.44v85.037c0 17.938-10.132 36.871-40.691 36.871-27.569 0-40.859-14.281-40.859-36.871v-85.04h25.08v83.377c0 14.783 6.311 20.593 15.447 20.593 10.959 0 15.943-7.307 15.943-20.593v-83.377h25.08m40.79 121.91c-31.058 0-36.871-18.27-35.542-39.03h25.078c0 11.462 0.5 21.092 14.282 21.092 8.472 0 12.62-5.482 12.62-13.618 0-21.592-50.486-22.922-50.486-58.631 0-18.769 8.968-33.715 39.525-33.715 24.42 0 36.543 10.963 34.883 36.043h-24.419c0-8.974-1.492-18.106-11.627-18.106-8.136 0-12.953 4.486-12.953 12.787 0 22.757 50.493 20.763 50.493 58.465 0 31.06-22.75 34.72-41.85 34.72m168.6 0c-31.06 0-36.871-18.27-35.539-39.03h25.075c0 11.462 0.502 21.092 14.285 21.092 8.475 0 12.625-5.482 12.625-13.618 0-21.592-50.494-22.922-50.494-58.631 0-18.769 8.969-33.715 39.531-33.715 24.412 0 36.536 10.963 34.875 36.043h-24.412c0-8.974-1.494-18.106-11.625-18.106-8.144 0-12.955 4.486-12.955 12.787 0 22.757 50.486 20.763 50.486 58.465 0 31.06-22.75 34.72-41.85 34.72m-79.89-46.684h14.76v26.461l-1.229 0.454c-3.816 1.332-8.301 2.327-12.453 2.327-14.287 0-17.943-6.645-17.943-44.177 0-23.256 0-44.348 15.615-44.348 12.146 0 14.711 8.198 14.933 18.107h24.981c0.198-23.271-14.789-36.043-38.42-36.043-41.021 0-42.52 30.724-42.52 60.954 0 45.507 4.938 63.167 47.12 63.167 9.784 0 25.36-2.211 32.554-4.18 0.436-0.115 1.212-0.596 1.212-1.216v-59.598h-38.612v18.09"
  wave.d="m48.736 55.595l0.419 0.403c11.752 9.844 24.431 8.886 34.092 2.464 6.088-4.049 33.633-22.367 49.202-32.718v-10.344h-116.03v27.309c7.071-1.224 18.47-0.022 32.316 12.886m43.651 45.425l-13.705-13.142c-1.926-1.753-3.571-3.04-3.927-3.313-11.204-7.867-21.646-5.476-26.149-3.802-1.362 0.544-2.665 1.287-3.586 1.869l-28.602 19.13v34.666h116.03v-24.95c-2.55 1.62-18.27 10.12-40.063-10.46m-44.677-42.322c-0.619-0.578-1.304-1.194-1.915-1.698-13.702-10.6-26.646-5.409-29.376-4.116v11.931l6.714-4.523s10.346-7.674 26.446 0.195l-1.869-1.789m16.028 15.409c-0.603-0.534-1.214-1.083-1.823-1.664-12.157-10.285-23.908-7.67-28.781-5.864-1.382 0.554-2.7 1.303-3.629 1.887l-13.086 8.754v12.288l21.888-14.748s10.228-7.589 26.166 0.054l-0.735-0.707m68.722 12.865c-4.563 3.078-9.203 6.203-11.048 7.441-4.128 2.765-13.678 9.614-29.577 2.015l1.869 1.797c0.699 0.63 1.554 1.362 2.481 2.077 11.418 8.53 23.62 7.303 32.769 1.243 1.267-0.838 2.424-1.609 3.507-2.334v-12.234m0-24.61c-10.02 6.738-23.546 15.833-26.085 17.536-4.127 2.765-13.82 9.708-29.379 2.273l1.804 1.729c0.205 0.19 0.409 0.375 0.612 0.571l-0.01 0.01 0.01-0.01c12.079 10.22 25.379 8.657 34.501 2.563 5.146-3.436 12.461-8.38 18.548-12.507l-0.01-12.165m0-24.481c-14.452 9.682-38.162 25.568-41.031 27.493-4.162 2.789-13.974 9.836-29.335 2.5l1.864 1.796c1.111 1.004 2.605 2.259 4.192 3.295 10.632 6.792 21.759 5.591 30.817-0.455 6.512-4.351 22.528-14.998 33.493-22.285v-12.344"
  xml_add_child(g.watermark,'path', d=usgs.d, onclick="vizlab.clicklink('https://www2.usgs.gov/water/')", 'class'='watermark')
  xml_add_child(g.watermark,'path', d=wave.d, onclick="vizlab.clicklink('https://www2.usgs.gov/water/')", 'class'='watermark')
  
  # sparkline masks:
  
  m = xml_add_child(d, 'mask', id="spark-opacity", x="0", y="-1", width="1", height="3", maskContentUnits="objectBoundingBox")
  xml_add_child(m, 'rect', x="0", y="-1", width="1", height="3", style="fill-opacity: 0.18; fill: white;", id='spark-light-mask')
  xml_add_child(m, 'rect', x="0", y="-1", width="0", height="3", style="fill-opacity: 1; fill: white;", id='spark-full-mask')

  # clips for pixel-based precip!
  cp <- xml_add_child(d, 'clipPath', id="state-clip")
  storm.states <- xml_attr(xml_children(xml_find_first(svg, "//*[local-name()='g'][@id='storm-states']") ), 'id')
  .jnk <- lapply(storm.states, function(x) xml_add_child(cp, 'use', 'xlink:href'=sprintf("#%s", x)))

  m = xml_add_child(d, 'mask', id="flood-opacity", x="0", y="-1", width="1", height="3", maskContentUnits="objectBoundingBox")
  xml_add_child(m, 'rect', x="0", y="-1", width="1", height="3", style="fill-opacity: 0; fill: white;", id='flood-light-mask')
  xml_add_child(m, 'rect', x="0", y="-1", width="0", height="3", style="fill-opacity: 1; fill: white;", id='flood-full-mask')
  
  
  xml_add_child(map.elements.mid, 'use', "xlink:href"="#storm-counties", class='county-borders-overlay')
  xml_add_child(map.elements.mid, 'use', "xlink:href"="#storm-states", class='state-borders-overlay')
  xml_add_child(map.elements.mid, 'use', "xlink:href"="#storm-sites", class='county-borders-overlay')
  xml_add_child(map.elements.mid, 'use', "xlink:href"="#storm-location")
  # tops are only things we want mouseovers on:
  xml_add_child(map.elements.top, 'use', "xlink:href" = "#storm-counties", class='county-borders-mousers')
  g.mouse <- xml_add_child(map.elements.top, 'g', id = 'gage-mousers')
  
  # now replicate and move the gages up to the top, but make their mousers invisible:
  storm.sites <- xml2::xml_find_all(svg, "//*[local-name()='circle'][contains(./@class,'nwis-dot')]")
  lapply(storm.sites, xml_add_child, .x = g.mouse)
  mouse.kids <- xml_children(g.mouse)
  IDs <- xml_attr(mouse.kids, 'id')
  IDs <- sapply(IDs, function(x) strsplit(x, '[-]')[[1]][2], USE.NAMES = FALSE)
  for (j in 1:length(IDs)){
    xml_attr(mouse.kids[j], 'id') <- paste('mouse', IDs[j], sep='-')
  }
  xml_attr(mouse.kids, "class") <- 'mouser-nwis'
  xml_attr(mouse.kids, "xmlns") <- NULL
  
  xml_attr(storm.sites, "onmousemove") <- NULL
  xml_attr(storm.sites, "onmouseout") <- NULL
  xml_attr(storm.sites, "onmouseover") <- NULL
  xml_attr(storm.sites, "onclick") <- NULL
  return(svg)
}


visualize.hurricane_map_portrait <- function(viz = as.viz('hurricane-map-portrait')){
  height <- viz[['height']]
  width <- viz[['width']]
  svg <- visualize_hurricane_map(viz, height = height, width = width, mode =  'portrait')
  
  # find and remove
  to.rm <- xml2::xml_find_all(svg, "//*[local-name()='g'][@id='storm-islands']") 
  xml_remove(to.rm)
  to.rm <- xml2::xml_find_all(svg, "//*[local-name()='circle'][@class='inactive-dot']") 
  xml_remove(to.rm)
  
  to.rm <- xml2::xml_find_all(svg, "//*[local-name()='text'][@id='alabama']") 
  xml_remove(to.rm)
  
  
  # NOT WORKING???
  to.rm <- xml2::xml_find_all(svg, "//*[local-name()='g'][@id='storm-counties']") 
  xml_remove(to.rm)
  to.rm <- xml2::xml_find_all(svg, "//*[local-name()='use'][@class='county-borders-mousers']") 
  xml_remove(to.rm)
  to.rm <- xml2::xml_find_all(svg, "//*[local-name()='use'][@class='county-borders-overlay']") 
  xml_remove(to.rm)
  # move some things:
  
  to.mv <- xml2::xml_find_all(svg, "//*[local-name()='text'][@id='atlantic-ocean']")
  xml_attr(to.mv, 'transform') <- "translate(210,270)"
  to.mv <- xml2::xml_find_all(svg, "//*[local-name()='text'][@id='gulf-of-mexico']")
  xml_attr(to.mv, 'transform') <- "translate(60,400)"
  
  write_xml(svg, file = viz[['location']])
  
}

visualize.hurricane_map_landscape <- function(viz = as.viz('hurricane-map-landscape')){
  height <- viz[['height']]
  width <- viz[['width']]
  svg <- visualize_hurricane_map(viz, height = height, width = width, mode =  'landscape')
  
  to.rm <- xml2::xml_find_all(svg, "//*[local-name()='circle'][@class='inactive-dot']") 
  xml_remove(to.rm)
  
  
  write_xml(svg, file = viz[['location']])
}
