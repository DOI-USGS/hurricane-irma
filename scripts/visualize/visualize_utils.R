init.svg <- function(..., width = 10, height = 8){
  # fragile, this is coded into svglite:
  ppi <- 72
  library(xml2)
  vb <- sprintf("%s %s %s %s", 0, 0, width*ppi, height*ppi)
  xml2::xml_new_root('svg', viewBox = vb, preserveAspectRatio="xMidYMid meet", 
                     xmlns="http://www.w3.org/2000/svg", `xmlns:xlink`="http://www.w3.org/1999/xlink", version="1.1" )
}


#' calculate the bounding box of the sp object and return as `SpatialPolygons` object
#' @param sp a spatial object
#' 
#' @return a `SpatialPolygons` object that represents the bounding box of the input `sp`
get_sp_bbox <- function(sp){
  bb <- sp::bbox(sp)
  xs <- bb[c(1, 3)]
  ys <- bb[c(2, 4)]
  proj.string = sp::CRS(sp::proj4string(sp))
  return(as.sp_box(xs, ys, proj.string))
}


#' create a spatial polygon from x and y coordinates
#' 
#' @param xs a numeric vector of length two, containing the min and max of x values
#' @param ys a numeric vector of length two, containing the min and max of y values
#' 
#' @return a `SpatialPolygons` object
as.sp_box <- function(xs, ys, proj.string){
  Sr1 <- sp::Polygon(cbind(c(xs[c(1, 2, 2, 1, 1)]), c(ys[c(1, 1, 2, 2, 1)])))
  Srs1 <- sp::Polygons(list(Sr1), "s1")
  SpP <- sp::SpatialPolygons(list(Srs1), proj4string = proj.string)
  return(SpP)
}

#' extract the svg elements from an sp object
#' 
#' @param sp a spatial object
#' @param ... additional arguments passed to the plotting methods of `sp` (e.g., `expandBB`)
#' @param width the width (in inches) of the svg view
#' @param height the height (in inches) of the svg view
#' @param pointsize number of pixels per inch
#' @param xlim x limits (in sp units) of the plot
#' @param ylim y limits (in sp units) of the plot
#' 
#' @return an `xml_document`
get_svg_geoms <- function(sp, ..., width = 10, height = 8, pointsize = 12, xlim, ylim){
  
  stopifnot(packageVersion('svglite') == '1.2.0.9003')
  
  if (missing(xlim)){
    xlim <- get_sp_lims(sp, ..., return = 'xlim')
  }
  if (missing(ylim)){
    ylim <- get_sp_lims(sp, ..., return = 'ylim')
  }
  
  # clip the spatial object so that it only contains features and data that are within the plotting range:
  clipped.sp <- clip_sp(sp, xlim, ylim)
  
  rendered <- svglite::xmlSVG(width = width, height = height, pointsize = pointsize, standalone = F, {
    set_sp_plot()
    sp::plot(clipped.sp, ..., xlim = xlim, ylim = ylim)
  })
  svg.g <- xml2::xml_child(rendered)
  if (xml2::xml_length(svg.g) == length(clipped.sp) + 1){
    xml_remove(xml_child(svg.g)) # remove the <rect thing it puts in there>
  } else if (xml2::xml_length(svg.g) != length(clipped.sp)){
    stop('something wrong. Length of svg elements is different than number of features')
  }
  
  # here either strip the important attributes out and re-add them with a xml_set_attrs call, or lapply the nodeset and add attrs one by one:
  
  return(svg.g) # removing the <rect/> element...
}

#' extract the plotting limits from a spatial object, given a sized svg view
#' 
#' @param sp a spatial object
#' @param ... additional arguments passed to the plotting methods of `sp` (e.g., `expandBB`)
#' @param width the width (in inches) of the svg view
#' @param height the height (in inches) of the svg view
#' @param pointsize number of pixels per inch
#' @param return what limits to return
#' 
#' @return a list or numeric vector, depending on what is specified for `return`

get_sp_lims <- function(sp, ..., width = 10, height = 8, pointsize = 12, return = c('xlim','ylim')){
  bb <- get_sp_bbox(sp)
  # now default plot
  # extract usr, return lims from usr
  .fun <- svglite::svgstring(width = width, height = height, pointsize = pointsize, standalone = F)
  suppressWarnings(sp::plot(bb, ...)) # warning is for expandBB param, if used
  usr <- par('usr')
  dev.off()
  xlim = usr[c(1,2)]
  ylim = usr[c(3,4)]
  
  list.out <- list(xlim = xlim, ylim = ylim)
  
  if (length(return) > 1){
    return(list.out)
  } else {
    return(list.out[[return]])
  }
  
}

#' retain data and features for all polys/points within the plotting range,
#' while removing those outsite and truncating the boundaries of 
#' polygons to the bounds of the plotting range. 
#' 
#' @param sp a spatial object
#' @param xlim the x limits of the plot using the same coordinate system as `sp`
#' @param ylim the y limits of the plot using the same coordinate system as `sp`
#' @param ... additional args, e.g., `clip.fun` for `Spatial` class method (others unused)
#' 
#' @return a clipped sp object
#' @export
clip_sp <- function(sp, xlim, ylim, ...) {
  message('clip_sp is not fully tested for all sp classes')
  UseMethod(generic = 'clip_sp', object = sp)
}


clip_sp.SpatialPolygonsDataFrame <- function(sp, xlim, ylim, ...){
  
  clipped.sp <- NextMethod('clip_sp', sp, ..., clip.fun = rgeos::gIntersection)
  return(clipped.sp)
}

clip_sp.SpatialPointsDataFrame <- function(sp, xlim, ylim, ...){
  
  clipped.sp <- NextMethod('clip_sp', sp, ..., clip.fun = rgeos::gContains)
  return(clipped.sp)
}

clip_sp.Spatial <- function(sp, xlim, ylim, ..., clip.fun = rgeos::gIntersection){
  
  clip <- as.sp_box(xlim, ylim, sp::CRS(sp::proj4string(sp)))
  g.i <- rgeos::gIntersects(sp, clip, byid = T) 
  
  sp <- rgeos::gBuffer(sp, byid=TRUE, width=0) # zero buffer to avoid TopologyException
  has.data <- ("data" %in% slotNames(sp))

  out <- lapply(which(g.i), function(i) {
    g.out <- clip.fun(sp[i,], clip)
    row.names(g.out) <- row.names(sp)[i]
    return(g.out)
  })
  
  # use rbind.SpatialPolygons method to combine into a new object.
  clipped.sp <- do.call("rbind", out)
  
  if(has.data){
    data.out <- as.data.frame(sp)[g.i, ]
    row.names(data.out) <- row.names(sp)[g.i]
    clipped.sp <- as(object = clipped.sp, Class = class(sp))
    clipped.sp@data <- data.out
  }
  
  # //to do: use rgeos::gContains for points
  return(clipped.sp)
}

#' set up the basic plot par for a map
set_sp_plot <- function(){
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
}