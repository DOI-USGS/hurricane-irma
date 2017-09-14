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
  
  rendered <- svglite::xmlSVG(width = width, height = height, pointsize = pointsize, standalone = F, {
    set_sp_plot()
    for (j in seq_len(length(sp))){
      if (inherits(sp, 'SpatialPoints')){
        sp::plot(sp[j, ], ..., xlim = xlim, ylim = ylim, add = ifelse(j == 1, F, T), pch = 20)
      } else {
        sp::plot(sp[j, ], ..., xlim = xlim, ylim = ylim, add = ifelse(j == 1, F, T))
      }
      
    }
    
  })
  svg.g <- xml2::xml_child(rendered)
  if (xml2::xml_length(svg.g) == length(sp) + 1){
    xml_remove(xml_child(svg.g)) # remove the <rect thing it puts in there>
  } else if (xml2::xml_length(svg.g) != length(sp)){
    message('something might be wrong. Length of svg elements is different than number of features',
            'but ignore this warning for lines.')
    xml_remove(xml_child(svg.g))
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
  sp <- rgeos::gBuffer(sp, byid=TRUE, width=0) # zero buffer to avoid TopologyException
  clipped.sp <- NextMethod('clip_sp', sp, ..., clip.fun = rgeos::gIntersection)
  return(clipped.sp)
}

clip_sp.SpatialPointsDataFrame <- function(sp, xlim, ylim, ...){
  
  clip <- as.sp_box(xlim, ylim, sp::CRS(sp::proj4string(sp)))
  g.i <- rgeos::gContains(clip, sp, byid = TRUE)
  clipped.sp <- rgeos::gIntersection(clip, sp, byid = TRUE)
  clipped.sp <- as(object = clipped.sp, Class = class(sp))
  clipped.sp@data <- sp@data[g.i, ]
  
  return(clipped.sp)
}

clip_sp.SpatialLinesDataFrame <- function(sp, xlim, ylim, ...){
  return(sp)
}
clip_sp.SpatialLines <- function(sp, xlim, ylim, ...){
  return(sp)
}

clip_sp.Spatial <- function(sp, xlim, ylim, ..., clip.fun = rgeos::gIntersection){
  
  clip <- as.sp_box(xlim, ylim, sp::CRS(sp::proj4string(sp)))
  
  
  if (!inherits(sp, 'SpatialPoints')){
    g.i <- rgeos::gIntersects(sp, clip, byid = T) 
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
  } else {
    stop('not supported')
  }
  
  # //to do: use rgeos::gContains for points
  return(clipped.sp)
}

#' set up the basic plot par for a map
set_sp_plot <- function(){
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
}

locate_css_class <- function(css, class_nm){
  class_declare_line <- grep(class_nm, css)
  stopifnot(length(class_declare_line) > 0)
  close_classes <- grep("}", css)
  
  css_details <- c()
  for(start_class in class_declare_line){
    end_class <- close_classes[which(start_class < close_classes)[1]]
    css_details <- c(css_details, css[start_class:end_class])
  }
  
  return(css_details)
}

locate_css_class_detail <- function(css, item_nm){
  class_item_line <- grep(item_nm, css)
  stopifnot(length(class_item_line) > 0)
  strip_excess1 <- gsub(paste0("  ", item_nm, ": "), "", css[class_item_line])
  item_details <- gsub(";", "", strip_excess1)
  return(item_details)
}

#'
#' Take style and data inputs to create a saved 
#' image file of a snapshot in time.
createHurricaneSnapshot <- function(fig_height, fig_width, css, time_stamp, states, islands, 
                                    counties, precip_breaks, precip_cols, timesteps, storm, 
                                    hurricane_track, expandBB, flood_sites){
  
  library(dplyr)
  
  # styling details
  ocean_color <- locate_css_class_detail(locate_css_class(css, ".ocean-water"), "fill")
  state_css <- locate_css_class(css, ".county-polygon")
  state_color <- locate_css_class_detail(state_css, "fill")
  state_lwd <- locate_css_class_detail(state_css, "stroke-width")
  island_css <- locate_css_class(css, ".island-polygon")
  island_color <- locate_css_class_detail(island_css, "fill")
  storm_color <- locate_css_class_detail(locate_css_class(css, "storm-dot"), "fill")
  
  counties@data$col <- NA_character_
  # process precip to get county & color category together w/ time
  time_stamp <- as.POSIXct(time_stamp, tz = "America/New_York")
  time_idx <- which(timesteps == time_stamp)
  
  for (j in 1:nrow(counties@data)){
    class <- counties@data[j, ]$class
    precip.classes <- tail(strsplit(class, '[ ]')[[1]], -1L)
    precip.time.i <- as.numeric(sapply(precip.classes, FUN = function(x) {strsplit(x,'[-]')[[1]][2]}, USE.NAMES = FALSE))
    # the way we did those was to have the class change only if the value changed, so it is compressed and number of 
    # classes likely != number of timesteps (i.e., if a never changed it would be p-1-1, if changed twice, p-1-1 p-32-2 p-34-1)
    precip.col.i <- as.numeric(sapply(precip.classes, FUN = function(x) {strsplit(x,'[-]')[[1]][3]}, USE.NAMES = FALSE))
    t.diffs <- time_idx - precip.time.i
    time.i <- which(t.diffs==min(t.diffs[t.diffs>=0]))
    col.to.use <- precip_cols[precip.col.i[time.i]]
    if(length(col.to.use) == 0) {
      col.to.use <- "#5F5A5A"
      warning("Watch out!  there was a county with no precip data")
    }
    counties@data$col[j] <- col.to.use
  }
  
  par(mar=c(0,0,0,0), oma=c(0,0,0,0), bg = ocean_color, xaxs = 'i', yaxs = 'i')
  sp::plot(counties, col = NA, border = "#c6c6c6",
           expandBB = eval(parse(text = expandBB)))
  sp::plot(states, col = state_color, border = "#c6c6c6", add = TRUE)
  sp::plot(islands, add = TRUE, border = "#c6c6c6", col = island_color)
  sp::plot(counties, add = TRUE, border = "#c6c6c6", col = counties@data$col)
  sp::plot(hurricane_track, add=TRUE, col = "#5f5f5f", lwd=3)
  cols <- rep("#FFFFFF00", length(storm))
  cols[time_idx] <- storm_color
  sp::plot(storm, pch=20, cex=5, col=cols, add = TRUE)
  sp::plot(flood_sites, pch=20, col = "#c10000", add = TRUE)
}

#' script to turn the dataviz into a thumbnail
#' 
visualize.map_thumbnail <- function(viz){
  library(dplyr)
  
  css <- readLines(viz$css)
  file_location <- viz[['location']]
  time_stamp <- viz$`time-stamp`
  fig_height <- viz$`fig-height`
  fig_width <- viz$`fig-width`
  expandBB <- viz[["expandBB"]]
  
  depends <- readDepends(viz)
  states <- depends[["storm-states"]]
  islands <- depends[["storm-islands"]]
  counties <- depends[["storm-counties"]]
  precip_breaks <- depends[["precip-breaks"]]
  precip_cols <- depends[["precip-colors"]]
  timesteps <- as.POSIXct(strptime(depends[['timesteps']]$times, format = '%b %d %I:%M %p', tz = "America/New_York"))
  storm <- depends[["storm-location"]]
  hurricane_track <- depends[["storm-track"]]
  flood_sites <- depends[['flood-sites']]
  
  png(file = file_location, height = fig_height, width = fig_width)
  createHurricaneSnapshot(fig_height, fig_width, css, time_stamp, states, islands, 
                          counties, precip_breaks, precip_cols, timesteps, storm, hurricane_track,
                          expandBB, flood_sites)
  dev.off()
}

visualize.timelapse_gif <- function(viz){
  
  library(animation)
  
  css <- readLines(viz$css)
  file_location <- viz[['location']]
  fig_height <- viz$`fig-height`
  fig_width <- viz$`fig-width`
  expandBB <- viz[["expandBB"]]
  
  depends <- readDepends(viz)
  states <- depends[["storm-states"]]
  islands <- depends[["storm-islands"]]
  counties <- depends[["storm-counties"]]
  precip_breaks <- depends[["precip-breaks"]]
  precip_cols <- depends[["precip-colors"]]
  timesteps <- as.POSIXct(strptime(depends[['timesteps']]$times, format = '%b %d %I:%M %p', tz = "America/New_York"))
  storm <- depends[["storm-location"]]
  hurricane_track <- depends[["storm-track"]]
  storm_sites <- depends[['storm-sites']]
  nwis_data <- depends[['nwis-data']]
  nws_data <- depends[["nws-data"]]
  
  # needed to use ts w/ seq_along b/c times turned numeric otherwise
  ani.options(interval = 0.3)
  saveGIF( 
  for(ts in seq_along(timesteps)){
    time_stamp <- as.character(timesteps[ts])
    
    # for the current timestamp, get gages that have exceeded their flood stage
    flood_sites <- filterFloodSites(gage_data, nws_data, storm_sites, time_stamp)
    
    # create map of precip + gages above flood stage
    createHurricaneSnapshot(fig_height, fig_width, css, time_stamp, states, islands, 
                            counties, precip_breaks, precip_cols, timesteps, storm, hurricane_track,
                            expandBB, flood_sites)
    title(time_stamp, line = -2)
  }, movie.name = file_location)
  
}
