#' take map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
#' 
library(maps)
library(mapdata)
library(sp)
to_sp <- function(..., proj.string, within = NULL){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  
  if (!is.null(within)){
    g.i <- rgeos::gIntersects(map.sp.t, within, byid = T) 
    map.sp.t <- map.sp.t[g.i]
  }
  
  return(map.sp.t)
}

fetch_map_data <- function(..., viz){
  view <- readDepends(viz)[['view-limits']]
  out <- to_sp(..., proj.string = view[['proj.string']], 
               within = as.sp_box(view$xlim, view$ylim, CRS(view[['proj.string']])))
  saveRDS(out, viz[['location']])
}

fetch.states <- function(viz = as.viz('states')){
  fetch_map_data('state', viz = viz)
}

fetch.counties <- function(viz = as.viz('counties')){
  fetch_map_data('county', viz = viz)
}

fetch.islands <- function(viz = as.viz('islands')){
  fetch_map_data('world2Hires', "(?!USA)", xlim = c(275, 300), ylim = c(16, 30), viz = viz)
}