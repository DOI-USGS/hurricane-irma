#' take map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
#' 
library(maps)
library(sp)
to_sp <- function(..., proj.string){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  return(map.sp.t)
}


fetch.states <- function(viz = as.viz('states')){
  view <- readDepends(viz)[['view-limits']]
  states.out <- to_sp('state', proj.string = view[['proj.string']])
  states.out <- rbind(states.out, to_sp('world','Mexico'))
  
  saveRDS(states.out, viz[['location']])
}

fetch.counties <- function(viz = as.viz('counties')){
  view <- readDepends(viz)[['view-limits']]
  states.out <- to_sp('county', proj.string = view[['proj.string']])
  
  saveRDS(states.out, viz[['location']])
}