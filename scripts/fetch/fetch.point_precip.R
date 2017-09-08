fetch.point_precip <- function(viz = as.viz('precip-spatial')){
  if(!viz[['refetch']]){
    return(NULL)
  }
  required <- c("location", "bbox")
  checkRequired(viz, required)
  
  ### constants
  cell_size <- .1 # degrees -- set to make grid more course or dense.
  states <- c("florida", "georgia", "alabama", "south carolina") # from maps states id list.
  ### /constants
    
  bbox <- viz[["bbox"]]
  
  x_range <- (bbox[3] - bbox[1])/cell_size
  y_range <- (bbox[4] - bbox[2])/cell_size
  
  grid_topology <- sp::GridTopology(cellcentre.offset = bbox[c(1:2)], 
                                    cellsize = c(cell_size,cell_size), 
                                    cells.dim = c(x_range, y_range))
  sp_grid <- raster::raster(sp::SpatialGrid(grid_topology, sp::CRS("+init=epsg:4326")))
  
  sp_cells <- raster::rasterToPolygons(sp_grid)
  sp_points <- sp::SpatialPoints(raster::rasterToPoints(sp_grid),sp::CRS("+init=epsg:4326"))
  
  state_boundary <- to_sp("state", proj.string = "+init=EPSG:4326")
  
  state_IDs <- sapply(slot(state_boundary, "polygons"), function(x) slot(x, "ID"))
  index <- which(state_IDs %in% states)
  index <- c()
  for(i in 1:length(state_boundary@polygons)) {
    if(state_boundary@polygons[[i]]@ID %in% states) {
      index <- c(index,i)
    }
  }
  state_boundary <- SpatialPolygonsDataFrame(state_boundary[index,], data.frame(viz=rep("viz", length(index)), 
                                                                                stringsAsFactors = F, row.names = states))
  state_boundary <- maptools::unionSpatialPolygons(state_boundary, state_boundary@data$viz)
  
  sp_cells@proj4string <- CRS("+init=epsg:4326")
  state_boundary@proj4string <- CRS("+init=epsg:4326")
  sp_cells <- gIntersection(state_boundary, sp_cells, byid = T, drop_lower_td = T)
  IDs <- sapply(slot(sp_cells, "polygons"), function(x) slot(x, "ID"))
  sp_cells <- SpatialPolygonsDataFrame(sp_cells, data.frame(id=c(1:length(sp_cells)), row.names = IDs))
  sp_point_selector <- sp::over(sp_points, sp_cells, fn=NULL)
  sp_points <- sp_points[which(!is.na(sp_point_selector))]
  
  sp_point_ids <- sp::over(sp_points, sp_cells, returnList = F, fn=NULL)
  
  sp_points <- SpatialPointsDataFrame(sp_points, sp_point_ids)
  
  location <- viz[['location']]
  
  rgdal::wrtieOGR(sp_points, paste0(location,"/irma_points.shp"), 
                  layer = "irma_points", driver = "ESRI Shapefile", overwrite_layer=TRUE)
  rgdal::writeOGR(sp_cells, paste0(location,"/irma_cells.shp"), 
                  layer = "irma_cells", driver = "ESRI Shapefile", overwrite_layer=TRUE)
}




