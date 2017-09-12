fetch.precipSpatial <- function(viz = as.viz('precip-spatial')){

  if(!viz[['refetch']]){
    return(NULL)
  }
  
  checkRequired(viz, "cell_size")
  
  view <- readDepends(viz)[["view-limits"]]
  storm_area <- readDepends(viz)[["storm-area-filter"]]
  states <- readData("states")
  cell_size <- viz[["cell_size"]] # meters -- set to make grid more course or dense.
  
  bbox <- sp::spTransform(storm_area,view$proj.string)
  proj.string <- view[["proj.string"]]
  proj <- sp::CRS(proj.string)
  
  x_range <- (xlim[2] - xlim[1])/cell_size
  y_range <- (ylim[2] - ylim[1])/cell_size
  
  grid_topology <- sp::GridTopology(c(xlim[1],ylim[1]),
                                    cellsize = c(cell_size,cell_size),
                                    cells.dim = c(x_range, y_range))
  sp_grid <- raster::raster(sp::SpatialGrid(grid_topology, proj))
  
  sp_cells <- raster::rasterToPolygons(sp_grid)
  sp_points <- sp::SpatialPoints(raster::rasterToPoints(sp_grid),proj)
  
  stateIDs <- sapply(slot(states, "polygons"), function(x) slot(x, "ID"))
  
  state_boundary <- SpatialPolygonsDataFrame(state_boundary[index,], data.frame(viz=rep("viz", length(index)), 
                                                                                stringsAsFactors = F, row.names = states))
  state_boundary <- maptools::unionSpatialPolygons(state_boundary, state_boundary@data$viz)
  
  sp_cells <- rgeos::gIntersection(state_boundary, sp_cells, byid = T, drop_lower_td = T)
  
  IDs <- sapply(slot(sp_cells, "polygons"), function(x) slot(x, "ID"))
  sp_cells <- SpatialPolygonsDataFrame(sp_cells, data.frame(id=c(1:length(sp_cells)), row.names = IDs))
  
  sp_point_selector <- sp::over(sp_points, sp_cells, fn=NULL)
  
  sp_points <- sp_points[which(!is.na(sp_point_selector))]
  
  sp_point_ids <- sp::over(sp_points, sp_cells, returnList = F, fn=NULL)
  
  sp_points <- SpatialPointsDataFrame(sp_points, sp_point_ids)
  
  saveRDS(sp_cells, viz[['location']])
  
  # Used for ScienceBase item.
  # rgdal::writeOGR(sp_points, irma_points.shp, # Will be used later instead of cells.
  #                 layer = "irma_points", driver = "ESRI Shapefile", overwrite_layer=TRUE)
  # rgdal::writeOGR(sp_cells, "irma_cells.shp",
  #                 layer = "irma_cells", driver = "ESRI Shapefile", overwrite_layer=TRUE)
}




