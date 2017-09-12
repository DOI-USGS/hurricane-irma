fetch.precipSpatial <- function(viz = as.viz('precip-spatial')){

  if(!viz[['refetch']]){
    return(NULL)
  }
  
  checkRequired(viz, "cell_size")
  
  view <- readDepends(viz)[["view-limits"]]
  proj.string <- view[["proj.string"]]
  proj <- sp::CRS(proj.string)
  storm_area <- sp::spTransform(readDepends(viz)[["storm-area-filter"]],proj)
  states <- readData("states")
  cell_size <- viz[["cell_size"]] # meters -- set to make grid more course or dense.
  
  bbox <- sp::bbox(storm_area)
  
  x_range <- (bbox[3] - bbox[1])/cell_size
  y_range <- (bbox[4] - bbox[2])/cell_size
  
  grid_topology <- sp::GridTopology(c(bbox[1],bbox[2]),
                                    cellsize = c(cell_size,cell_size),
                                    cells.dim = c(x_range, y_range))
  sp_grid <- raster::raster(sp::SpatialGrid(grid_topology, proj))
  
  sp_cells <- raster::rasterToPolygons(sp_grid)
  IDs <- paste0("p",sapply(slot(sp_cells, "polygons"), function(x) slot(x, "ID")))
  sp::spChFIDs(sp_cells) <- IDs
  
  sp_points <- sp::SpatialPoints(raster::rasterToPoints(sp_grid),proj)
  
  sp_cells <- sp::SpatialPolygonsDataFrame(sp_cells, data.frame(id=IDs, 
                                                                row.names = IDs, 
                                                                stringsAsFactors = F))

  sp_point_ids <- sp::over(sp_points, sp_cells, returnList = F, fn=NULL)
  sp_points <- sp::SpatialPointsDataFrame(sp_points, sp_point_ids)

  # Now we have sp_points and sp_cells that share IDs.
  
  stateIDs <- sapply(slot(states, "polygons"), function(x) slot(x, "ID"))
  state_boundary <- rgeos::gBuffer(sp::SpatialPolygonsDataFrame(states, 
                                             data.frame(viz=rep("viz", 
                                                                length(stateIDs)), 
                                                        stringsAsFactors = F, 
                                                        row.names = stateIDs)),
                                   byid = T,
                                   width = 0) # zero buffer to fix topology exception.
  
  state_boundary <- maptools::unionSpatialPolygons(state_boundary, state_boundary@data$viz)
  
  storm_states <- rgeos::gIntersection(storm_area, state_boundary)
  
  sp_cells <- sp_cells[which(!is.na(sp::over(sp_cells, storm_states, returnList = F))),]
  
  sp_cells_i <- rgeos::gIntersection(sp_cells, state_boundary, byid = T)
  sp_cells_ids <- sp::over(sp_cells_i, sp_cells, returnList = F, fn=NULL)
  
  IDs <- sapply(slot(sp_cells_i, "polygons"), function(x) strsplit(slot(x, "ID")," ")[[1]][1])
  
  sp::spChFIDs(sp_cells_i) <- IDs
  
  sp_cells <- sp::SpatialPolygonsDataFrame(sp_cells_i, data.frame(id = IDs, 
                                                                  row.names = IDs, 
                                                                  stringsAsFactors = F))
  
  sp_points <- sp_points[which(sp_points@data$id %in% sp_cells@data$id),]
  
  saveRDS(sp_points, viz[['location']])
  
  # rgdal::writeOGR(sp_cells, "irma_cells.shp",
  #                 layer = "irma_cells", driver = "ESRI Shapefile", overwrite_layer=TRUE) 
  # rgdal::writeOGR(sp_points, "irma_points.shp",
  #                 layer = "irma_points", driver = "ESRI Shapefile", overwrite_layer=TRUE)
}




