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
  
  # grab bbox in viz projection coords.
  bbox <- sp::bbox(storm_area)
  
  # Build grid topology and make raster.
  x_range <- (bbox[3] - bbox[1])/cell_size
  y_range <- (bbox[4] - bbox[2])/cell_size
  grid_topology <- sp::GridTopology(c(bbox[1],bbox[2]),
                                    cellsize = c(cell_size,cell_size),
                                    cells.dim = c(x_range, y_range))
  sp_grid <- raster::raster(sp::SpatialGrid(grid_topology, proj))
  
  # Create cell geometry
  sp_cells <- raster::rasterToPolygons(sp_grid)
  # Create and set unique ids
  IDs <- paste0("p",sapply(slot(sp_cells, "polygons"), function(x) slot(x, "ID")))
  sp::spChFIDs(sp_cells) <- IDs
  
  # create spatialpolygonsdataframe with rownames and one column that match polygon IDs
  sp_cells <- sp::SpatialPolygonsDataFrame(sp_cells, 
                                           data.frame(id=IDs, 
                                                      row.names = IDs, 
                                                      stringsAsFactors = F))
  
  # Create points for cell geometry
  sp_points <- sp::SpatialPoints(raster::rasterToPoints(sp_grid),proj)
  
  # Get IDs from cell geometry and make points into SpatialPointsDataFrame
  sp_point_ids <- sp::over(sp_points, sp_cells, returnList = F, fn=NULL)
  sp_points <- sp::SpatialPointsDataFrame(sp_points, sp_point_ids)
  
  ### Now we have sp_points and sp_cells that share IDs. ###
  
  # convert States to SpatialPolygonDataFrame with grouping column for union step.
  stateIDs <- sapply(slot(states, "polygons"), function(x) slot(x, "ID"))
  state_boundary <- rgeos::gBuffer(sp::SpatialPolygonsDataFrame(states, 
                                                                data.frame(viz=rep("viz", 
                                                                                   length(stateIDs)), 
                                                                           stringsAsFactors = F, 
                                                                           row.names = stateIDs)),
                                   byid = T,
                                   width = 0) # zero buffer to fix topology exception.
  
  # Union States into a single polygon.
  state_boundary <- maptools::unionSpatialPolygons(state_boundary, 
                                                   state_boundary@data$viz)
  
  # Create one total clipping geometry that is the intersection of states 
  # and the storm_area.
  storm_states <- rgeos::gIntersection(storm_area, state_boundary)
  
  # Throw away cells that are not in the clipping geometry.
  sp_cells <- sp_cells[which(!is.na(sp::over(sp_cells, storm_states, returnList = F))),]
  
  # Intersect the cells with the state boundary. This removes partial overlap 
  #with the ocean but leaves cell edges intact inland. 
  sp_cells_i <- rgeos::gIntersection(sp_cells, state_boundary, byid = T)
  
  # fix IDs after gIntersection killed them
  sp_cells_ids <- sp::over(sp_cells_i, sp_cells, returnList = F, fn=NULL)
  IDs <- sapply(slot(sp_cells_i, "polygons"), function(x) strsplit(slot(x, "ID")," ")[[1]][1])
  sp::spChFIDs(sp_cells_i) <- IDs
  sp_cells <- sp::SpatialPolygonsDataFrame(sp_cells_i, data.frame(id = IDs, 
                                                                  row.names = IDs, 
                                                                  stringsAsFactors = F))
  
  # Throw away points by cell ID for cells that are still partially included.
  sp_points <- sp_points[which(sp_points@data$id %in% sp_cells@data$id),]
  
  # Output is cell geometry ready for geoknife and points for use in the viz.
  out <- list(cells = sp::spTransform(sp_cells, sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')), points = sp_points)
  
  saveRDS(out, viz[['location']])
  
}



