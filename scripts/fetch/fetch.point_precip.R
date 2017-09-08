fetch.point_precip <- function(viz = as.viz('view-limits')){
  viz <- readRDS("temp_view-limits.rds")
  bbox <- viz[["bbox"]]
  
  # First we need to figure out the data we need to request.
  nc <- ncdf4::nc_open("https://cida.usgs.gov/thredds/dodsC/stageiv_combined")
  lon <- ncdf4::ncvar_get(nc, nc$var$lon)
  lat <- ncdf4::ncvar_get(nc, nc$var$lat)
  
  coords <- data.frame(x=matrix(lon, ncol=1), y=matrix(lat, ncol=1))
  
  indices <- data.frame(x = matrix(matrix(rep(c(0:1120), 881), 
                                          ncol=1121, byrow = T), 
                                   ncol=1), 
                        y = matrix(matrix(rep(c(0:880), 1121), 
                                          ncol=1121), 
                                   ncol=1))
  
  all_points <- sp::SpatialPoints(coords, sp::CRS("+init=EPSG:4326"))
  
  bbox <- as.sp_box(xs = bbox[c(1,3)], ys = bbox[c(2,4)], proj.string = sp::CRS("+init=EPSG:4326"))
  
  in_points <- sp::over(all_points, bbox, fn = NULL)
  points <- all_points[which(!is.na(in_points))]
  indices <- indices[which(!is.na(in_points)),]
  
  #Verified with:
  # indices[100,]
  #          x   y
  # 720110 817 332
  # points@coords[100,]
  # x         y 
  # -86.76411  33.74411
  # https://cida.usgs.gov/thredds/dodsC/stageiv_combined.ascii?lon[817][332],lat[817][332]
  # returns:
  # Dataset {
  # Float32 lon[x = 1][y = 1];
  # Float32 lat[x = 1][y = 1];
  # } stageiv_combined;
  # ---------------------------------------------
  # lon[1][1]
  # [0], -86.764114
  # 
  # lat[1][1]
  # [0], 33.74411
  
  # Set up the static stuff that we will be able to loop over.
  x_range <- range(indices$x)+1 # ncdf4 is 1 indexed, opendap is 0 indexed.
  y_range <- range(indices$y)+1
  
  lon <- ncdf4::ncvar_get(nc = nc, 
                          varid = nc$var$lon, 
                          start = c(y_range[1], x_range[1]), 
                          count = c(y_range[2]-y_range[1], x_range[2]-x_range[1]))
  lat <- ncdf4::ncvar_get(nc = nc, 
                          varid = nc$var$lat, 
                          start = c(y_range[1], x_range[1]), 
                          count = c(y_range[2]-y_range[1], x_range[2]-x_range[1]))
  
  coords <- data.frame(x=matrix(lon, ncol=1), y=matrix(lat, ncol=1))
  
  # Will implement a loop instead of just doing 1 here.
  pr_data <- t(ncdf4::ncvar_get(nc = nc,
                              varid = nc$var$Total_precipitation_surface_1_Hour_Accumulation, 
                              start = c(x_range[1], y_range[1], 1),
                              count = c(x_range[2]-x_range[1], y_range[2]-y_range[1], 1)))
  
  points <- sp::SpatialPointsDataFrame(coords = coords, 
                                       data = data.frame(matrix(pr_data, ncol=1)),
                                       proj4string = CRS("+init=EPSG:4326"))
  
  # Temporary? We could do this with more states or another area.
  state_boundary <- to_sp("state", proj.string = "+init=EPSG:4326")
  states <- c("florida", "georgia", "alabama", "south carolina")
  index <- c()
  for(i in 1:length(state_boundary@polygons)) {
    print(state_boundary@polygons[[i]]@ID)
    if(state_boundary@polygons[[i]]@ID %in% states) {
      index <- c(index,i)
    }
  }
  state_boundary <- state_boundary[index,]
  
  lor <- range(lon)
  lar <- range(lat)
  spacing = 0.25
  lon_out_points <- seq(lor[1], lor[2], by=spacing)
  lat_out_points <- seq(lar[1], lar[2], by=spacing)
  tx<-matrix(rep(lon_out_points, length(lat_out_points)), ncol= length(lat_out_points))
  ty<-matrix(rep(lat_out_points, length(lon_out_points)), ncol= length(lat_out_points), byrow = F)
  out_coords <- data.frame(x=matrix(tx, ncol=1), 
                           y=matrix(ty, ncol=1, byrow = F))
}




