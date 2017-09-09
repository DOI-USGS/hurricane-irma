process.storm_location <- function(viz = as.viz('storm-location')){
  
  message("someday we should support the pattern of multiple layers in a single shapefile zip")
  library(sp)
  library(dplyr)
  
  shp.path <- file.path(tempdir(), 'tmp')
  if (!dir.exists(shp.path)){
    dir.create(shp.path)
  }
  
  depends <- readDepends(viz)
  times <- as.POSIXct(strptime(depends[['timesteps']]$times, format = '%b %d %I:%M %p', tz = "America/New_York"))
  track.meta <- getContentInfo("hurricane-track")
  
  unzip(track.meta$location, exdir = shp.path)
  
  as.time <- function(YEAR, MONTH, DAY, HHMM){
    as.POSIXct(sprintf('%s-%s-%s %s', YEAR, MONTH, DAY, HHMM), format='%Y-%m-%d %H%M', tz="America/New_York")
  }
  
  warning('NWS file pattern is hard-coded here and is storm-specific. They may change it?')
  pts.layer <- sprintf("%s%s%s_pts", toupper(track.meta$ocean), track.meta$stormnum, track.meta$year)
  shp.data <- rgdal::readOGR(shp.path, layer = pts.layer) %>% data.frame %>% 
    filter(STORMNAME==track.meta$storm) %>% 
    mutate(DateTime = as.time(YEAR, MONTH, DAY, HHMM)) %>% 
    select(LAT, LON, DateTime, INTENSITY)
  
  unlink(shp.path)
  
  
  lat.out <- approx(shp.data$DateTime, shp.data$LAT, xout = times)$y
  lon.out <- approx(shp.data$DateTime, shp.data$LON, xout = times)$y
  pts <- cbind(lon.out[!is.na(lon.out)], lat.out[!is.na(lon.out)])
  location <- SpatialPoints(pts, proj4string=CRS("+proj=longlat +datum=WGS84"))

  
  data.out <- data.frame(id = paste0('storm-', 1:length(location)), 
                         class = "storm-dot", 
                         r = "8", 
                         stringsAsFactors = FALSE) 
  row.names(data.out) <- row.names(location)
  sp.data.frame <- as(object = location, Class = paste0(class(location), "DataFrame"))
  sp.data.frame@data <- data.out
  row.names(sp.data.frame) <- row.names(data.out)
  
  
  location <- spTransform(sp.data.frame, CRS(depends[['view-limits']]$proj.string))
  
  saveRDS(location, viz[['location']])
}