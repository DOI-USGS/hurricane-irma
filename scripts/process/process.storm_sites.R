process.storm_sites <- function(viz = as.viz('storm-sites')){
  
  depends <- readDepends(viz)
  view.lims <- depends[["view-limits"]]
  sites <- depends[['sites']] 
  
  sites.sp <- sp::SpatialPoints(cbind(sites$dec_long_va,sites$dec_lat_va), 
                            proj4string = sp::CRS("+proj=longlat +ellps=GRS80 +no_defs"))
  sites.sp <- sp::spTransform(sites.sp, sp::CRS(view.lims$proj.string))

  is.featured <- sites$dec_lat_va < 28 # for NOW!! need filter
  
  data.out <- data.frame(id = paste0('nwis-', sites$site_no), 
                         class = ifelse(is.featured, 'active-gage','inactive-gage'),
                         r = ifelse(is.featured, '2','1'),
                         onmousemove = "hovertext('TEST gage',evt);", 
                         onmouseout = "hovertext(' ');", 
                         stringsAsFactors = FALSE) 
  row.names(data.out) <- row.names(sites.sp)
  sp.data.frame <- as(object = sites.sp, Class = paste0(class(sites.sp), "DataFrame"))
  sp.data.frame@data <- data.out
  row.names(sp.data.frame) <- row.names(data.out)
  
  saveRDS(sp.data.frame, viz[['location']])
}

#fetch NWIS iv data, downsample to hourly

process.getNWISdata <- function(viz = as.viz('gage-data')){
  required <- c("depends", "location")
  checkRequired(viz, required)
  depends <- readDepends(viz)
  siteInfo <- depends[['storm-sites']]
  sites_active <- dplyr::filter(siteInfo@data, class == 'active-gage')$id
  sites_active <- gsub(pattern = "nwis-", replacement = "", x = sites_active)
  
  dateTimes <- depends[['timesteps']]$times
  dateTimes_fromJSON <- as.POSIXct(strptime(dateTimes, format = '%b %d %I:%M %p'), 
             tz = "America/New_York")
  
  start.date <-  as.Date(dateTimes_fromJSON[1])
  end.date <- as.Date(dateTimes_fromJSON[length(dateTimes)])
  
  nwisParams <- getContentInfo('sites')
  
  nwisData <- dataRetrieval::renameNWISColumns(dataRetrieval::readNWISdata(service="iv",
                                              parameterCd=nwisParams[['pCode']],
                                              sites = sites_active,
                                              startDate = start.date,
                                              endDate = end.date,
                                              tz = "America/New_York"))
  
  nwisData <- dplyr::filter(nwisData, dateTime %in% dateTimes_fromJSON)
  
  location <- viz[['location']]
  saveRDS(nwisData, file=location)
}