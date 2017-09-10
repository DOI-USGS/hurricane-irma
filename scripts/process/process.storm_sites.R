process.storm_sites <- function(viz = as.viz('storm-sites')){
  library(magrittr)
  depends <- readDepends(viz)
  checkRequired(depends, c("view-limits", "sites", "storm-area-filter", "nws-data"))
  view.lims <- depends[["view-limits"]]
  sites <- depends[['sites']] 
  storm_poly <- depends[['storm-area-filter']]
  nws.sites <- depends[['nws-data']]
  
  library(dplyr)
  
  sites.sp <- sp::SpatialPoints(cbind(sites$dec_long_va,sites$dec_lat_va), 
                            proj4string = sp::CRS("+proj=longlat +ellps=GRS80 +no_defs"))
  
  sites.sp <- sp::spTransform(sites.sp, sp::CRS(view.lims$proj.string))
  
  storm_poly <- sp::spTransform(storm_poly, sp::CRS(view.lims$proj.string))

  is.featured <- rgeos::gContains(storm_poly, sites.sp, byid = TRUE) %>% rowSums() %>% as.logical() &
    sites$site_no %in% nws.sites$site_no[!is.na(nws.sites$flood.stage)]
  
  data.out <- data.frame(id = paste0('nwis-', sites$site_no), 
                         class = ifelse(is.featured, 'nwis-dot','inactive-dot'),
                         r = ifelse(is.featured, '2','1'),
                         onmousemove = ifelse(is.featured, sprintf("hovertext('USGS %s',evt);",sites$site_no), ""),
                         onmouseout = ifelse(is.featured, sprintf("setNormal('sparkline-%s');hovertext(' ');", sites$site_no), ""),
                         onmouseover= ifelse(is.featured, sprintf("setBold('sparkline-%s');", sites$site_no), ""),
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
  sites_active <- dplyr::filter(siteInfo@data, class == 'nwis-dot')$id
  sites_active <- gsub(pattern = "nwis-", replacement = "", x = sites_active)
  
  dateTimes <- depends[['timesteps']]$times
  dateTimes_fromJSON <- as.POSIXct(strptime(dateTimes, format = '%b %d %I:%M %p'), 
                                   tz = "America/New_York")
  
  start.date <-  as.Date(dateTimes_fromJSON[1])
  end.date <- as.Date(dateTimes_fromJSON[length(dateTimes)])
  
  sites_fetcher_params <- getContentInfo('sites')
  parameter_code <- sites_fetcher_params[['pCode']]
  
  nwisData <- dataRetrieval::renameNWISColumns(
    dataRetrieval::readNWISdata(service="iv",
                                parameterCd=parameter_code,
                                sites = sites_active,
                                startDate = start.date,
                                endDate = end.date,
                                tz = "America/New_York"))
  
  nwisData <- dplyr::filter(nwisData, dateTime %in% dateTimes_fromJSON)
  
  location <- viz[['location']]
  saveRDS(nwisData, file=location)
}