process.storm_sites <- function(viz = as.viz('storm-sites')){
  library(magrittr)
  
  checkRequired(viz, "perc_flood_stage")
  depends <- readDepends(viz)
  checkRequired(depends, c("view-limits", "sites", "storm-area-filter", "nws-data"))
  
  view.lims <- depends[["view-limits"]]
  sites <- depends[['sites']] 
  storm_poly <- depends[['storm-area-filter']]
  nws.sites <- depends[['nws-data']]$sites
  nws.data <- depends[['nws-data']]$forecasts
  
  library(dplyr)
  
  sites$begin_date <- as.Date(sites$begin_date)
  
  sites.sp <- sp::SpatialPointsDataFrame(cbind(sites$dec_long_va,
                                               sites$dec_lat_va), 
                                         data = sites,
                                         proj4string = sp::CRS("+proj=longlat +ellps=GRS80 +no_defs"))
  
  sites.sp <- sp::spTransform(sites.sp, sp::CRS(view.lims$proj.string))
  
  storm_poly <- sp::spTransform(storm_poly, sp::CRS(view.lims$proj.string))
  
  percent_flood_stage <- as.numeric(viz[["perc_flood_stage"]])
  
  nws_flood_predicted <- unique(nws.data %>% 
    left_join(nws.sites[c("site_no", "flood.stage", "NWS")], by = c("site" = "NWS")) %>% 
    mutate(forecast_vals = as.numeric(forecast_vals)) %>% 
    filter(forecast_vals > (percent_flood_stage * flood.stage)) %>% 
    select(site_no))

  is.featured <- rgeos::gContains(storm_poly, sites.sp, byid = TRUE) %>% 
    rowSums() %>% 
    as.logical() & 
    sites$site_no %in% nws.sites$site_no[!is.na(nws.sites$flood.stage)] & # has a flood stage estimate
    sites$begin_date < as.Date("2017-01-01") & # has period of record longer than some begin date
    sites$site_no %in% nws_flood_predicted$site_no # is precicted to be within a configurable percent of flood stage
  
  sites.sp@data <- data.frame(id = paste0('nwis-', sites.sp@data$site_no), 
                         class = ifelse(is.featured, 'nwis-dot','inactive-dot'),
                         r = ifelse(is.featured, '2','1'),
                         onmousemove = ifelse(is.featured, sprintf("hovertext('USGS %s',evt);",sites.sp@data$site_no), ""),
                         onmouseout = ifelse(is.featured, sprintf("setNormal('sparkline-%s');hovertext(' ');", sites.sp@data$site_no), ""),
                         onmouseover= ifelse(is.featured, sprintf("setBold('sparkline-%s');", sites.sp@data$site_no), ""),
                         onclick=ifelse(is.featured, sprintf("openNWIS('%s');", sites.sp@data$site_no), ""), 
                         stringsAsFactors = FALSE)
  
  saveRDS(sites.sp, viz[['location']])
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
  
  names(nwisData)[which(grepl(".*_Inst$", names(nwisData)))] <- "p_Inst"
  names(nwisData)[which(grepl(".*_Inst_cd$", names(nwisData)))] <- "p_Inst_cd"
  
  location <- viz[['location']]
  saveRDS(nwisData, file=location)
}