fetch.nwis_data <- function(viz = as.viz('nwis-data')) {

  if(!viz[['refetch']]){
    return(NULL)
  }
  
  required <- c("depends", "location")
  checkRequired(viz, required)
  depends <- readDepends(viz)
  sites <- depends[['sites']]@data
  nws.sites <- depends[['nws-data']]$sites
  
  sites <- sites[sites$site_no %in% nws.sites$site_no[!is.na(nws.sites$flood.stage)], ]
  
  dateTimes <- depends[['timesteps']]$times
  dateTimes_fromJSON <- as.POSIXct(strptime(dateTimes, format = '%b %d %I:%M %p'), 
                                   tz = "America/New_York")
  
  start.date <-  as.Date(dateTimes_fromJSON[1])
  end.date <- as.Date(dateTimes_fromJSON[length(dateTimes)])
  
  sites_fetcher_params <- getContentInfo('sites')
  parameter_code <- sites_fetcher_params[['pCode']]
  
  nwis_data <- data.frame()
  
  for(i in seq(1,nrow(sites),by=20)) {
    
    site_set <- na.omit(sites$site_no[i:(i+19)])
    
    nwis_set <- dataRetrieval::renameNWISColumns(
      dataRetrieval::readNWISdata(service="iv",
                                parameterCd=parameter_code,
                                sites = site_set,
                                startDate = start.date,
                                endDate = end.date,
                                tz = "America/New_York"))
  
    nwis_data <- dplyr::bind_rows(nwis_data, nwis_set)
  }
  
  nwis_data <- dplyr::filter(nwis_data, dateTime %in% dateTimes_fromJSON)
  
  names(nwis_data)[which(grepl(".*_Inst$", names(nwis_data)))] <- "p_Inst"
  names(nwis_data)[which(grepl(".*_Inst_cd$", names(nwis_data)))] <- "p_Inst_cd"
  
  location <- viz[['location']]
  saveRDS(nwis_data, file=location)
  
}

fetchTimestamp.nwis_data <- vizlab:::fetchTimestamp.file
