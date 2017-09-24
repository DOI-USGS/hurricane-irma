fetch.nwis_data <- function(viz = as.viz('nwis-data')) {

  if(!viz[['refetch']]){
    return(NULL)
  }
  
  required <- c("depends", "location")
  checkRequired(viz, required)
  depends <- readDepends(viz)
  sites <- depends[['sites']]@data
  nws.sites <- depends[['nws-conversion']]$USGS
  
  sites <- sites[sites$site_no %in% nws.sites, ]
  
  dateTimes <- depends[['timesteps']]$times
  dateTimes_fromJSON <- as.POSIXct(strptime(dateTimes, format = '%b %d %I:%M %p'), 
                                   tz = "America/New_York")
  
  start.date <-  as.Date(dateTimes_fromJSON[1])
  end.date <- as.Date(dateTimes_fromJSON[length(dateTimes)])
  
  sites_fetcher_params <- getContentInfo('sites')
  parameter_code <- sites_fetcher_params[['pCode']]
  
  nwis_data <- data.frame()
  
  site.chunk <- 30
  
  for(i in seq(1,nrow(sites),by=site.chunk)) {
    site_set <- na.omit(sites$site_no[i:(i+site.chunk-1)])
    dRurl <- dataRetrieval:::constructNWISURL(service="iv", 
                                              parameterCd=parameter_code,
                                              siteNumbers = site_set,
                                              startDate = start.date,
                                              endDate = end.date)
    dRquery <- strsplit(dRurl, '[?]')[[1]][2]
    dRurl <- sprintf('https://waterservices.usgs.gov/nwis/iv/?%s', dRquery) # the DR base URL is 503'ing
    
    nwis_set <- dataRetrieval::renameNWISColumns(dataRetrieval::importWaterML1(dRurl, tz = "America/New_York"))
    nwis_set$dateTime <- as.POSIXct(nwis_set$dateTime, tz = "America/New_York", format = "%Y-%m-%dT%H:%M:%S")
    
    names(nwis_set)[which(grepl(".*_Inst$", names(nwis_set)))] <- "p_Inst"
    names(nwis_set)[which(grepl(".*_Inst_cd$", names(nwis_set)))] <- "p_Inst_cd"  
    nwis_data <- dplyr::bind_rows(nwis_data, nwis_set)
  }
  
  nwis_data <- dplyr::filter(nwis_data, dateTime %in% dateTimes_fromJSON)
  
  
  
  location <- viz[['location']]
  saveRDS(nwis_data, file=location)
  
}