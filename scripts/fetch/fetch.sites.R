#fetch NWIS iv data, downsample to hourly

fetch.sites <- function(viz){
  required <- c("location")
  checkRequired(viz, required)
  
  library(dplyr)
  counties <- readDepends(viz)[['counties']] 
  #will this be ok with dependency management?
  start.date <-  as.Date(getContentInfo('precip-data')[['start.date']])
  
  site_sum_all <- data.frame()
  for(i in states){
    sites <- dataRetrieval::readNWISdata(service = "site",
                          seriesCatalogOutput=TRUE,
                          parameterCd="00060",
                          stateCd = i)
    sites_sum <- filter(sites, parm_cd == "00060",
                        data_type_cd == "uv",
                        !site_tp_cd %in% c("LK", "ES", "GW")) %>% #others?
      mutate(end_date = as.Date(end_date)) %>%
      filter(end_date >= start.date,
             count_nu >= 3000,
             !(is.na(alt_datum_cd))) %>%
      select(site_no, station_nm, dec_lat_va, dec_long_va) %>%
      data.frame() 
    site_sum_all <- bind_rows(site_sum_all, sites_sum)
    
  }
  
  location <- viz[['location']]
  saveRDS(site_sum_all, file=location)
}
