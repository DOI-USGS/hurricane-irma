# fetch NWIS sites and return a useful collection of site info

fetch.sites <- function(viz = as.viz('sites')){
  library(dplyr)
  
  required <- c("location", "pCode")
  checkRequired(viz, required)
  
  pCode <- viz[['pCode']]
  
  counties <- readDepends(viz)[['counties']] 
  #will this be ok with dependency management?
  start.date <-  as.Date(getContentInfo('precip-data')[['start.date']])
  
  site_sum_all <- data.frame()
  
  counties_fips <- maps::county.fips %>% 
    dplyr::filter(polyname %in% names(counties)) %>% 
    mutate(fips = sprintf('%05d', fips))
  
  #max 20 counties at once
  reqBks <- seq(1,nrow(counties_fips),by=20)
  
  for(brk in reqBks) {
    fips_subset <- na.omit(counties_fips$fips[brk:(brk+19)])
    
    sites <- dataRetrieval::readNWISdata(service = "site",
                                         seriesCatalogOutput=TRUE,
                                         parameterCd=pCode,
                                         countyCd = fips_subset)
    
    sites <- filter(sites, parm_cd == pCode,
                    !site_tp_cd %in% c("LK", "ES", "GW")) %>%
      data.frame()
    
    daily_begin_date <- filter(sites, data_type_cd == "dv") %>% 
      select(site_no, begin_date) %>%
      rename(dv_begin_date = begin_date)
    
    sites_sum <- filter(sites, data_type_cd == "uv") %>% #others?
      mutate(end_date = as.Date(end_date)) %>%
      filter(end_date >= start.date,
             count_nu >= 3000,
             !(is.na(alt_datum_cd))) %>%
      select(site_no, station_nm, dec_lat_va, dec_long_va) %>%
      left_join(daily_begin_date, by="site_no")
      data.frame() 
    
    site_sum_all <- bind_rows(site_sum_all, sites_sum)
    
  }
  location <- viz[['location']]
  saveRDS(site_sum_all, file=location)
}
