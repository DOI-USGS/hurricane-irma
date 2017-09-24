# fetch NWIS sites and return a useful collection of site info

fetch.sites <- function(viz = as.viz('sites')){
  
  if(!viz[['refetch']]){
    return(NULL)
  }
  
  library(dplyr)
  depends <- readDepends(viz)
  required <- c("location", "pCode")
  checkRequired(viz, required)
  
  # Site Fetch Dependencies
  pCode <- viz[['pCode']]
  start.date <-  as.Date(getContentInfo('precip-cell-data')[['start.date']])
  counties <- depends[['counties']] 
  
  # Post Fetch Filter Dependencies
  storm_area <- depends[['storm-area-filter']]
  view_lims <- depends[["view-limits"]]
  
  sites <- data.frame()
  
  counties_fips <- maps::county.fips %>% 
    dplyr::filter(polyname %in% names(counties)) %>% 
    mutate(fips = sprintf('%05d', fips))
  
  #max 20 counties at once
  for(county_set in seq(1,nrow(counties_fips),by=32)) {
    fips_subset <- na.omit(counties_fips$fips[county_set:(county_set+19)])
    
    site_set <- dataRetrieval::readNWISdata(service = "site",
                                         seriesCatalogOutput=TRUE,
                                         parameterCd=pCode,
                                         countyCd = fips_subset)
    
    site_set <- filter(site_set, parm_cd == pCode,
                    !site_tp_cd %in% c("LK", "ES", "GW")) %>%
      data.frame()
    
    daily_begin_date <- filter(site_set, data_type_cd == "dv") %>% 
      select(site_no, begin_date) %>%
      rename(dv_begin_date = begin_date)
    
    site_set <- filter(site_set, data_type_cd == "uv") %>% #others?
      mutate(end_date = as.Date(end_date)) %>%
      filter(end_date >= start.date,
             count_nu >= 3000,
             !(is.na(alt_datum_cd))) %>%
      select(site_no, station_nm, dec_lat_va, dec_long_va) %>%
      left_join(daily_begin_date, by="site_no")
    data.frame() 
    
    sites <- dplyr::bind_rows(sites, site_set)
    
  }
  
  # Filter out stuff we know we don't need.
  sites <- unique(sites)
  
  sites$dv_begin_date <- as.Date(sites$dv_begin_date)
  
  sites <- sp::spTransform(sp::SpatialPointsDataFrame(cbind(sites$dec_long_va,
                                               sites$dec_lat_va), 
                                         data = sites,
                                         proj4string = sp::CRS("+proj=longlat +ellps=GRS80")),
                              sp::CRS(view_lims$proj.string))
  
  storm_poly <- sp::spTransform(storm_area, 
                                sp::CRS(view_lims$proj.string))
  
  in_out <- rgeos::gContains(storm_poly, sites, byid = TRUE) %>% 
    rowSums() %>% 
    as.logical() & 
    !(sites$site_no %in% c('08074500', '02489500', '08165500', '08047500',
                           '08020000', '07342500', '08020900')) # is not one of our manually selected bad sites
  
  sites <- sites[which(in_out), ]
  
  location <- viz[['location']]
  saveRDS(sites, file=location)
}
