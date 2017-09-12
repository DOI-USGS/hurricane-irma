#' 
#' filter sites to just those that have exceed flood stage
#' for the time stamp selected
process.flood_sites <- function(viz){
  library(dplyr)
  
  time_stamp <- viz[['time-stamp']]
  
  depends <- readDepends(viz)
  nws_data <- depends[["nws-data"]]
  gage_data <- depends[["gage-data"]]
  storm_sites <- depends[["storm-sites"]]$sites.sp
  
  flood_sites_sp <- filterFloodSites(gage_data, nws_data, storm_sites, time_stamp)
  
  saveRDS(flood_sites_sp, viz[['location']])
}

filterFloodSites <- function(gage_height_data, flood_stage_info, sites_sp, time_stamp){
  library(dplyr)
  library(sp)
  
  flood_data <- gage_height_data %>% 
    filter(site_no %in% unique(flood_stage_info$sites$site_no)) %>% 
    filter(dateTime == as.POSIXct(time_stamp, tz = "America/New_York")) %>% 
    left_join(flood_stage_info$sites) %>% 
    filter(p_Inst >= flood.stage) %>% 
    mutate(id = paste0("nwis-", site_no))
  
  flood_sites_sp <- sites_sp[which(sites_sp@data$id %in% flood_data$id), ]
  
  return(flood_sites_sp)
}
