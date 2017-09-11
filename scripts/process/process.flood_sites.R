#' 
#' filter sites to just those that have exceed flood stage
#' for the time stamp selected
process.flood_sites <- function(viz){
  
  library(dplyr)
  library(sp)
  
  time_stamp <- viz[['time-stamp']]
  
  depends <- readDepends(viz)
  nws_data <- depends[["nws-data"]]
  gage_data <- depends[["gage-data"]]
  storm_sites <- depends[["storm-sites"]]
  
  flood_stage_info <- select(nws_data$sites, site_no, flood.stage, flood.stage.units)
  
  flood_data <- gage_data %>% 
    filter(site_no %in% unique(nws_data$sites$site_no)) %>% 
    filter(dateTime == as.POSIXct(time_stamp, tz = "America/New_York")) %>% 
    left_join(flood_stage_info) %>% 
    filter(p_Inst >= flood.stage) %>% 
    mutate(id = paste0("nwis-", site_no))
  
  flood_sites_sp <- storm_sites
  flood_sites_sp <- flood_sites_sp[which(flood_sites_sp@data$id %in% flood_data$id), ]
  
  saveRDS(flood_sites_sp, viz[['location']])
}
