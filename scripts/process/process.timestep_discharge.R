process.timestep_discharge <- function(viz = as.viz('timestep-discharge')){
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c("gage-data","timesteps", "storm-sites"))
  times <- as.POSIXct(strptime(depends[['timesteps']]$times, 
                               format = '%b %d %I:%M %p', 
                               tz = "America/New_York"))
  
  active.sites <- depends$`storm-sites`@data %>% 
    filter(class == 'nwis-dot') %>% .$id
  
  data.sub <- filter(depends[["gage-data"]], paste0("nwis-", site_no) %in% active.sites) %>% 
    group_by(site_no) %>% 
    filter(min(dateTime) <= times[2], max(dateTime) >= tail(times, 2L)[2])
  
  interp_q <- function(site.no) {
    use.i <- data.sub$site_no == site.no
    approx(x = data.sub$dateTime[use.i], y = data.sub$p_Inst[use.i], xout = times)
  }
  
  timestep.q <- lapply(unique(data.sub$site_no), interp_q) %>% setNames(paste0("nwis-", unique(data.sub$site_no)))
  saveRDS(timestep.q, viz[['location']])
}