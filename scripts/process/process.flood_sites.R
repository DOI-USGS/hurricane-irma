#' 
#' filter sites to just those that have exceed flood stage
#' for the time stamp selected
process.flood_sites <- function(viz = as.viz("flood-sites-classify")){
  library(dplyr)
  
  depends <- readDepends(viz)
  nws_data <- depends[["nws-data"]]$sites
  gage_data <- depends[["gage-data"]]
  storm_sites <- depends[["storm-sites"]]
  times <- as.POSIXct(strptime(depends[['timesteps']]$times, 
                                        format = '%b %d %I:%M %p', 
                                        tz = "America/New_York"))
  times_df <- data.frame(dateTime = times, index = 1:length(times))
  
  #this won't work if we switched to discharge
  pcode <- getContentInfo('sites')$pCode
  stopifnot(pcode == "00065")
  
  storm_site_ids <- filter(storm_sites@data, class == "nwis-dot")$id
  storm_site_ids <- gsub(pattern = "nwis-", replacement = "", x = storm_site_ids)
  nwis_flood_indexed <- filter(gage_data, site_no %in% storm_site_ids) %>% 
        left_join(nws_data, by = "site_no") %>% filter(p_Inst > flood.stage) %>% 
        left_join(times_df, by = "dateTime") %>% select(site_no, index)
  
  class_df <- data.frame()
  #actually create the classes
  for(site in unique(nwis_flood_indexed$site_no)) {
    site_df <- filter(nwis_flood_indexed, site_no == site)
    site_class <- paste(paste("f", site_df$index, sep = "-"), collapse = " ")
    class_df_row <- data.frame(site_no = site, class = site_class, 
                               stringsAsFactors = FALSE)
    class_df <- bind_rows(class_df, class_df_row)
  }
  
  saveRDS(class_df, viz[['location']])
}


