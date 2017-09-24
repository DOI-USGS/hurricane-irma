#has a site passed flood stage or forecasted to?
process.select_flood_sites <- function(viz = as.viz('storm-sites-flood')) {
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(viz, 'perc_flood_stage')
  gage_data <- depends[['nwis-data']]
  sites <- depends[['sites']]
  
  nws_sites <- depends[['nws-data']]
  
  gage_data <- filter(gage_data, site_no %in% sites@data$site_no)
  
  # this is where we have NAs from gages that are missing some data:
  gage_flooded <- gage_data %>% group_by(site_no) %>% summarize(max_gage = max(p_Inst, na.rm = TRUE)) %>% 
    left_join(nws_sites) %>% filter(!is.na(max_gage), !is.na(flood.stage)) %>% 
    filter(max_gage > viz[['perc_flood_stage']] * flood.stage)
  
  library(sp)
  sites_filtered <- sites[sites@data$site_no %in% gage_flooded$site_no, ]
  
  is.featured <- rep(TRUE, nrow(sites_filtered@data))
  
  sites_filtered@data <- data.frame(id = paste0('nwis-', sites_filtered$site_no), 
                            class = ifelse(is.featured, 'nwis-dot','inactive-dot'),
                            r = ifelse(is.featured, '3.5','1'),
                            onmousemove = ifelse(is.featured, sprintf("hovertext('USGS %s',evt);",sites$site_no), ""),
                            onmouseout = ifelse(is.featured, sprintf("setNormal('sparkline-%s');setNormal('nwis-%s');hovertext(' ');", sites$site_no, sites$site_no), ""),
                            onmouseover= ifelse(is.featured, sprintf("setBold('sparkline-%s');setBold('nwis-%s');", sites$site_no, sites$site_no), ""),
                            onclick=ifelse(is.featured, sprintf("openNWIS('%s', evt);", sites$site_no), ""), 
                            stringsAsFactors = FALSE)
  
  saveRDS(object = sites_filtered, file = viz[['location']])
}

#' 
#' filter sites to just those that have exceed flood stage
#' for the time stamp selected
process.flood_sites_classify <- function(viz = as.viz("flood-sites-classify")){
  library(dplyr)
  
  depends <- readDepends(viz)
  nws_data <- depends[["nws-data"]]
  gage_data <- depends[["timestep-discharge"]]
  storm_sites <- depends[["storm-sites-flood"]]
  
  site.nos <- sapply(names(gage_data), function(x) strsplit(x, '[-]')[[1]][2], USE.NAMES = FALSE)
  
  #this won't work if we switched to discharge
  pcode <- getContentInfo('sites')$pCode
  stopifnot(pcode == "00065")
  
  class_df <- data.frame(stringsAsFactors = FALSE)
  #actually create the classes
  for(site in site.nos) {
    flood.stage <- filter(nws_data, site_no == site) %>% .$flood.stage %>% .[1]
    which.floods <- which(gage_data[[paste0('nwis-',site)]]$y > flood.stage)
    site_class <- paste(paste("f", which.floods, sep = "-"), collapse = " ")
    class_df_row <- data.frame(site_no = site, class = site_class, 
                               stringsAsFactors = FALSE)
    class_df <- bind_rows(class_df, class_df_row)
  }
  d.out <- mutate(class_df, id = paste0('nwis-', site_no)) %>% 
    select(id, raw.class = class) %>% left_join(storm_sites@data, by="id") %>% 
    mutate(raw.class = ifelse(is.na(raw.class), "", paste0(" ", raw.class))) %>% 
    mutate(class = paste0(class,  raw.class)) %>% select(-raw.class)
  storm_sites@data <- d.out
  saveRDS(storm_sites, viz[['location']])
}
