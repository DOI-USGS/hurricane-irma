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
  gage_flooded <- gage_data %>% dplyr::group_by(site_no) %>% dplyr::summarize(max_gage = max(p_Inst, na.rm = TRUE)) %>% 
    dplyr::left_join(nws_sites) %>% dplyr::filter(!is.na(max_gage), !is.na(flood.stage)) %>% 
    dplyr::filter(max_gage > viz[['perc_flood_stage']] * flood.stage)
  
  library(sp)
  sites_filtered <- sites[sites@data$site_no %in% gage_flooded$site_no, ]
  s.ids <- sites_filtered$site_no
  sites_filtered@data <- data.frame(id = paste0('nwis-', s.ids), 
                            class = 'nwis-dot',
                            r = '3.5',
                            onmousemove = sprintf("hovertext('USGS %s',evt);", s.ids),
                            onmouseout = sprintf("setNormal('sparkline-%s');setNormal('nwis-%s');hovertext(' ');", s.ids, s.ids),
                            onmouseover= sprintf("setBold('sparkline-%s');setBold('nwis-%s');", s.ids, s.ids),
                            onclick=sprintf("openNWIS('%s', evt);", s.ids),
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
  
  
  use.sites <- storm_sites$id %in% names(gage_data) & !duplicated(storm_sites$id)
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
  storm_sites <- storm_sites[use.sites, ]
  d.out <- mutate(class_df, id = paste0('nwis-', site_no), add.class = class) %>% 
    select(id, add.class) %>% 
    left_join(storm_sites@data, by = 'id')
  
  # why? because the sites were wrong
  for (j in 1:length(storm_sites)){
    which.i <- d.out$id == storm_sites@data$id[j]
    storm_sites@data$class[j] <- paste(storm_sites@data$class[j], d.out$add.class[which.i], sep=' ')
  }
  
  saveRDS(storm_sites, viz[['location']])
}
