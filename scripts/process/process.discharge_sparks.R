
grab_spark <- function(vals){
  x = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(vals, type='l', axes=F, ann=F)
  }, height=0.4, width=2)
  xml2::xml_attr(xml2::xml_find_first(x, '//*[local-name()="polyline"]'),'points')
}

# Should rename at some point. Not always going to be discharge.
process.discharge_sparks <- function(viz = as.viz('discharge-sparks')){
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c('timesteps', "gage-data"))
  times <- as.POSIXct(strptime(depends[['timesteps']]$times, 
                               format = '%b %d %I:%M %p', 
                               tz = "America/New_York"))
  
  interp_q <- function(x,y) {
    approx(x, y, xout = times)$y %>% grab_spark
  }
  
  sparks <- group_by(depends[["gage-data"]], site_no) %>% 
    filter(min(dateTime) <= times[2], max(dateTime) >= tail(times, 2L)[2]) %>% 
    summarize(points = interp_q(dateTime, p_Inst)) %>% 
    mutate(class = "sparkline", 
           id = sprintf("sparkline-%s", site_no), 
           style = "mask: url(#spark-opacity);",
           onmouseover=sprintf("setBold('nwis-%s');", site_no), 
           onmouseout=sprintf("setNormal('nwis-%s');hovertext(' ');", site_no),
           onclick=sprintf("openNWIS('%s', evt);", site_no),
           onmousemove=sprintf("hovertext('USGS %s',evt);", site_no))
  
  sites <- depends[['sites']][c("site_no", "dec_lat_va")]
  
  sparks <- sparks %>% 
    left_join(sites, by = "site_no") %>%
    arrange(desc(dec_lat_va)) %>%
    select(-site_no, -dec_lat_va) 
  
  saveRDS(sparks, viz[['location']])
}