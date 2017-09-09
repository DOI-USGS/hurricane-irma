
grab_spark <- function(vals){
  
  x = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(vals, type='l', axes=F, ann=F)
  }, height=0.4, width=2)
  xml2::xml_attr(xml2::xml_find_first(x, '//*[local-name()="polyline"]'),'points')
}

process.discharge_sparks <- function(viz = as.viz('discharge-sparks')){
  library(dplyr)
  disch <- readData(viz[['depends']][1])
  times <- readData(viz[['depends']][2]) %>% .$times %>% 
    as.POSIXct(format = '%b %d %I:%M %p', tz= "America/New_York")
  interp_q <- function(x,y){
    approx(x, y, xout = times)$y %>% grab_spark
  }
  sparks <- group_by(disch, site_no) %>% filter(min(dateTime) <= times[2], max(dateTime) >= tail(times, 2L)[2]) %>% 
    summarize(points = interp_q(dateTime, Flow_Inst))
  saveRDS(sparks, viz[['location']])
}