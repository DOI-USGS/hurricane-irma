fetch.nws_data <- function(viz=as.viz("nws-data")){

  library(dplyr)
  library(xml2)
  library(httr)
  
  depend.data <- readDepends(viz)
  checkRequired(depend.data, c('sites','nws-conversion'))
  sites <- depend.data[["sites"]]
  nws_conversion <- depend.data[["nws-conversion"]]

  sites <- left_join(sites, 
                     select(nws_conversion, NWS, USGS), 
                     by = c("site_no"="USGS")) %>% 
    filter(!is.na(NWS))
  
  sites$flood.stage <- NA
  sites$flood.stage.units <- NA
  
  if(viz[["refetch"]] || !file.exists(viz[['location']])){
    for(i in sites$NWS){
      url.site <- paste0("https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=",i,"&output=xml")
      
      return_list <- GET(url.site)
      returnedDoc <- content(return_list,encoding = "UTF-8")
      nws.site <- xml_root(returnedDoc)
      sigstages <- xml_find_all(nws.site, "sigstages")
      
      if(length(sigstages) > 0){
  
        sites$flood.stage[which(sites$NWS %in% i)] <- as.numeric(xml_text(xml_find_all(sigstages, "flood")))
        sites$flood.stage.units[which(sites$NWS %in% i)] <- xml_attr(xml_find_all(sigstages, "flood"),"units")
      }
    }
  }
  sites <- sites %>% filter(!is.na(flood.stage))
  location <- viz[['location']]
  saveRDS(sites, file=location)
}