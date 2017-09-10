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
  
  #get stage or discharge forecast?
  site_content <- getContentInfo('sites')
  pCode <- site_content[['pCode']]
  if(pCode == "00060") {
    nws_node <- "secondary"
  } else if(pCode == "00065") {
    nws_node <- "primary"
  }
  
  forecastDF <- data.frame()
  
  if(viz[["refetch"]] || !file.exists(viz[['location']])){
    for(i in sites$NWS){
      url.site <- paste0("https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=",i,"&output=xml")
      print(i)
      return_list <- GET(url.site)
      returnedDoc <- content(return_list,encoding = "UTF-8")
      nws.site <- xml_root(returnedDoc)
      sigstages <- xml_find_all(nws.site, "sigstages")
      
      if(length(sigstages) > 0){
        sites$flood.stage[which(sites$NWS %in% i)] <- as.numeric(xml_text(xml_find_all(sigstages, "flood")))
        sites$flood.stage.units[which(sites$NWS %in% i)] <- xml_attr(xml_find_all(sigstages, "flood"),"units")
      }
      
      forecast <- xml_find_all(nws.site, "forecast")
      if(length(xml_children(forecast)) == 0) {
        #not all sites have forecasts
        next
      } else {
        datum_children <- xml_find_all(xml_children(forecast), nws_node)
        assertthat::assert_that(length(unique(xml_attr(datum_children, "units"))) == 1)
        forecast_site <- data.frame(forecast_vals = xml_text(datum_children), 
                                    site = i)
        forecastDF <- bind_rows(forecastDF, forecast_site)
      }
    }
    sites <- sites %>% filter(!is.na(flood.stage))
    sites_forecast_list <- list(sites = sites, forecasts = forecastDF)
    saveRDS(sites_forecast_list, file=viz[['location']])
  } else {
    return(NULL)
  }
  
}