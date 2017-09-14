
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
  checkRequired(depends, c("timestep-discharge", "sites"))
  
  site.nos <- site.nos <- sapply(names(depends[["timestep-discharge"]]), 
                                 function(x) strsplit(x, '[-]')[[1]][2], USE.NAMES = FALSE)
  
  sparks <- data.frame(points = sapply(depends[["timestep-discharge"]], function(x) grab_spark(x$y), USE.NAMES = FALSE),
                       site_no = site.nos, stringsAsFactors = FALSE) %>% 
    mutate(class = "sparkline", 
           id = sprintf("sparkline-%s", site_no), 
           style = "mask: url(#spark-opacity);",
           onmouseover=sprintf("setBold('nwis-%s');", site_no), 
           onmouseout=sprintf("setNormal('nwis-%s');hovertext(' ');", site_no),
           onclick=sprintf("openNWIS('%s', evt);", site_no),
           onmousemove=sprintf("hovertext('USGS %s',evt);", site_no))
  
  sites <- depends[["sites"]]@data[c("site_no", "dec_lat_va")]
  
  sparks <- sparks %>% 
    left_join(sites, by = "site_no") %>%
    arrange(desc(dec_lat_va)) %>%
    select(-site_no, -dec_lat_va) 
  
  sparks <- unique(sparks)
  
  saveRDS(sparks, viz[['location']])
}

grab_clip_rect <- function(vals, flood){
  
  x = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(vals, type='l', axes=F, ann=F)
    abline(h = flood)
  }, height=0.4, width=2)
  y.out <- xml2::xml_attr(xml2::xml_find_first(x, '//*[local-name()="line"]'),'y1')
  if (is.na(y.out)){
    return("0")
  } else {
    return(y.out)
  }
}

# Should rename at some point. Not always going to be discharge.
process.flood_sparks <- function(viz = as.viz('flood-sparks')){
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c("timestep-discharge", "sites", "nws-data"))
  
  nws_data <- depends[["nws-data"]]$sites
  ids <- names(depends[["timestep-discharge"]])
  site.nos <- sapply(ids, function(x) strsplit(x, '[-]')[[1]][2], USE.NAMES = FALSE)

  
  sparks <- data.frame(site_no = site.nos, id = ids, stringsAsFactors = FALSE)
  for (i in 1:length(sparks$site_no)){
    site <- sparks$site_no[i]
    flood <- filter(nws_data, site_no == site) %>% .$flood.stage %>% .[1]
    sparks$y[i] = grab_clip_rect(depends[["timestep-discharge"]][[sparks$id[i]]]$y, flood)
  }
  sparks <- sparks %>% 
    mutate(class = "floodline", 
           id = sprintf("floodline-%s", site_no), 
           style = "mask: url(#flood-opacity);",
           "clip-path"=sprintf("url(#flood-clip-%s)", site_no), 
           onmouseover=sprintf("setBold('nwis-%s');setBold('sparkline-%s');", site_no, site_no),
           onmouseout=sprintf("setNormal('nwis-%s');setNormal('sparkline-%s');hovertext(' ');", site_no, site_no),
           onclick=sprintf("openNWIS('%s', evt);", site_no),
           onmousemove=sprintf("hovertext('USGS %s',evt);", site_no))
  
  sites <- depends[["sites"]][c("site_no", "dec_lat_va")]
  
  sparks <- sparks %>% 
    left_join(sites, by = "site_no") %>%
    arrange(desc(dec_lat_va)) %>%
    select(-site_no, -dec_lat_va) 
  
  sparks <- unique(sparks)
  
  saveRDS(sparks, viz[['location']])
}