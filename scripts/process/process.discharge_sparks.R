
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
  
  sites <- depends[["sites"]][c("site_no", "dec_lat_va")]
  
  sparks <- sparks %>% 
    left_join(sites, by = "site_no") %>%
    arrange(desc(dec_lat_va)) %>%
    select(-site_no, -dec_lat_va) 
  
  sparks <- unique(sparks)
  
  saveRDS(sparks, viz[['location']])
}

# Should rename at some point. Not always going to be discharge.
process.flood_sparks <- function(viz = as.viz('flood-sparks')){
  library(dplyr)
  depends <- readDepends(viz)
  checkRequired(depends, c("timestep-discharge", "sites", "flood-sites-classify"))
  
  
  ids <- names(depends[["timestep-discharge"]])
  site.nos <- sapply(ids, function(x) strsplit(x, '[-]')[[1]][2], USE.NAMES = FALSE)
  
  flood.data <- depends$`flood-sites-classify`@data %>% filter(id %in% names(depends[["timestep-discharge"]]))
  num.times <- length(depends$`timestep-discharge`[[1]]$y)
  
  grab_clip_path <- function(class){
    flags <- as.numeric(strsplit(class, 'f-')[[1]])
    flags <- flags[!is.na(flags)]
    138.24/num.times
    buf <- 5.33
    tot.w <- 144 #px
    
    x = seq(buf/2, to = tot.w-buf/2, length.out = num.times)
    if (length(flags) == 0){
      return(x)
    } else {
      return(paste(x[!flags], sep=' '))
    }
    
  }
  
  
  sparks <- data.frame(points = sapply(depends[["timestep-discharge"]], function(x) grab_spark(x$y), USE.NAMES = FALSE),
                       site_no = site.nos, id = ids, stringsAsFactors = FALSE) %>% 
    left_join(depends$`flood-sites-classify`@data) %>% 
    mutate(clip.x = grab_clip_path(class)) %>% select(clip.x)
    mutate(class = "sparkline", 
           id = sprintf("sparkline-%s", site_no), 
           style = "mask: url(#spark-opacity);",
           onmouseover=sprintf("setBold('nwis-%s');", site_no), 
           onmouseout=sprintf("setNormal('nwis-%s');hovertext(' ');", site_no),
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