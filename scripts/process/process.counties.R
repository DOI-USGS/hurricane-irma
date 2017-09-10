#' convern default names from the maps package to svg ids
#'
#' @param names a character vector of "maps" names, following the pattern "state,county" 
county_map_name_2_id <- function(names){
  names.split <- unlist(strsplit(names, '[,]'))
  states <- names.split[seq(1, length(names.split), by=2)] %>% gsub(" ", "_", .)
  counties <- names.split[seq(2, length(names.split), by=2)] %>% gsub(" ", "_", .)
  names.out <- paste(states, counties, sep = '-')
  return(names.out)
}

county_map_name_2_mouser <- function(names){
  names.split <- unlist(strsplit(names, '[,]'))
  states <- names.split[seq(1, length(names.split), by=2)] 
  counties <- names.split[seq(2, length(names.split), by=2)]
  names.out <- as.character(sapply(paste(counties, states, sep = ', '), simpleCap))
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  s <- paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
  s <- strsplit(s, "-")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse="-")
}

#' clip/reduce the actual counties that are used, add a data.frame to them that will be used by `visualize`
#' 
#' @param
#' @param
process.storm_counties <- function(viz = as.viz('storm-counties')){
  library(dplyr)
  depends <- readDepends(viz)
  sp <- depends[['counties']]
  precip.classes <- depends[['precip-classify']]
  epsg_code <- getContentInfo('view-limits')[['proj.string']]
  footy <- depends[['storm-area-filter']] %>% sp::spTransform(epsg_code) %>% 
    rgeos::gBuffer(width=200000, byid=TRUE )
  counties <- sp::spTransform(sp, sp::CRS(epsg_code))
  overlap <- rgeos::gContains(footy, counties, byid = TRUE) %>% rowSums() %>% as.logical()
  sp <- sp[overlap, ]
  
  library(dplyr)
  data.out <- data.frame(id = NA_character_, 
                         base.class = rep('county-polygon', length(sp)), 
                         polyname = names(sp), 
                         onmouseout = "hovertext(' ');", 
                         stringsAsFactors = FALSE) %>% 
    left_join(precip.classes) %>% 
    mutate(id = county_map_name_2_id(polyname)) %>% mutate(class = paste0(base.class, class)) %>% 
    mutate(onmousemove = sprintf("hovertext('%s',evt);", county_map_name_2_mouser(polyname))) %>%  
    select(-polyname, -base.class) 
  
  row.names(data.out) <- row.names(sp)
  
  sp.data.frame <- as(object = sp, Class = paste0(class(sp), "DataFrame"))
  sp.data.frame@data <- data.out
  
  saveRDS(sp.data.frame, viz[['location']])
}

