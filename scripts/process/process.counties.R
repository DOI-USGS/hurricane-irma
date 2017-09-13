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
  counties <- depends[['counties']]
  epsg_code <- getContentInfo('view-limits')[['proj.string']]
  
  storm_area_filter <- depends[['storm-area-filter']] %>% sp::spTransform(epsg_code) %>% 
  rgeos::gBuffer(width=200000, byid=TRUE )
  
  counties <- sp::spTransform(counties, sp::CRS(epsg_code))
  
  overlap <- rgeos::gContains(storm_area_filter, counties, byid = TRUE) %>% rowSums() %>% as.logical()
  counties <- counties[overlap, ]
  
  library(dplyr)
  county_ids <- sapply(slot(counties, "polygons"), function(x) slot(x, "ID"))
  data.out <- data.frame(id = NA_character_, 
                         class = rep('county-polygon', length(county_ids)), 
                         polyname = county_ids, 
                         onmouseout = "hovertext(' ');", 
                         stringsAsFactors = FALSE) %>% 
    mutate(id = county_map_name_2_id(polyname)) %>% 
    mutate(onmousemove = sprintf("hovertext('%s',evt);", county_map_name_2_mouser(polyname))) %>%  
    select(-polyname) 
  
  row.names(data.out)  <- county_ids
  
  counties <- sp::SpatialPolygonsDataFrame(counties, data.out)
  
  saveRDS(counties, viz[['location']])
}

