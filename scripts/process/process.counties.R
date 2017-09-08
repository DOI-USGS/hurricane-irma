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
#' clip/reduce the actual counties that are used, add a data.frame to them that will be used by `visualize`
#' 
#' @param
#' @param
process.storm_counties <- function(viz = as.viz('storm-counties')){
  sp <- readDepends(viz)[['counties']]
  library(dplyr)
  data.out <- data.frame(id = NA_character_, class = rep('county-polygon', length(sp)), raw.name = names(sp), stringsAsFactors = FALSE) %>% 
    mutate(id = county_map_name_2_id(raw.name)) %>% select(id, class)
  
  row.names(data.out) <- row.names(sp)
  sp.data.frame <- as(object = sp, Class = paste0(class(sp), "DataFrame"))
  sp.data.frame@data <- data.out
  
  saveRDS(sp.data.frame, viz[['location']])
}