
#' clip/reduce the actual states that are used, add a data.frame to them that will be used by `visualize`
#' 
#' @param
#' @param
process.storm_states <- function(viz = as.viz('storm-states')){
  sp <- readDepends(viz)[['states']]
  add_svg_data(viz, sp, class.name = 'state-polygon')
}

add_svg_data <- function(viz, sp, class.name){
  library(dplyr)
  data.out <- data.frame(id = NA_character_, class = rep(class.name, length(sp)), raw.name = names(sp), stringsAsFactors = FALSE) %>% 
    mutate(id = gsub(" ", "_", raw.name)) %>% select(id, class)
  
  row.names(data.out) <- row.names(sp)
  sp.data.frame <- as(object = sp, Class = paste0(class(sp), "DataFrame"))
  sp.data.frame@data <- data.out
  
  saveRDS(sp.data.frame, viz[['location']])
}

