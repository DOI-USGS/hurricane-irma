readData.json <- function(viz){
  jsonlite::fromJSON(viz[['location']])
}