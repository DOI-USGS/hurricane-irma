#' Pull the shapefile(s) for a hurricane and writes the file(s) to disk
#' viz fields:
#' ocean: al or ep
#' stormnum: 01-n
#' year: 2016
fetch.hurricaneTrack <- function(viz) {
  library(httr)
  library(magrittr)
  required <- c("location", "ocean", "stormnum", "year")
  checkRequired(viz, required)
  
  nhc.url <- "http://www.nhc.noaa.gov/gis/best_track/%s%s%s_best_track.zip"
  download.url <- sprintf(nhc.url, viz[['ocean']], viz[['stormnum']], viz[['year']])
  setupFoldersForFile(file = viz[['location']])
  # a bit more work for DRY
  fetchviz <- list(
    location = viz[['location']],
    remoteURL = download.url,
    fetcher = "url"
  ) %>%
    as.viz() %>%
    as.fetcher()
  fetch(fetchviz)
}