readData.shp <- function(viz, idx = 1){
  library(rgdal)
  shp.path <- file.path(tempdir(), 'tmp')
  if (!dir.exists(shp.path)){
    dir.create(shp.path)
  }
  unzip(viz[['location']], exdir = shp.path)
  layer <- tools::file_path_sans_ext(list.files(shp.path, pattern='*.shp'))[idx]
  data.out = readOGR(shp.path, layer=layer)
  unlink(shp.path, recursive = TRUE)
  return(data.out)
}
