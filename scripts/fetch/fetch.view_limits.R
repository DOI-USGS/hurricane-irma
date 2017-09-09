fetch.view_limits <- function(viz = as.viz('view-limits')){
  bbox <- viz[['bbox']]
  sp.bbox <- as.sp_box(xs = bbox[c(1,3)], ys = bbox[c(2,4)], proj.string = sp::CRS("+proj=longlat +datum=WGS84"))
  bbox.transformed <- sp::spTransform(sp.bbox, viz[['proj.string']])
  
  view.limits <- get_sp_lims(bbox.transformed, # it would be nice to just use defaults if these aren't included:
                             width = viz[['width']], height = viz[['height']], pointsize = viz[['pointsize']])
  
  out <- append(view.limits, list(proj.string = viz[['proj.string']],
    width = viz[['width']], height = viz[['height']], pointsize = viz[['pointsize']]))
  
  saveRDS(out, viz[['location']])
}

fetch.storm_area_filter <- function(viz = as.viz('storm-area-filter')) {
  pointsVec <- as.numeric(c(lats = viz[['lons']], lons = viz[['lats']]))
  pointsMatrix <- matrix(pointsVec, nrow = length(pointsVec)/2, ncol = 2)
  poly <- sp::SpatialPolygons(list(
    sp::Polygons(list(sp::Polygon(pointsMatrix)), ID =1 )), 
    pO = 1L, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  saveRDS(poly, viz[['location']])
}