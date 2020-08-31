max_dim <- function(feature) {
  bbox <- st_bbox(feature)
  
  length <- bbox$xmax - bbox$xmin
  width <- bbox$ymax - bbox$ymin
  
  if(length > width) {
    return(unname(c(bbox$xmin, bbox$xmax)))
  } else {
    return(unname(c(bbox$ymin, bbox$ymax)))
  }
}
