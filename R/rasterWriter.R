#' Function for generating a raster from a shapefile
#' 
#' @param shp a shape file to be converted to raster
#' @param res the resolution of the output raster
#' @param rastOut an string specifying the output name for the raster generated
#' @return a raster file with the extension .asc

rasterWriter <- function(shp, res, rastOut) {
  # read the shapefile
  shp <- maptools::readShapeSpatial(shp)
  # define some raster params
  num_col <- round((raster::extent(shp)@xmax - raster::extent(shp)@xmin)/res)
  num_row <- round((raster::extent(shp)@ymax - raster::extent(shp)@ymin)/res)
  # create a raster obj
  r <- raster::raster(raster::extent(shp), ncol = num_col, nrow = num_row)
  # define raster resolution
  raster::res(r) <- c(res, res)
  # generate raster
  rast <- raster::rasterize(x = shp, y = r)
  # fix missing values that are not integers
  #rast[rast[] !]
  # write the raster
  raster::writeRaster(rast, paste(rastOut, ".asc", sep = ""), 
                      overwrite = TRUE)
}