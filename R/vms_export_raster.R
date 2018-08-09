#' @export
vms_export_raster <- function(d, file.name = "data-products/Effort_raster.asc") {

  # Convert first two columns as lon-lat and third as value
  r <- raster::rasterFromXYZ(d)
  # NOTE: Check the projection specs, think they need to be more specific
  raster::projection(r) = sp::CRS("+proj=longlat")

  raster::writeRaster(file = file.name,
                      format = "ascii",
                      overwrite = TRUE)

}