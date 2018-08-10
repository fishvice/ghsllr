#' @title Export rasterized data as ascii
#'
#' @description The function converts a rasterized dataframe into a raster
#' object that is then exported as an ascii text file. That file can be
#' imported into QGIS.
#'
#' @param d A standardized VMS tibble containing variables lon, lat and n
#' @param file.name The path and the filename of the ascii raster textfile to
#' be exported.
#'
#' @return NULL
#' @export
vms_export_raster <- function(d, file.name = "data-products/Effort_raster.asc") {

  # Convert first two columns as lon-lat and third as value
  r <-
    d %>%
    dplyr::select(lon, lat, n) %>%
    tidyr::drop_na() %>%
    raster::rasterFromXYZ()
  # NOTE: Check the projection specs, think they need to be more specific
  raster::projection(r) = sp::CRS("+proj=longlat")

  raster::writeRaster(r,
                      file = file.name,
                      format = "ascii",
                      overwrite = TRUE)

}