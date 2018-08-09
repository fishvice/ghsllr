#' Title
#'
#' @param d XXX
#' @param res XXX
#' @param lon.min XXX
#' @param lon.max XXX
#' @param lat.min XXX
#' @param lat.max XXX
#'
#' @export
#'
vms_compute_effort <- function(d, res,
                               lon.min = -179, lon.max = 179,
                               lat.min = -89,  lat.max = 89)
{

  if(missing(res)) stop("You need to provide the resolution")

  d <- d %>%
    tidyr::drop_na() %>%
    #dplyr::filter(speed >= speed.min,
    #              speed <= speed.max) %>%
    dplyr::filter(fishing) %>%
    dplyr::select(lon, lat)

  # Convert to SpatialPoints
  sp::coordinates(d) <- c("lon", "lat")
  sp::proj4string(d) <- "+proj=longlat"

  # Rasterization
  rst <-
    raster::raster(xmn = lon.min,
                   xmx = lon.max,
                   ymn = lat.min,
                   ymx = lat.max,
                   res = res,
                   crs = "+proj=longlat")

  rst <- raster::rasterize(d, rst, field = 1, fun = "count")

  return(rst)
}

