#' Title
#'
#' @param d XXX
#' @param grid.lim XXX
#'
#' @export
#'
vms_rasterize <- function(d, grid.lim) {

  if(missing(grid.lim)) stop("You need to provide the resolution")
  if(length(grid.lim) == 1) {
    dx <- dy <- grid.lim
  } else {
    dx <- grid.lim[[1]]
    dy <- grid.lim[[2]]
  }


  has.country <- ifelse("country" %in% names(d), TRUE, FALSE)

  if(has.country) {
    d <-
      d %>%
      dplyr::select(lon, lat, country) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(lon = grade(lon, dx),
                    lat = grade(lat, dy)) %>%
      dplyr::group_by(country, lon, lat) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      select(lon, lat, n, country)
  } else {
    d <-
      d %>%
      dplyr::select(lon, lat) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(lon = grade(lon, dx),
                    lat = grade(lat, dy)) %>%
      dplyr::group_by(lon, lat) %>%
      dplyr::count() %>%
      dplyr::ungroup()
  }

  return(d)

}

