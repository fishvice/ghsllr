#' Filter standardized vms dataframe
#'
#' Take a standarized vms dataframe and filters data not within any of the
#' specified boundarise of longitude, latitude, date and speed. The user can
#' specify the filter on one variable or all. Those not specified will not be
#' affected.
#'
#' @param d Standardized vms data frame
#' @param lon.lim A vector of two value, the lower and upper longitudinal range
#' to be retained.
#' @param lat.lim A vector of two value, the lower and upper latitudinal range
#' to be retained.
#' @param date.lim A vector of two value, the lower and upper date range
#' to be retained. The vector has to be on the format of c("yyyy-mm-dd hh:mm:ss",
#' "yyyy-mm-dd hh:mm:ss")
#' @param speed.lim A vector of two value, the lower and upper speed range
#' to be retained.
#'
#' @return A dataframe
#' @export
vms_filter_data <- function(d, lon.lim, lat.lim, date.lim, speed.lim) {

  if(!missing(lon.lim)) {
    d <-
      d %>%
      dplyr::filter(dplyr::between(lon, lon.lim[[1]], lon.lim[[2]]))
  }

  if(!missing(lat.lim)) {
    d <-
      d %>%
      dplyr::filter(dplyr::between(lat, lat.lim[[1]], lat.lim[[2]]))
  }

  if(!missing(date.lim)) {
    t1 <- lubridate::ymd_hms(date.lim[[1]])
    t2 <- lubridate::ymd_hms(date.lim[[2]])
    d <-
      d %>%
      dplyr::filter(dplyr::between(date, t1, t2))
  }

  if(!missing(speed.lim)) {
    d <-
      d %>%
      dplyr::filter(dplyr::between(speed, speed.lim[[1]], speed.lim[[2]]))
  }

  return(d)

}