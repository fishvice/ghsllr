#' Title
#'
#' @param d A standardize vms dataframe
#' @param lon.lim A vector specifying the lower and upper longitude boundaries
#' to be plotted.
#' @param lat.lim A vector specifying the lower and upper latitudinal boundaries
#' to be plotted.
#' @export
#'

vms_plot_tracks <- function(d, lon.lim, lat.lim) {

  has.country <- ifelse("country" %in% names(d), TRUE, FALSE)
  if(has.country) {
    country <- unique(d$country)[[1]]
    if(country %in% c("Ghana", "Sierra Leone", "Liberia", "Iceland")) {
      if(missing(lon.lim)) {
        if(country == "Ghana")        lon.lim <- c( -4,   2)
        if(country == "Sierra Leone") lon.lim <- c(-15, -12)
        if(country == "Liberia")      lon.lim <- c(-12, -10)
        if(country == "Iceland")      lon.lim <- c(-25, -14)
      }
      if(missing(lat.lim)) {
        if(country == "Ghana")        lat.lim <- c(  3,  7)
        if(country == "Sierra Leone") lat.lim <- c(  6, 10)
        if(country == "Liberia")      lat.lim <- c(5.5,  7)
        if(country == "Iceland")      lat.lim <- c( 63, 65)
      }
    }
  }

  if(missing(lon.lim)) lon.lim <- range(d$lon, na.rm = TRUE)
  if(missing(lat.lim)) lat.lim <- range(d$lat, na.rm = TRUE)

  vids <- unique(d$vid)

  for(i in vids){

    p <-
      d %>%
      dplyr::filter(vid == i) %>%
      vms_plot_track(lon.lim = lon.lim, lat.lim = lat.lim) +
      ggplot2::labs(title = stringr::str_c("Vessel ID:", i))

    print(p)

  }
}




