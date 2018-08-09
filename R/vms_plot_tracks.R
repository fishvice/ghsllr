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




