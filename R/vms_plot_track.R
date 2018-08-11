#' Title
#'
#' @param d A standardized vms dataframe
#' @param lon.lim A vector specifying the lower and upper longitude boundaries
#' to be plotted.
#' @param lat.lim A vector specifying the lower and upper latitudinal boundaries
#' to be plotted.
#'
#' @export
#'

vms_plot_track <- function(d, lon.lim, lat.lim) {

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

  m <- ggplot2::map_data("worldHires",
                         xlim = lon.lim,
                         ylim = lat.lim)

  d <-
    d %>%
    dplyr::mutate(speed = dplyr::if_else(speed > 12, 12, speed, NA_real_))

  p <-
    d %>%
    ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::geom_polygon(data = m, ggplot2::aes(long, lat, group = group), fill = "grey") +
    ggplot2::geom_path(ggplot2::aes(lon, lat), colour = "grey")

  if("activity" %in% names(d)) {
    p <-
      p +
      ggplot2::geom_point(ggplot2::aes(lon, lat, colour = speed, shape = activity), size = 1)
  } else {
    p <-
      p +
      ggplot2::geom_point(ggplot2::aes(lon, lat, colour = speed))
  }

  p <-
    p +
    viridis::scale_colour_viridis(option = "B", direction = -1) +
    ggplot2::coord_quickmap(xlim = lon.lim,  ylim = lat.lim) +
    ggplot2::scale_x_continuous(NULL) +
    ggplot2::scale_y_continuous(NULL)

  return(p)
}




