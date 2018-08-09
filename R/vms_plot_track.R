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

  if(missing(lon.lim)) lon.lim <- range(d$lon, na.rm = TRUE)
  if(missing(lat.lim)) lat.lim <- range(d$lat, na.rm = TRUE)

  m <- ggplot2::map_data("worldHires",
                         xlim = lon.lim,
                         ylim = lat.lim)

  ids <- unique(d$vid)

  p <-
    d %>%
    ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::geom_polygon(data = m, ggplot2::aes(long, lat, group = group), fill = "grey") +
    ggplot2::geom_path(ggplot2::aes(lon, lat), colour = "grey") +
    ggplot2::geom_point(ggplot2::aes(lon, lat, colour = speed, shape = activity), size = 1) +
    viridis::scale_colour_viridis(option = "B", direction = -1) +
    ggplot2::coord_quickmap(xlim = lon.lim,  ylim = lat.lim) +
    ggplot2::scale_x_continuous(NULL) +
    ggplot2::scale_y_continuous(NULL)

  return(p)
}




