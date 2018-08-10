#' Title
#'
#' @param d A tidy vms dataframe containing variables "lon" and "lat"
#' @param lon.lim A vector containing lower and upper longitudes to plot
#' @param lat.lim A vector containing lower and upper latitudes to plot
#'
#' @export
#'
vms_plot_raster <- function(d, lon.lim, lat.lim) {

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

  m <-
    ggplot2::map_data("worldHires",
                      xlim = lon.lim,
                      ylim = lat.lim)

  p <-
    ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::geom_raster(data = d,
                         ggplot2::aes(x = lon, y = lat, fill = n), alpha=0.8) +
    ggplot2::geom_polygon(data = m, ggplot2::aes(long, lat, group = group), fill = "grey") +
    ggplot2::coord_quickmap(xlim = lon.lim,  ylim = lat.lim) +
    viridis::scale_fill_viridis(option = "B", direction = -1) +
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL) +
    ggplot2::labs(title = "Fishing effort")

  return(p)

}