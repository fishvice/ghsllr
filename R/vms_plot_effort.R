#' Title
#'
#' @param rs XXX
#'
#' @export
#'
vms_plot_effort <- function(rs) {

  rs <- methods::as(rs, "SpatialPixelsDataFrame")
  rs <- as.data.frame(rs)
  colnames(rs) <- c("np", "x", "y")

  m <-
    ggplot2::map_data("worldHires",
                      xlim = c(lon.min, lon.max),
                      ylim = c(lat.min, lat.max))

  plot <-
    ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::geom_tile(data=rs, ggplot2::aes(x=x, y=y, fill = np), alpha=0.8) +
    ggplot2::geom_polygon(data = m, ggplot2::aes(long, lat, group = group), fill = "grey") +
    ggplot2::coord_quickmap(xlim = c(lon.min, lon.max),  ylim = c(lat.min, lat.max)) +
    viridis::scale_fill_viridis(option = "B", direction = -1) +
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL) +
    ggplot2::ggtitle("Fishing effort")

  print(plot)

}