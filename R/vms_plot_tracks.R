#' Title
#'
#' @param d XXX
#'
#' @export
#'

vms_plot_tracks <- function(d){

  m <- ggplot2::map_data("worldHires",
                xlim = c(lon.min, lat.max),
                ylim = c(lat.min, lat.max))

  ids <- unique(d$vid)

  for(i in ids){

    plot <-
      d %>%
      dplyr::filter(vid == i) %>%
      ggplot2::ggplot() +
      ggplot2::theme_bw(base_size = 16) +
      ggplot2::geom_polygon(data = m, ggplot2::aes(long, lat, group = group), fill = "grey") +
      ggplot2::geom_path(ggplot2::aes(lon, lat), colour = "grey") +
      ggplot2::geom_point(ggplot2::aes(lon, lat, colour = speed, shape=activity), size = 1) +
      viridis::scale_colour_viridis(option = "B", direction = -1) +
      ggplot2::coord_quickmap(xlim = c(lon.min, lon.max),  ylim = c(lat.min, lat.max)) +
      ggplot2::scale_x_continuous(NULL) +
      ggplot2::scale_y_continuous(NULL) +
      ggplot2::ggtitle(stringr::str_c("Vessel ID:", i))

    print(plot)

  }
}




