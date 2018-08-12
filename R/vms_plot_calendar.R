vms_plot_calendar <- function(d, ping.max = Inf) {

  d %>%
    dplyr::select(date, vid, speed) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::group_by(date, vid) %>%
    dplyr::count() %>%
    dplyr::mutate(n = ifelse(n > ping.max, ping.max, n)) %>%
    dplyr::group_by(vid) %>%
    dplyr::mutate(min.date = min(date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vid = as.character(vid)) %>%
    ggplot2::ggplot(ggplot2::aes(date, vid, fill = n)) +
    ggplot2::geom_raster() +
    ggplot2::scale_x_date(date_minor_breaks = "1 month") +
    ggplot2::scale_fill_viridis_c(option = "B", direction = -1) +
    ggplot2::labs(x = NULL, y = "Vessel id", fill = "Number of pings",
                  title = "Records per vessel per day")

}