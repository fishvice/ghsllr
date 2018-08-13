vms_plot_activity <- function(d) {

  if("activity" %in% names(d)) {
    p <-
      d %>%
      dplyr::mutate(date = lubridate::as_date(date)) %>%
      dplyr::group_by(date, activity) %>%
      dplyr::count() %>%
      ggplot2::ggplot() +
      ggplot2::geom_linerange(ggplot2::aes(date, ymin = 0, ymax = n)) +
      ggplot2::facet_grid(activity ~ .) +
      ggplot2::scale_x_date(date_minor_breaks = "1 month") +
      ggplot2::labs(x = NULL,
                    y = "Number of pings",
                    title = "Records per day")
    return(p)
  } else {
    message("Variable activity not in dataframe, run vms_categorise_fishing fist")
    return(NULL)
  }

}
