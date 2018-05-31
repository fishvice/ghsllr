#' Title
#'
#' @param d
#' @param bindwidth
#' @param lim
#'
#' @return dataframe
#' @export
#'
vms_plot_speed <- function(d, bindwidth = 0.5, lim = speed.limit) {

  d %>%
    dplyr::select(speed, activity) %>%
    dplyr::mutate(activity = ifelse(dplyr::between(speed, speed.min, speed.max),
                                    "Fishing", "Not fishing")) %>%
    tidyr::drop_na() %>%
    ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::geom_histogram (aes(x=speed, fill = activity), binwidth = 0.5) +
    ggplot2::xlim(0, speed.limit)
}