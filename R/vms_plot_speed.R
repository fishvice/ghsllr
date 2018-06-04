#' Histogram of speed limits
#'
#' A wrapper for a histogram plot. It is expected that the dataframe contains variable labelled "speed". User can control the bindwidth and cut off
#' inplausable speed in the plot
#'
#' @param d A standardized VMS data frame
#' @param bindwidth The binwidth of the histogram
#' @param speed.limit A value that indicates maximum plausable speed. Any speed
#' higher than this valule will not be plotted
#'
#' @return dataframe
#' @export
#'
vms_plot_speed <- function(d, bindwidth = 0.5, speed.limit = 15) {

  d %>%
    dplyr::select(speed, fishing) %>%
    dplyr::mutate(fishing = ifelse(fishing, "Fishing", "Not fishing")) %>%
    tidyr::drop_na() %>%
    ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::geom_histogram (ggplot2::aes(x=speed, fill = fishing), binwidth = 0.5) +
    ggplot2::xlim(0, speed.limit)
}