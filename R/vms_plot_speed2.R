#' Histogram of speed limits
#'
#' A wrapper for a histogram plot. It is expected that the dataframe contains variable labelled "speed". User can control the bindwidth and cut off
#' inplausable speed in the plot
#'
#' @param d A standardized VMS data frame
#' @param speed.min XXX
#' @param speed.max XXX
#' @param speed.limit A value that indicates maximum plausable speed. Any speed
#' higher than this valule will not be plotted
#' @param bindwidth The binwidth of the histogram
#'
#' @return dataframe
#'
vms_plot_speed2 <- function(d,
                           speed.min = 2,
                           speed.max = 4.5,
                           speed.limit = 15,
                           bindwidth = 0.5) {

  d %>%
    mutate(fishing = ifelse(between(speed, speed.min, speed.max), TRUE, FALSE)) %>%
    dplyr::select(speed, fishing) %>%
    dplyr::mutate(fishing = ifelse(fishing, "Fishing", "Not fishing")) %>%
    tidyr::drop_na() %>%
    ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::geom_histogram (ggplot2::aes(x=speed, fill = fishing), binwidth = 0.5) +
    ggplot2::xlim(0, speed.limit) +
    ggplot2::scale_fill_brewer(palette = "Set1")
}