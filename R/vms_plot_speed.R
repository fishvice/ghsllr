#' Histogram of speed limits
#'
#' A wrapper for a histogram plot. It is expected that the dataframe contains
#' variable labelled "speed". User can control the bindwidth and cut off
#' inplausable speed in the plot. If varible "fishing" is available in the
#' input dataframe the histogram is plotted with colour coding of activity.
#'
#' @param d A standardized VMS data frame
#' @param bindwidth The binwidth of the histogram, default is set to 0.5
#' @param max.speed A value that indicates maximum plausable speed. Any speed
#' higher than this value will be assigned to this value and appear as such
#' in the plot
#'
#' @return a ggplot2 object
#' @export
#'
vms_plot_speed <- function(d, bindwidth = 0.5, max.speed = 15) {

  if("fishing" %in% names(d)) {
    p <-
      d %>%
      dplyr::select(speed, fishing) %>%
      dplyr::mutate(fishing = ifelse(fishing, "Fishing", "Not fishing"),
                    speed = ifelse(speed > max.speed, max.speed, speed)) %>%
      tidyr::drop_na() %>%
      ggplot2::ggplot() +
      ggplot2::theme_bw(base_size = 16) +
      ggplot2::geom_histogram (ggplot2::aes(x=speed, fill = fishing), binwidth = bindwidth)
  } else {
    p <-
      d %>%
      dplyr::select(speed) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(speed = ifelse(speed > max.speed, max.speed, speed)) %>%
      ggplot2::ggplot() +
      ggplot2::theme_bw(base_size = 16) +
      ggplot2::geom_histogram (ggplot2::aes(x=speed), binwidth = bindwidth)
  }
  return(p)
}