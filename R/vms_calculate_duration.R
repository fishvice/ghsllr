#' Calculates durations between pings
#'
#' The function calculates the duration between consequtive pings for each
#' vessel.
#'
#' @param d A standardize vms dataframe
#'
#' @return A dataframe with a new variable duration (in units hours)
#' @export
#'
vms_calculate_duration <- function(d) {

  d %>%
    dplyr::arrange(vid, date) %>%
    dplyr::group_by(vid) %>%
    dplyr::mutate(duration = as.numeric(lubridate::as.duration(dplyr::lead(date) - date), "hours")) %>%
    dplyr::ungroup()

}