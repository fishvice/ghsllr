#' Calculates speed between pings
#'
#' The function calculates the speed between consequtive pings for each
#' vessel. If either a variable distance or duration or both is not available
#' it gets calculated from the data.
#'
#' @param d A standardize vms dataframe
#'
#' @return A dataframe with a new variable speed (in units of nautical miles
#' per hour)
#' @export
#'
vms_calculate_speed <- function(d) {

  if(!"distance" %in% names(d)) d <- vms_calculate_distance(d)
  if(!"duration" %in% names(d))     d <- vms_calculate_duration(d)

  d %>%
    dplyr::mutate(dspeed = distance / duration) %>%
    dplyr::ungroup()

}