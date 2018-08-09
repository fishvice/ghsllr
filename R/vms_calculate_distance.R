#' Calculates distance between pings
#'
#' The function calculates the distance between consequtive pings for each
#' vessel.
#'
#' @param d A standardize vms dataframe
#'
#' @return A dataframe with a new variable distance (in units of nautical miles)
#' @export
#'
vms_calculate_distance <- function(d) {

  d %>%
    dplyr::arrange(vid, date) %>%
    dplyr::group_by(vid) %>%
    dplyr::mutate(distance = arcdist(dplyr::lead(lat), dplyr::lead(lon), lat, lon)) %>%
    dplyr::ungroup()

}