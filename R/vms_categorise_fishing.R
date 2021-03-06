#' Categorizes fishing activity based on vessel speed
#'
#' Creates two new variables, "activity" indicating speed classification and
#' "fishing", a boolean variable where TRUE is considered fishing and FALSE
#' not fishing.
#'
#' @param d A standardized vms dataframe
#' @param fishing.lim A vector specifying the lower and uppper value of
#' instantaneous vessel speed that is classified as fishing.
#'
#' @return A dataframe with
#' @export
vms_categorise_fishing <- function(d, fishing.lim) {

  low <- fishing.lim[[1]]
  hig <- fishing.lim[[2]]
  d %>%
    dplyr::mutate(activity = vms_cut_speed(speed, fishing.lim),
                  fishing = ifelse(dplyr::between(speed, low, hig), TRUE, FALSE))

}
