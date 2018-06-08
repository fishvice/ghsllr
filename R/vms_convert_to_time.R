#' Title
#'
#' @param x Character vector representing time
#'
#' @return Time vector
#' @export
#'
vms_convert_to_time <- function(x) {

  dplyr::data_frame(time.txt = x) %>%
    dplyr::mutate(year.first = !is.na(as.integer(stringr::str_sub(time.txt, 1, 4))),
                  char.first = stringr::str_sub(time.txt, 1, 1) %>% as.integer(),
                  char.first = ifelse(is.na(char.first), TRUE, FALSE)) %>%
    dplyr::mutate(time = dplyr::case_when(year.first ~ lubridate::ymd_hms(time.txt, truncated = 5),
                                          char.first ~ lubridate::mdy_hms(time.txt, truncated = 5),
                                          TRUE ~ lubridate::dmy_hms(time.txt, truncated = 5))) %>%
    dplyr::pull(time)

}
