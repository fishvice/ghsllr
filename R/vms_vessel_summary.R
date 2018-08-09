#' @export
vms_vessel_summary <- function(d) {
  d %>%
    dplyr::mutate(day = lubridate::day(date)) %>%
    dplyr::group_by(vid) %>%
    dplyr::summarise(Date.minimum = min(date, na.rm = TRUE),
                     Date.maximum = max(date, na.rm = TRUE),
                     Date.range = lubridate::as_date(Date.maximum) - lubridate::as_date(Date.minimum) + 1,
                     Date.distinct = dplyr::n_distinct(day),
                     Date.missing = sum(is.na(date)),
                     Longitude.minimum = min(lon, na.rm = TRUE),
                     Longitude.maximum = max(lon, na.rm = TRUE),
                     Longitude.missing = sum(is.na(lon)),
                     Latitude.minimum = min(lat, na.rm = TRUE),
                     Latitude.maximum = max(lat, na.rm = TRUE),
                     Latitude.missing = sum(is.na(lat)),
                     Speed.minimum = min(speed, na.rm = TRUE),
                     Speed.maximum = max(speed, na.rm = TRUE),
                     Speed.missing = sum(is.na(speed)),
                     Vessel.distinct = dplyr::n_distinct(vid)) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::gather(variable, value, -vid) %>%
    tidyr::separate(variable, c("variable", "statistics")) %>%
    dplyr::arrange(vid)
}