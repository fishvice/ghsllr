#' @export
vms_import_data_is <- function(file.name) {
  d <-
    readr::read_csv(file.name) %>%
    dplyr::rename(lon = poslon,
                  lat = poslat,
                  vid = mobileid,
                  date = posdate) %>%
    dplyr::mutate(lon = lon * 45 / atan(1),
                  lat = lat * 45 / atan(1),
                  heading = heading * 45 / atan(1),
                  speed = speed * 1.852) %>%
    dplyr::arrange(vid, date) #%>%
    #dplyr::mutate(activity = dplyr::case_when(speed < speed.min ~ paste0("<", speed.min),
    #                                          speed < speed.max ~ paste0(speed.min, "-", speed.max),
    #                                          TRUE ~ paste0(">", speed.max)))
  return(d)
}