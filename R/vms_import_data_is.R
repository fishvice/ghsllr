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
                  speed = speed * 3600/1852) %>%
    dplyr::arrange(vid, date) #%>%

    return(d)
}
