vms_import_data_gh <- function(file.name) {

  d <-
    rio::import(file.name, setclass="tibble") %>%
    dplyr::rename(vid = Mobile,
                  date = Date,
                  lon = Longitude,
                  lat = Latitude,
                  speed = Speed) %>%
    dplyr::mutate(date = lubridate::dmy_hm(date),
                  lon = as.numeric(lon),
                  lat = as.numeric(lat),
                  speed = as.numeric(speed)) %>% #,
                  #activity = dplyr::case_when(speed < speed.min ~ paste0("<", speed.min),
                  #                            speed < speed.max ~ paste0(speed.min, "-", speed.max),
                  #                            TRUE ~ paste0(">", speed.max))) %>%
    dplyr::arrange(vid, date)

  return(d)

}