vms_import_data_lr <- function(file.name) {
  d <-
    rio::import(file.name, setclass="tibble") %>%
    dplyr::rename(date = DateTime, # or gpsTime???
                  lon = Longitude,
                  lat = Latitude,
                  speed = "Speed (MPH)") %>%
    dplyr::mutate(date = lubridate::ymd_hms(date),
                  lat = as.numeric(lat),
                  lon = as.numeric(lon)) %>% #,
                  #activity = dplyr::case_when(speed < speed.min ~ paste0("<", speed.min),
                  #                            speed < speed.max ~ paste0(speed.min, "-", speed.max),
                  #                           TRUE ~ paste0(">", speed.max))) %>%
    dplyr::arrange(vid, date)
  return(d)
}