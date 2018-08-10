vms_import_data_sl <- function(file.name) {

  d <- rio::import(file.name, setclass="tibble") %>%
    dplyr::rename(vid = PublicDeviceID,
                  date = gpsTime, # Check
                  lon = Lon,
                  lat = Lat,
                  speed = Speed) %>%
    dplyr::mutate(lat = as.numeric(stringr::str_replace(lat,  ",",  ".")),
                  lon = as.numeric(stringr::str_replace(lon,  ",",  ".")),
                  date = stringr::str_sub(date, 1, 19),
                  date = lubridate::ymd_hms(date),
                  speed = as.numeric(speed) * 3600/185200) %>% #,
                  #activity = dplyr::case_when(speed < speed.min ~ paste0("<", speed.min),
                  #                            speed < speed.max ~ paste0(speed.min, "-", speed.max),
                  #                            TRUE ~ paste0(">", speed.max)))%>%
    dplyr::arrange(vid, date)
  return(d)
}