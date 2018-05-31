#' Title
#'
#' @param file.name
#' @param country
#'
#' @return dataframe
#' @export
#'
vms_import_data <- function(file.name, country)
{

  if(country == "Ghana") {

    d <- rio::import(file.name, setclass="tibble") %>%
      dplyr::rename(vid = Mobile,
                    date = Date,
                    lon = Longitude,
                    lat = Latitude,
                    speed = Speed) %>%
      dplyr::mutate(date = dmy_hm(date),
                    lon = as.numeric(lon),
                    lat = as.numeric(lat),
                    speed = as.numeric(speed),
                    activity = dplyr::case_when(speed < speed.min ~ paste0("<", speed.min),
                                                speed < speed.max ~ paste0(speed.min, "-", speed.max),
                                                TRUE ~ paste0(">", speed.max))) %>%
      dplyr::arrange(vid, date)
  }

  if(country == "Sierra Leone") {
    d <- import(file.name, setclass="tibble") %>%
      dplyr::rename(vid = PublicDeviceID,
                    date = ReceiveTime, # or gpsTime???
                    lon = Lon,
                    lat = Lat,
                    speed = Speed) %>%
      dplyr::mutate(lat = as.numeric(str_replace(lat,  ",",  ".")),
                    lon = as.numeric(str_replace(lon,  ",",  ".")),
                    date = str_sub(date, 1, 19),
                    date = ymd_hms(date),
                    speed = as.numeric(speed) * 3600/185200,
                    activity = case_when(speed < speed.min ~ paste0("<", speed.min),
                                         speed < speed.max ~ paste0(speed.min, "-", speed.max),
                                         TRUE ~ paste0(">", speed.max)))%>%
      dplyr::arrange(vid, date)
  }

  if(country == "Liberia") {
    d <-
      rio::import(file.name, setclass="tibble") %>%
      dplyr::rename(date = DateTime, # or gpsTime???
                    lon = Longitude,
                    lat = Latitude,
                    speed = "Speed (MPH)") %>%
      dplyr::mutate(date = ymd_hms(date),
                    lat = as.numeric(lat),
                    lon = as.numeric(lon),
                    activity = case_when(speed < speed.min ~ paste0("<", speed.min),
                                         speed < speed.max ~ paste0(speed.min, "-", speed.max),
                                         TRUE ~ paste0(">", speed.max))) %>%
      dplyr::arrange(vid, date)
  }


  d <-
    d %>%
    dplyr::filter(speed <= speed.limit,
                  between(lon, -179, 179),
                  between(lat, -89, 89))


  return(d)
}
