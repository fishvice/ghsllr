#' Standardize vms data
#'
#' XXX
#'
#' @param file.name XXX
#' @param country XXX
#'
#' @return dataframe
#'
vms_import_data2 <- function(file.name, country)
{

  if(country == "Iceland") {
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
      dplyr::arrange(vid, date)
  }

  if(country == "Ghana") {

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
                    speed = as.numeric(speed)) %>%
      dplyr::arrange(vid, date)
  }

  if(country == "Sierra Leone") {
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
                    speed = as.numeric(speed) * 3600/185200)%>%
      dplyr::arrange(vid, date)
  }

  if(country == "Liberia") {
    d <-
      rio::import(file.name, setclass="tibble") %>%
      dplyr::rename(date = DateTime, # or gpsTime???
                    lon = Longitude,
                    lat = Latitude,
                    speed = "Speed (MPH)") %>%
      dplyr::mutate(date = lubridate::ymd_hms(date),
                    lat = as.numeric(lat),
                    lon = as.numeric(lon)) %>%
      dplyr::arrange(vid, date)
  }


  d <-
    d %>%
    dplyr::filter(dplyr::between(lon, -179.99999999999, 179.99999999999),
                  dplyr::between(lat, -89.99999999999, 89.99999999999)) %>%
    dplyr::mutate(country = country)


  return(d)
}
