#' Standardize vms data
#'
#' XXX
#'
#' @param file.name XXX
#' @param country XXX
#'
#' @return dataframe
#' @export
#'
vms_import_data <- function(file.name, country)
{

  if(!country %in% c("Iceland", "Ghana", "Liberia", "Sierra Leone"))
    stop('The only vms country formats supported so far are: "Iceland", "Ghana", "Liberia", "Sierra Leone"')


  d <-
    rio::import(file.name, setclass="tibble") %>%
    dplyr::rename_all(., tolower)
  colnames(d) <- stringr::str_trim(colnames(d))
  colnames(d) <- vms_standardize_names(colnames(d))

  if(class(d$lon) == "character")   d$lon <-   as.numeric(stringr::str_replace(d$lon, ",", "."))
  if(class(d$lat) == "character")   d$lat <-   as.numeric(stringr::str_replace(d$lat, ",", "."))
  if(class(d$speed) == "character") d$speed <- as.numeric(stringr::str_replace(d$speed, ",", "."))

  if(!any(colnames(d) %in% "vid")) {
    d$vid <- file.name
  }
  if(class(d$vid) != "character")   d$vid <-   as.character(d$vid)


  if(country == "Iceland") {
    d <-
      d %>%
      dplyr::mutate(lon = lon * 45 / atan(1),
                    lat = lat * 45 / atan(1),
                    heading = heading * 45 / atan(1),
                    speed = speed * 1.852)
                    #time = lubridate::ymd_hms(time),
                    #recdate = lubridate::ymd_hms(recdate),
                    #vid = as.character(vid)) %>%
      #dplyr::arrange(vid, time)
  }

  if(country == "Ghana") {
    d <-
      d #%>%
      #dplyr::mutate(#time = lubridate::dmy_hm(time),
      #              lon = as.numeric(lon),
      #              lat = as.numeric(lat),
      #              speed = as.numeric(speed))
  }

  if(country == "Sierra Leone") {
    d <-
      d %>%
      dplyr::mutate(#lat = as.numeric(stringr::str_replace(lat,  ",",  ".")),
                    #lon = as.numeric(stringr::str_replace(lon,  ",",  ".")),
                    time = stringr::str_sub(time, 1, 19),
                    #time = lubridate::ymd_hms(time),
                    speed = speed * 3600/185200)
  }

  if(country == "Liberia") {
    d <-
      d #%>%
      #dplyr::mutate(#time = lubridate::ymd_hms(time),
      #              lat = as.numeric(lat),
      #              lon = as.numeric(lon))
  }


  d <-
    d %>%
    #dplyr::filter(dplyr::between(lon, -179.99999999999, 179.99999999999),
    #              dplyr::between(lat, -89.99999999999, 89.99999999999)) %>%
    dplyr::mutate(time2 = anytime::anytime(time)) #%>%
    #arrange(vid, time2)

  return(d)
}
