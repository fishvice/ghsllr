#' Standardize vms data
#'
#' XXX
#'
#' @param file.name XXX
#'
#' @return dataframe
#' @export
#'
vms_import_data <- function(file.name)
{

  #if(!country %in% c("Iceland", "Ghana", "Liberia", "Sierra Leone"))
  #  stop('The only vms country formats supported so far are: "Iceland", "Ghana", "Liberia", "Sierra Leone"')


  d <-
    rio::import(file.name, setclass="tibble") %>%
    dplyr::rename_all(., tolower)

  cn <- colnames(d) %>% stringr::str_trim()

  # Guess country

  d$country <- vms_guess_country(cn)

  # Convert icelandic data -----------------------------------------------------
  if(any(cn %in% c("trailid", "mobileid", "poslat", "poslon",
                   "posdate", "recdate"))) {
    d <-
      d %>%
      dplyr::mutate(poslon = poslon * 45 / atan(1),
                    poslat = poslat * 45 / atan(1),
                    heading = heading * 45 / atan(1),
                    speed = speed * 1.852)
  }

  # Convert sierra leone speed
  if(all(cn %in%  c("receivetime", "gpstime", "lat", "lon", "speed",
                    "gpsquality", "publicdeviceid", "type"))) {
    d <-
      d %>%
      dplyr::mutate(speed = as.numeric(speed) * 3600/185200)
  }

  # convert liberia speed
  if(any(cn %in% c("speed (mph)"))) {
    d <-
      d %>%
      dplyr::mutate(`speed (mph)` = 0.868976 * `speed (mph)`)
  }

  colnames(d) <- stringr::str_trim(colnames(d))
  colnames(d) <- vms_standardize_names(colnames(d))

  if(class(d$lon) == "character")   d$lon <-   as.numeric(stringr::str_replace(d$lon, ",", "."))
  if(class(d$lat) == "character")   d$lat <-   as.numeric(stringr::str_replace(d$lat, ",", "."))
  if(class(d$speed) == "character") d$speed <- as.numeric(stringr::str_replace(d$speed, ",", "."))

  if(!any(colnames(d) %in% "vid")) {
    d$vid <- file.name
  }
  if(class(d$vid) != "character")   d$vid <-   as.character(d$vid)
  if(!any(colnames(d) %in% "heading")) {
    d$heading <- NA_character_
  }
  if(class(d$heading) != "character") d$heading <- as.character(d$heading)

  d <-
    d %>%
    dplyr::filter(dplyr::between(lon, -179.9999, 179.9999),
                  dplyr::between(lat, -89.9999, 89.9999)) %>%
    dplyr::mutate(time = vms_convert_to_time(time.txt)) %>%
    dplyr::arrange(vid, time)

  return(d)
}
