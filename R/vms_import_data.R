#' @title Import and standardize vms data
#'
#' @description The function makes an attempt to import data and subsequently
#' standardize names and units of VMS data from the country specified. The
#' functions also filters out any data with non-plausible longitudes (those not
#' between -179.9 and 179.9 degrees) and latitudes (those not between -89.9 and
#' 89.9 degrees).
#'
#' @param file.name A character vector specifying the path and the filename of
#' the VMS data to import and standardize.
#' @param country A character vector specifying country of origin of the data.
#' Current choices are "Ghana", "Liberia", "Sierra Leone" and "Iceland".
#'
#' @return dataframe
#' @export
#'
vms_import_data <- function(file.name, country) {

  if(!country %in% c("Iceland", "Ghana", "Liberia", "Sierra Leone")) {
    stop('country specification needs to be one of "Ghana", "Liberia", "Sierra Leone" or "Iceland"')
  }

  if(!file.exists(file.name)) stop(paste0('path and file name: "', file.name, '"not found'))

  if(country == "Iceland")      d <- vms_import_data_is(file.name)
  if(country == "Ghana")        d <- vms_import_data_gh(file.name)
  if(country == "Sierra Leone") d <- vms_import_data_sl(file.name)
  if(country == "Liberia")      d <- vms_import_data_lr(file.name)

  d <-
    d %>%
    dplyr::mutate(country = country) %>%
    dplyr::filter(dplyr::between(lon, -179.99999999999, 179.99999999999),
                  dplyr::between(lat, -89.99999999999, 89.99999999999))

  return(d)
}
