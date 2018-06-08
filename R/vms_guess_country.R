#' Title
#'
#' @param cn Character vector
#'
#' @return Character
#' @export
#'
vms_guess_country <- function(cn) {


  if(all(cn %in% c("trailid", "mobileid", "poslat", "poslon", "speed",
                   "heading", "posdate", "recdate", "in_out_of_harbor",
                   "harborid"))) {
    return("Iceland")
  }

  if(all(cn %in% c("date/time", "latitude", "longitude", "event type",
    "speed (mph)", "heading"))) {
    return("Liberia")
  }

  if(all(cn %in% c("vessel", "datetime", "lon", "lat", "dist_nm", "speed_kts"))) {
    return("Liberia")
  }

  if(all(cn %in%  c("receivetime", "gpstime", "lat", "lon", "speed",
                    "gpsquality", "publicdeviceid", "type"))) {
    return("Sierra Leone")
  }

  if(all(cn %in% c("vessel name", "date", "lat", "lond", "speed", "eez"))) {
    return("Ghana")
  }

  if(all(cn %in% c("mobile", "date", "latitude",  "longitude", "speed",
                   "eez", "port"))) {
    return("Ghana")
  }

  if(all(cn %in% c("mobile", "date", "latitude", "longitude", "speed",
                   "eez1", "port", "radio call sign", "mmsi", "v10"))) {
    return("Ghana")
  }

  if(all(cn %in% c("vessel","date", "latitude",  "longitude", "speed"))) {
    return("Ghana")
  }

  return("Unkown")

}