# Column names in vms files ----------------------------------------------------

# Ghana ------------------------------------------------------------------------
# Vessel Name, Date, Lat, Lond, Speed, EEZ
# Mobile, Date, Latitude, Longitude, Speed
# Mobile;Date;Latitude;Longitude;Speed;EEZ1;Port;Radio Call Sign;MMSI

# Sierra Leone -----------------------------------------------------------------
# ReceiveTime;gpsTime;Lat;Lon;Speed;gpsQuality;PublicDeviceID;Type

# Liberia ----------------------------------------------------------------------
# "vessel","datetime","lon","lat","dist_Nm","speed_kts"
# Date/Time	Latitude	Longitude	Event Type	Speed (MPH)	Heading
# Date/Time	Latitude	Longitude	Event Type	Speed (MPH)	Heading


#' Title
#'
#' @param cn A character vector
#'
#' @return A character vector
#' @export
#'
vms_standardize_names <- function(cn) {

  cn <- stringr::str_trim(cn)

  cn.time <- c("time", "date", "gpstime", "date/time", "posdate", "datetime")
  cn.lon <- c("lon", "lond", "longitude", "poslon")
  cn.lat <- c("lat", "latitude", "poslat")
  cn.speed <- c("speed", "speed_kts", "speed (mph)")
  cn.vid <- c("vid", "mobileid", "vessel name", "mobile", "publicdeviceid", "vessel", "vessel name")
  cn.eez <- c("eez", "eez1")

  i <- cn %in% cn.time
  cn[i] <- "time.txt"

  i <- cn %in% cn.lon
  cn[i] <- "lon"

  i <- cn %in% cn.lat
  cn[i] <- "lat"

  i <- cn %in% cn.speed
  cn[i] <- "speed"

  i <- cn %in% cn.vid
  cn[i] <- "vid"

  i <- cn %in% cn.eez
  cn[i] <- "eez"

  return(cn)

}