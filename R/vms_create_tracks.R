# Function to create vessel tracks for each vessel
#' Title
#'
#' @param d XXX
#'
#' @export
#'
vms_create_tracks <- function(d)
{
  tracks <- list()
  ids <- unique(d$vid)

  for(i in 1:length(ids)){

    xy <- d %>%
      dplyr::filter(vid == ids[i],
                    dspeed > 0,
                    is.finite(dspeed)) %>%
      dplyr::select(lon, lat)

    if(nrow(xy)>0){

      tracks[[i]] <- sp::Lines(list(sp::Line(xy)), ID = ids[i])

    }
  }

  k <- unlist(lapply(tracks, class)) == "Lines"
  tracks <- tracks[k]
  tracks <-
    sp::SpatialLines(tracks, proj4string = sp::CRS("+proj=longlat +datum=WGS84")) %>%
    sp::SpatialLinesDataFrame(data = data.frame(vid = ids), match.ID = FALSE)

  return(tracks)
}