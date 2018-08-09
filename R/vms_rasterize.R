#' Title
#'
#' @param d XXX
#' @param res XXX
#'
#' @export
#'
vms_rasterize <- function(d, res)
{

  if(missing(res)) stop("You need to provide the resolution")
  if(length(res) == 1) {
    dx <- dy <- res
  } else {
    dx <- res[[1]]
    dy <- res[[2]]
  }

  d <-
    d %>%
    dplyr::filter(fishing) %>%
    dplyr::select(lon, lat) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(lon = grade(lon, dx),
                  lat = grade(lat, dy)) %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::count() %>%
    dplyr::ungroup()

  return(d)

}

