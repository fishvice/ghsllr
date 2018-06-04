#' Title
#'
#' @param x A numerical vector
#' @param dx The binwidth resolution
#'
#' @return A numerical vector
#' @export
#'
grade <- function(x, dx) {
  brks <- seq(floor(min(x)), ceiling(max(x)),dx)
  ints <- findInterval(x, brks, all.inside = TRUE)
  x <- (brks[ints] + brks[ints + 1]) / 2
  return(x)
}