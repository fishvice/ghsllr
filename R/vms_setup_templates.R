#' @title Seeds three rmarkdown documents
#'
#' @description The function installs three rmarkdown documents in the current
#' working directory: 01vms_cleanup.Rmd, 02vms_rasterize.Rmd and 03vms_tracks.Rmd.
#' The default specification in the rmarkdown documents are set in order to
#' run VMS example data that reside in the data-raw directory
#' (see \code{\link{vms_setup_directory}}).
#'
#' @return NULL
#' @export
#'
vms_setup_templates <- function() {

  path <- paste0(system.file(package = "ghsllr"), "/rmarkdown/templates")
  file.copy(from = paste0(path, "/tidy/skeleton/skeleton.Rmd"),
            to = "01vms_standardize.Rmd")
  file.copy(from = paste0(path, "/rasterize/skeleton/skeleton.Rmd"),
            to = "02vms_rasterize.Rmd")
  file.copy(from = paste0(path, "/tracks/skeleton/skeleton.Rmd"),
            to = "03vms_tracks.Rmd")

}