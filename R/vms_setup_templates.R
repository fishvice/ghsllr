#' @export
vms_setup_templates <- function() {

  path <- paste0(system.file(package = "ghsllr"), "/rmarkdown/templates")
  file.copy(from = paste0(path, "/tidy/skeleton/skeleton.Rmd"),
            to = "01vms_cleanup.Rmd")
  file.copy(from = paste0(path, "/rasterize/skeleton/skeleton.Rmd"),
            to = "02vms_rasterize.Rmd")
  file.copy(from = paste0(path, "/tracks/skeleton/skeleton.Rmd"),
            to = "03vms_tracks.Rmd")

}