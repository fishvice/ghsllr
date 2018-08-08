#' @export
vms_directory_setup <- function(example = TRUE, templates = TRUE) {

  if(!dir.exists("data-raw")) dir.create("data-raw")
  if(!dir.exists("data-tidy")) dir.create("data-tidy")
  if(!dir.exists("data-products")) dir.create("data-products")

  if(example) {
    tmp <- read.csv("ftp://ftp.hafro.is/pub/reiknid/einar/vms/iceland_vms.csv")
    write.csv(tmp, file = paste0("data-raw", "/iceland_vms.csv"), row.names = FALSE)
  }

  if(templates) {
    path <- paste0(system.file(package = "ghsllr"), "/rmarkdown/templates")
    file.copy(from = paste0(path, "/tidy/skeleton/skeleton.Rmd"),
              to = "01vms_cleanup.Rmd")
    file.copy(from = paste0(path, "/rasterize/skeleton/skeleton.Rmd"),
              to = "02vms_rasterize.Rmd")
    file.copy(from = paste0(path, "/tracks/skeleton/skeleton.Rmd"),
              to = "03vms_tracks.Rmd")
  }

}