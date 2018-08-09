#' @export
vms_setup_directory <- function(example = TRUE) {

  if(!dir.exists("data-raw")) dir.create("data-raw")
  if(!dir.exists("data-tidy")) dir.create("data-tidy")
  if(!dir.exists("data-products")) dir.create("data-products")

  if(example) {
    tmp <- utils::read.csv("ftp://ftp.hafro.is/pub/reiknid/einar/vms/iceland_vms.csv")
    utils::write.csv(tmp, file = paste0("data-raw", "/iceland_vms.csv"), row.names = FALSE)
  }

}