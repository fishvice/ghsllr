vms_render_documents <- function(file.name) {

  if(missing(file.name)) file.name <- sort(dir(".", pattern = ".Rmd"))
  if(length(file.name) > 0) {
    for(i in file.name) {
      rmarkdown::render(i)
    }
  }
}