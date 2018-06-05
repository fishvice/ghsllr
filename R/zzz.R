.onLoad <- function(libname, pkgname){
  x <<- stats::rnorm(10)   ## dummy example
  if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)
}