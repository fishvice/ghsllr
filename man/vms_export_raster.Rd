% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vms_export_raster.R
\name{vms_export_raster}
\alias{vms_export_raster}
\title{Export rasterized data as ascii}
\usage{
vms_export_raster(d, file.name = "data-products/Effort_raster.asc")
}
\arguments{
\item{d}{A standardized VMS tibble containing variables lon, lat and n}

\item{file.name}{The path and the filename of the ascii raster textfile to
be exported.}
}
\description{
The function converts a rasterized dataframe into a raster
object that is then exported as an ascii text file. That file can be
imported into QGIS.
}
