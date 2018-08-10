## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("fishvice/ghsllr")

## ---- eval = FALSE-------------------------------------------------------
#  install.packages("mapdata")

## ------------------------------------------------------------------------
library(mapdata)
library(tidyverse)
library(ghsllr)

## ------------------------------------------------------------------------
vms <- 
  vms_import_data(file.name = "ftp://ftp.hafro.is/pub/reiknid/einar/vms/iceland_vms.csv",
                  country = "Iceland")
glimpse(vms)

## ---- fig.width = 7------------------------------------------------------
vms_plot_speed(vms)

## ------------------------------------------------------------------------
vms <- vms_categorise_fishing(vms, fishing.lim = c(2.5, 4.5))
glimpse(vms)

## ---- fig.width = 7------------------------------------------------------
vms_plot_speed(vms)

## ---- fig.width = 7------------------------------------------------------
vms.raster <-
  vms %>% 
  vms_rasterize(grid.lim = c(1, 0.5))

## ------------------------------------------------------------------------
vms.raster %>% glimpse()

## ---- fig.width = 7------------------------------------------------------
vms.raster %>% 
  vms_plot_raster(lon.lim = c(-26.5, -11.5),
                  lat.lim = c(63, 67))

## ------------------------------------------------------------------------
vms.fishing <- 
  vms %>% 
  vms_filter_data(speed.lim = c(2.5, 4.5))
glimpse(vms.fishing)

## ------------------------------------------------------------------------
vms.raster <-
  vms.fishing %>% 
  vms_rasterize(grid.lim = c(1, 0.5))

## ---- fig.width = 7------------------------------------------------------
vms.raster %>% 
  vms_plot_raster()

## ---- fig.width = 7------------------------------------------------------
vms %>% 
  vms_filter_data(speed.lim = c(2.5, 4.5)) %>% 
  vms_rasterize(grid.lim = c(0.1, 0.05)) %>% 
  vms_plot_raster()

## ---- fig.width = 7------------------------------------------------------
vms %>% 
  vms_filter_data(speed.lim = c(2.5, 4.5)) %>% 
  vms_rasterize(grid.lim = c(0.01, 0.005)) %>% 
  vms_plot_raster()

## ------------------------------------------------------------------------
vms %>% 
  vms_filter_data(speed.lim = c(2.5, 4.5)) %>% 
  vms_rasterize(grid.lim = c(0.01, 0.01)) %>%
  vms_export_raster(file.name = "data-products/vms_raster")

