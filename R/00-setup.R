

##
## Load packages ######
##

## Simple function
use_package <- function(.pkg_name) {
  pkg_name <- as.character(substitute(.pkg_name))
  if (!require(pkg_name, character.only = T,  quietly = TRUE)) install.packages(pkg_name, dep =TRUE)
  library(pkg_name, character.only = T, quietly = TRUE)
}

suppressPackageStartupMessages({
  use_package(tidyverse)
  use_package(sf)
  use_package(terra)
  use_package(tidyterra)
  use_package(tictoc)
})

## Set ggplot theme
theme_set(theme_bw())


##
## Download ancillaries if needed ######
##

if (!"data-anci" %in% list.files()) dir.create("data-anci")


## + Chave 2014 raster E ####

if (!"E.bil" %in% list.files("data-anci")) {
  download.file(
    url = "https://github.com/umr-amap/BIOMASS/raw/refs/heads/master/data-raw/climate_variable/E.zip",
    destfile = file.path("data-anci", "E.zip")
  )
  unzip(zipfile = "data-anci/E.zip", exdir = "data-anci")
  unlink("data-anci/E.zip")
  
}


## + Get WD at species level ####

## OUTDATED
# if (!"wdData.csv" %in% list.files("data-anci")) {
#   download.file(
#     url = "https://raw.githubusercontent.com/umr-amap/BIOMASS/refs/heads/master/data-raw/wdData.csv",
#     destfile = file.path("data-anci", "wdData.csv")
#   )
# }

if (!"wsg_estimates.rda" %in% list.files("data-anci")) {
  download.file(
    url = "https://github.com/umr-amap/BIOMASS/raw/refs/heads/master/data/wsg_estimates.rda",
    destfile = file.path("data-anci", "wsg_estimates.rda")
  )
}


## + FAO GEZ for IPCC default values ######

## Download manually into "data-anci" and unzip. Expected path: "data-anci/gez2010/gez_2010_wgs84.shp"
## https://storage.googleapis.com/fao-maps-catalog-data/uuid/2fb209d0-fd34-4e5e-a3d8-a13c241eb61b/resources/gez2010.zip

if(!"gez2010/gez_2010_wgs84.shp" %in% list.files("data-anci", recursive = TRUE)) stop("Download manually GEZ 2010, see 'R/00-setup.R' for more instructions")

## GEZ 2010 is too large and contains error, preparing a clean smaller file
if(!"gez2010/GMB_gez_2010_wgs84.geojson" %in% list.files("data-anci", recursive = TRUE)) {
  
  message("Preparing GEZ spatial layer may take time...")
  tic()
  sf_GEZ <- st_read("data-anci/gez2010/gez_2010_wgs84.shp", quiet = TRUE) |>
    st_make_valid() |>
    st_crop(anci$sf_GEZ, xmin = -17, xmax = -13.5, ymin = 13, ymax = 14)
  message("...Done - ", round(with(toc(quiet = TRUE), toc - tic), 3), " sec")
  
  st_write(sf_GEZ, "data-anci/gez2010/GMB_gez_2010_wgs84.geojson")
  rm(sf_GEZ)
  
}

