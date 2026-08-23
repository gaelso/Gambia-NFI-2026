

##
## Load packages ######
##

## Simple function
use_package <- function(.pkg_name) {
  pkg_name <- as.character(substitute(.pkg_name))
  if (!require(pkg_name, character.only = T,  quietly = TRUE)) install.packages(pkg_name, dep =TRUE)
  library(pkg_name, character.only = T, quietly = TRUE)
}

use_package(tidyverse)
use_package(sf)
use_package(terra)
use_package(tidyterra)


## Set ggplot theme
theme_set(theme_bw())


##
## Download ancillaries if needed ######
##

## + Chave 2014 raster E ####

if (!"data-anci" %in% list.files()) dir.create("data-anci")

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

