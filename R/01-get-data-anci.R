##
## Get Phase 1 data corrected ######
##

## These files were prepared in the project Gambia-NFI-2025
## See: https://github.com/gaelso/Gambia-NFI-2025

ph1         <- list()
ph1$raw     <- read_csv("data-source/CEO_phase1/from_CEO/ceo-Land-Cover-Assessment_Gambia_(All)_data.csv", show_col_types = FALSE)
ph1$cluster <- read_csv("data-source/CEO_phase1/ceo-phase1-final-allocation.csv", show_col_types = FALSE)




##
## Read ancillaries to a list ######
##

## Ancillaries downloaded from internet , see 00-setup.R
anci$chave_E <- terra::rast("data-anci/E.bil")
#plot(anci$chave_E)

anci$wood_density <- local({
  load("data-anci/wsg_estimates.rda")
  as_tibble(wsg_estimates)
})

anci$sf_GEZ <- st_read("data-anci/gez2010/GMB_gez_2010_wgs84.geojson", quiet = TRUE)

## Ancillaries from gvt
anci$sf_regions <- st_read("data-source/administrative_boundaries/regions.shp", quiet = TRUE) |>
  st_transform(crs = 4326)

anci$sf_country <- anci$sf_regions |> summarise()

anci$sf_lga <- st_read("data-source/administrative_boundaries/LG_Areas.shp", quiet = TRUE) |>
  st_transform(crs = 4326)

# ggplot() +  
#   geom_sf(data = anci$sf_country, fill = NA, col = "RED", linewidth = 2) +
#   geom_sf(data = anci$sf_regions, fill = NA, aes(color = REGION), linewidth = 1) +
#   geom_sf(data = anci$sf_lga, aes(fill = LG_Area))
