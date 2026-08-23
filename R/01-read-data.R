
## Path to NFI data

list.dirs("data-source", recursive = F)

dir_nfi <- list.dirs("data-source", recursive = F) |> str_subset("taxonomies", negate = T) |> sort(decreasing = TRUE)
dir_nfi <- dir_nfi[1]

dir_nfi_files <- list.files(dir_nfi, pattern = "\\.csv")



##
## Read NFI files to environment ######
##

walk(dir_nfi_files, function(x){
  
  data <- read_csv(file.path(dir_nfi, x), show_col_types = FALSE)
  name <- x |> str_sub(start = 4, end = -5)
  
  assign(name, data, envir = .GlobalEnv)
  
})



##
## Read categories to a list ######
##

if (!"categories" %in% list.files(dir_nfi)) stop("missing categories in the data export from OF Arena")

categories <- list.files(file.path(dir_nfi, "categories"), pattern = "\\.csv") |>
  set_names(~ str_remove(.x, "\\.csv$")) |>
  map(~ read_csv(file.path(dir_nfi, "categories",.x), show_col_types = FALSE))

## Taxonomies are downloaded manually from the Arena survey and renamed to same name as in Arena survey

if (!"taxonomies" %in% list.dirs("data-source", full.names = FALSE)) stop("missing taxonomines, export from OF Arena manually into: 'data-source/'")

taxonomies         <- list()
taxonomies$tree    <- read_csv("data-source/taxonomies/tree_V3.csv", show_col_types = FALSE)
taxonomies$birds   <- read_csv("data-source/taxonomies/birds_species_list.csv", show_col_types = FALSE)
taxonomies$mammals <- read_csv("data-source/taxonomies/mammal_species.csv", show_col_types = FALSE)

## Phase 1 CEO data added to categories
categories$cluster_ph1 <- read_csv("data-source/CEO_phase1/ceo-Land-Cover-Assessment_Gambia_(All)_data.csv", show_col_types = FALSE)


##
## Read ancillaries to a list
##

anci <- list()

## Ancillaries downloaded from internet , see 00-setup.R
anci$chave_E <- terra::rast("data-anci/E.bil")
#plot(anci$chave_E)

anci$wood_density <- local({
  load("data-anci/wsg_estimates.rda")
  as_tibble(wsg_estimates)
})

anci$sf_GEZ <- st_read("data-anci/gez2010/GMB_gez_2010_wgs84.geojson", quiet = TRUE)

## Ancillaries from gvt
anci$sf_regions <- st_read("data-source/administrative_boundaries/regions.shp", quiet = TRUE)

anci$sf_country <- anci$sf_region |> summarise()

anci$sf_lga <- st_read("data-source/administrative_boundaries/LG_Areas.shp", quiet = TRUE)

# ggplot() +  
#   geom_sf(data = anci$sf_country, fill = NA, col = "RED", linewidth = 2) +
#   geom_sf(data = anci$sf_regions, fill = NA, aes(color = REGION), linewidth = 1) +
#   geom_sf(data = anci$sf_lga, aes(fill = LG_Area))
