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

## IUCN Red List, plants and animals. Accepted names and synonyms, with the family and category of the accepted
## species. Files without header, column positions from 'data-anci/iucn/meta.xml'.
## iucn_name: genus + species epithet, for matching with tree species names (kingdom PLANTAE) and wildlife
## names (kingdom ANIMALIA)
anci$iucn <- local({

  taxon <- read_tsv("data-anci/iucn/taxon.txt", col_names = FALSE, quote = "", col_types = cols(.default = "c"), progress = FALSE) |>
    select(iucn_id = X1, iucn_name_full = X2, kingdom = X3, family = X7, iucn_rank = X11, iucn_status = X13, iucn_accepted_id = X14) |>
    filter(kingdom %in% c("PLANTAE", "ANIMALIA"))

  dist <- read_tsv("data-anci/iucn/distribution.txt", col_names = FALSE, quote = "", col_types = cols(.default = "c"), progress = FALSE) |>
    select(iucn_accepted_id = X1, threat_status = X6) |>
    distinct(iucn_accepted_id, .keep_all = TRUE)

  taxon |>
    left_join(
      taxon |> filter(iucn_status == "accepted") |> select(iucn_accepted_id = iucn_id, iucn_family = family, iucn_accepted_name = iucn_name_full),
      by = join_by(iucn_accepted_id)
    ) |>
    inner_join(dist, by = join_by(iucn_accepted_id)) |>
    mutate(
      iucn_name = word(iucn_name_full, 1, 2),
      iucn_family = str_to_title(iucn_family),
      ## Lower case categories are the 1994 Lower Risk (LR) categories
      iucn_code = case_when(
        threat_status == "Extinct"                ~ "EX",
        threat_status == "Extinct in the Wild"    ~ "EW",
        threat_status == "Critically Endangered"  ~ "CR",
        threat_status == "Endangered"             ~ "EN",
        threat_status == "Vulnerable"             ~ "VU",
        threat_status == "Near Threatened"        ~ "NT",
        threat_status == "Least Concern"          ~ "LC",
        threat_status == "Data Deficient"         ~ "DD",
        threat_status == "conservation dependent" ~ "LR/cd",
        threat_status == "near threatened"        ~ "LR/nt",
        threat_status == "least concern"          ~ "LR/lc"
      ),
      iucn_category = case_when(
        iucn_code == "LR/cd" ~ "Lower Risk/conservation dependent",
        iucn_code == "LR/nt" ~ "Lower Risk/near threatened",
        iucn_code == "LR/lc" ~ "Lower Risk/least concern",
        TRUE ~ threat_status
      )
    ) |>
    mutate(iucn_accepted_name = word(iucn_accepted_name, 1, 2)) |>
    select(kingdom, iucn_name, iucn_name_full, iucn_rank, iucn_status, iucn_accepted_id, iucn_accepted_name, iucn_family, iucn_code, iucn_category)
})

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
