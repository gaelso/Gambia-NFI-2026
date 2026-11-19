

## This section combines OF Arena processing chain scripts
## On the long run, recommends to write proper scripts in the Arena processing chain


##
## Tree DBH Class 10 cm ######
##



tree <- tree |> mutate(
  tree_dbh_class_10cm = case_when(
    tree_dbh < 10  ~ "000 -  9.9",
    tree_dbh < 20  ~ "010 - 19.9",
    tree_dbh < 30  ~ "020 - 29.9",
    tree_dbh < 40  ~ "030 - 39.9",
    tree_dbh < 50  ~ "040 - 49.9",
    tree_dbh < 60  ~ "050 - 59.9",
    tree_dbh < 70  ~ "060 - 69.9",
    tree_dbh < 80  ~ "070 - 79.9",
    tree_dbh < 90  ~ "080 - 89.9",
    tree_dbh < 100 ~ "090 - 99.9",
    tree_dbh < 110 ~ "100 - 109.9",
    tree_dbh < 120 ~ "110 - 119.9",
    TRUE ~ "120+"
  )
)

##
## tree Height class #######
##

tree <- tree |>
  mutate(
    tree_h_class = case_when(
      tree_height <  5 ~ "00 - 4.9",
      tree_height < 10 ~ "05 - 9.9",
      tree_height < 15 ~ "10 - 14.9",
      tree_height < 20 ~ "15 - 19.9",
      tree_height < 25 ~ "20 - 24.9",
      tree_height < 30 ~ "25 - 29.9",
      tree_height < 35 ~ "30 - 34.9",
      tree_height < 40 ~ "35 - 39.9",
      tree_height < 45 ~ "40 - 44.9",
      tree_height < 50 ~ "45 - 49.9",
      TRUE ~ "50+"
    )
  )


##
## Tree Count ######
##

## Remove trees with missing DBH or DBH < 10, move to 'common.R' ?
summary(tree$tree_dbh)
#tree <- tree |> filter(!is.na(tree_dbh), tree_dbh >= 10)

tree$tree_count <- 1


##
## Tree Basal Area ######
##

tree$tree_basal_area <- pi * (tree$tree_dbh/200)^2



##
## Tree Stem Volume ######
##

tree$tree_stem_volume <- 0.67 * tree$tree_basal_area * tree$tree_height_corr



##
## Tree Aboveground Biomass ######
##

## Chave 2014 equation

tree$tree_biomass_ag <- with(tree, round(0.0673 * (tree_wood_density * tree_dbh^2 * tree_height_corr)^0.976 / 1000, 3))


##
## Tree Belowground Biomass ######
##

## Root-to-shoot ratios (R), IPCC defaults, by forest type of the tree LULC section:
## - Mangrove forest: 0.49, 95% CI 0.47-0.51 (IPCC 2013 Wetlands Supplement, Table 4.5), SE from the CI
## - Other forest types: tropical dry forest, Africa, natural, AGB <= 125 t/ha: 0.332, SD 0.247
##   (IPCC 2019 Refinement, Vol. 4, Table 4.4 Updated), applied to the whole country.
##   The country is split between the tropical moist (TAwa) and tropical dry (TAwb) FAO GEZ (cluster_gez,
##   02a-preparation-common.R), see the table below. The dry forest value is used everywhere for simplicity
##   (tropical moist, Africa, natural: 0.232, +/-90%). With the GEZ of each cluster, the BGB of closed and
##   open forests would be ~14% lower. Gambian forests are far below the 125 t/ha AGB threshold
##   (> 125 t/ha: 0.379, SD 0.040).
## The SE (SD for the dry forest) are used for the uncertainty of BGB (IPCC approach 1, see 05n-carbon.R).
defaults$RS <- tibble(
  rs_class = c("Mangrove", "Dry forest"),
  rs       = c(0.49, 0.332),
  rs_se    = c((0.51 - 0.47) / (2 * qnorm(0.975)), 0.247)
)

## Table for the report: phase 1 points and phase 2 clusters by stratum and FAO GEZ
## (phase 1 points outside the GEZ polygons, on the coast, are counted as "Not in GEZ layer")
if (!exists("tab")) tab <- list()

tmp$gez_levels <- c("TAwa" = "moist", "TAwb" = "dry", "none" = "no GEZ")

tab$rs_gez <- bind_rows(
  tmp$sf_ph1 |>
    st_join(anci$sf_GEZ) |>
    as_tibble() |>
    transmute(phase = "Phase 1 points", stratum = ph1_stratum, gez = replace_na(gez_abbrev, "none")),
  cluster |>
    filter(!is.na(cluster_info_stratum)) |>
    transmute(phase = "Phase 2 clusters", stratum = cluster_info_stratum, gez = cluster_gez)
  ) |>
  mutate(
    stratum = str_to_sentence(if_else(stratum == "nonforest", "non-forest", stratum)),
    stratum = factor(stratum, levels = c("Closed forest", "Open forest", "Mangrove forest", "Non-forest")),
    gez = factor(tmp$gez_levels[gez], levels = tmp$gez_levels)
  ) |>
  count(phase, gez, stratum) |>
  pivot_wider(names_from = c(phase, gez), names_sep = ", ", values_from = n, values_fill = 0, names_sort = TRUE) |>
  arrange(stratum) |>
  select(where(\(x) !is.numeric(x) || sum(x) > 0)) |>
  rename(Stratum = stratum)
tab$rs_gez

tmp$get_rs <- \(.ftype) if_else(.ftype == "Mangrove Forest", defaults$RS$rs[1], defaults$RS$rs[2])

tree <- tree |>
  mutate(tree_biomass_bg = round(tree_biomass_ag * tmp$get_rs(plot_lulc_forest_label), 3))



##
## Tree Total Biomass ######
## 

tree$tree_biomass_total <- tree$tree_biomass_bg + tree$tree_biomass_ag



##
## Tree Aboveground Carbon ######
##

tree$tree_carbon_ag <- tree$tree_biomass_ag * defaults$CF



##
## Tree Belowground Carbon ######
##

tree$tree_carbon_bg <- tree$tree_biomass_bg * defaults$CF



##
## Tree Total Carbon ######
##

tree$tree_carbon_total <- tree$tree_biomass_total * defaults$CF





##
## Baobab Aboveground Biomass ######
##

## Baobabs (Adansonia) are kept out of the live tree table (see 02a-preparation-common.R), and so out of the
## plot level totals (03-baseunit-calc.R). Their AGB is calculated with the same Chave 2014 equation and the
## species wood density, from the measured heights, and reported separately in 05i-biomass.R.
## The pantropical equation is not calibrated for the water-storing stem of baobabs: values to be reviewed.

## Check: all baobab heights measured (no height-diameter model for baobabs)
sum(is.na(baobab$tree_height))

baobab$tree_biomass_ag <- with(baobab, round(0.0673 * (tree_wood_density * tree_dbh^2 * tree_height)^0.976 / 1000, 3))
baobab$tree_biomass_bg <- round(baobab$tree_biomass_ag * tmp$get_rs(baobab$plot_lulc_forest_label), 3)
