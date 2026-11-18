

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

tree$tree_biomass_bg <- NA

table(cluster$cluster_info_stratum)
table(cluster$cluster_gez, cluster$cluster_gez_label, useNA = "ifany")

cluster_gez <- cluster |> select(cluster_no, cluster_gez, cluster_gez_label, cluster_info_stratum) 

##------##
## Demonstration dry forest has AGB <= 125 tdm/ha
# cluster_agb <- tree |> summarise(cluster_agb = sum(tree_biomass_ag * tree_cluster_weight), .by = c(cluster_no)) |>
#   left_join(cluster_gez, by = join_by(cluster_no))
# ggplot(cluster_agb) +
#   geom_boxplot(aes(x = cluster_info_stratum, y = cluster_agb, fill = cluster_gez))
# rm(cluster_agb)
##------##

## Add GEZ/Stratum info, get RS and calc BGB = AGB * RS
tree <- tree |>
  mutate(
    cluster_gez = NA, 
    cluster_gez_label = NA, 
    cluster_info_stratum = NA
  ) |>
  left_join(cluster_gez, by = join_by(cluster_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    tree_RS = case_when(
      cluster_info_stratum == "mangrove forest" ~ 0.49, ## Table 4.5, IPCC 2013 Wetlands Supplement 
      cluster_gez == "TAwa" ~ 0.232, ## Table 4.4 IPCC 2019 V4CH4
      cluster_gez == "TAwb" ~ 0.332, ## Table 4.4 IPCC 2019 V4CH4, considered all dry forest with AGB <= 125 tdm/ha
    ),
    tree_biomass_bg = round(tree_biomass_ag * tree_RS, 3)
  )

## remove temporary columns and objects
tree <- tree |>
  select(-cluster_gez , -cluster_gez_label, -tree_RS)



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
