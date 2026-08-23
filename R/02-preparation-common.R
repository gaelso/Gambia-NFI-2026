

## This section combines OF Arena processing chain scripts
## On the long run, recommends to write proper scripts in the Arena processing chain

## In 00-common

tmp <- list()

## 
## Defaults #####
##

defaults <- list()

## Wood density
defaults$WD <- 0.57

## Carbon faction
defaults$CF <- 0.47



##
## Tree weight ######
##

table(cluster$cluster_info_stratum, useNA = "ifany")

tmp$cluster_stratum <- cluster |> select(cluster_no, cluster_info_stratum)
tmp$cluster_count <- nrow(cluster)

tree <- tree |>
  mutate(cluster_info_stratum = NA) |>
  left_join(tmp$cluster_stratum, by = join_by(cluster_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    tree_weight = if_else(tree_dbh < 20, 10000/400, 10000/1000),
    tree_cluster_weight = if_else(cluster_info_stratum == "mangrove_forest", tree_weight / 3, tree_weight / 5)
  )



##
## Add WD to tree ######
##

tmp$species_list <- tree$tree_species_scientific_name |> unique() |> sort()
tmp$genus_list   <- tmp$species_list |> word() |> unique()

tmp$wd_species <- anci$wood_density |> 
  filter(species %in% tmp$species_list) |>
  arrange(species) |>
  select(tree_species_scientific_name = species, tree_species_wood_density = wsg)

tmp$wd_genus <- anci$wood_density |> 
  filter(genus %in% tmp$genus_list, is.na(species)) |>
  select(tree_genus = genus, tree_genus_wood_density = wsg)

tree <- tree |>
  mutate(
    tree_genus = word(tree_species_scientific_name),
    tree_species_wood_density = NA,
    tree_genus_wood_density = NA,
    tree_default_wood_density = defaults$WD
  ) |>
  left_join(tmp$wd_species, by = join_by(tree_species_scientific_name), suffix = c("_rm", "")) |>
  left_join(tmp$wd_genus, by = join_by(tree_genus), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    tree_wood_density = case_when(
      !is.na(tree_species_wood_density) ~ tree_species_wood_density,
      !is.na(tree_genus_wood_density) ~ tree_genus_wood_density,
      TRUE ~ tree_default_wood_density
    ),
    tree_wood_density_level = case_when(
      !is.na(tree_species_wood_density) ~ "species",
      !is.na(tree_genus_wood_density) ~ "genus",
      TRUE ~ "default"
    )
  )

## Remove intermediate columns
tree <- tree |>
  select(-tree_genus, -tree_species_wood_density, -tree_genus_wood_density, -tree_default_wood_density)



##
## Extract E value at plot center coordinates ######
##

## Plot starting point assumed at center
tmp$sf_plot <- categories$sampling_point_data |>
  filter(!is.na(level2_code)) |>
  select(cluster_no = level1_code, plot_no = level2_code, x = location_x, y = location_y) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

## Incomplete cause of inaccessible plots  
# tmp$sf_plot <- plot |>
#   filter(plot_access %in% c(0,1)) |>
#   mutate(
#     x = if_else(is.na(plot_starting_point_taken_x), plot_starting_point_given_x, plot_starting_point_taken_x),
#     y = if_else(is.na(plot_starting_point_taken_y), plot_starting_point_given_y, plot_starting_point_taken_y)
#       ) |>
#   select(cluster_no, plot_no, x, y) |>
#   st_as_sf(coords = c("x", "y"), crs = 4326)

# ggplot() +
#   geom_spatraster(data = anci$chave_E) +
#   geom_sf(data = sf_plot) +
#   coord_sf(xlim = c(-17, -13), ylim = c(13, 14))

tmp$plot_E <- terra::extract(anci$chave_E, terra::vect(tmp$sf_plot), bind = TRUE) |> as_tibble() |> rename(chaveE = E)

tree <- tree |>
  mutate(chaveE = NA) |>
  left_join(tmp$plot_E, by = join_by(cluster_no, plot_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))



##
## Get GEZ to cluster level ######
##

tmp$sf_cluster <- categories$sampling_point_data |>
  filter(is.na(level2_code)) |>
  select(cluster_no = level1_code, x = location_x, y = location_y) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

## Incomplete cause of uinaccessible plots
# tmp$sf_cluster <- cluster |>
#   select(cluster_no, x = cluster_info_centre_given_x, y = cluster_info_centre_given_y) |>
#   filter(!is.na(x), !is.na(y)) |>
#   st_as_sf(coords = c("x", "y"), crs = 4326)

# tmp$sf_cluster |>
#   filter(cluster_no == 39) |>
#   ggplot() +
#   geom_sf(size = 4, col = "red") +
#   geom_sf(data = anci$sf_GEZ, aes(fill = gez_abbrev), alpha = 0.5) +
#   geom_sf(data = tmp$sf_cluster) +
#   coord_sf(xlim = c(-17, -13.5), ylim = c(13, 14))

tmp$cluster_gez <-  tmp$sf_cluster |> 
  st_join(anci$sf_GEZ) |>
  as_tibble() |>
  select(cluster_no, cluster_gez = gez_abbrev, cluster_gez_label = gez_name) |>
  mutate(
    cluster_gez = if_else(cluster_no == 39, "TAwa", cluster_gez),
    cluster_gez_label = if_else(cluster_no == 39, "Tropical moist forest", cluster_gez_label)
  )

## Check for completeness
tmp$cluster_gez |> filter(is.na(cluster_gez))

cluster <- cluster |>
  mutate(
    cluster_gez = NA,
    cluster_gez_label = NA
    ) |>
  left_join(tmp$cluster_gez, by = join_by(cluster_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))


##
## Reconcile Phase 1 and Phase 2 cluster IDs #####
##

## Make spatial join
tmp$sf_cluster

tmp$sf_ph1 <- ph1$cluster |>
  select(ceo_cluster_no = ceo_tract_no, ceo_id, type, center_lon, center_lat, ph1_stratum = lu_class_final, ph2_selected) |>
  mutate(x = center_lon, y = center_lat) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

ph1$sf_cluster_harmo <- st_join(tmp$sf_ph1, tmp$sf_cluster)

table(ph1$sf_cluster_harmo$ph2_selected, useNA = "ifany")

## Remove cluster outside country boundaries 
ph1$sf_cluster_harmo <- st_filter(ph1$sf_cluster_harmo, anci$sf_country)

st_write(ph1$sf_cluster_harmo, "results/sf_cluster_harmo.kml", delete_dsn = TRUE)

