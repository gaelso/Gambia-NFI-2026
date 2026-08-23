

## This section combines OF Arena processing chain scripts
## On the long run, recommends to write proper scripts in the Arena processing chain

## In 00-common

tmp <- list()

##
## + Tree weight ######
##

table(cluster$cluster_info_stratum, useNA = "ifany")

tmp$cluster_stratum <- cluster |> select(cluster_no, cluster_info_stratum)
tmp$cluster_count <- nrow(cluster)

tree <- tree |>
  mutate(cluster_info_stratum = NA) |>
  left_join(tmp$cluster_stratum, by = join_by(cluster_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    tree_weight = if_else(tree_dbh < 20, 400/10000, 1000/10000),
    tree_cluster_weight = if_else(cluster_info_stratum == "mangrove_forest", tree_weight * 3, tree_weight * 5)
  )



##
## + Extract E value at plot center coordinates ######
##

## Plot starting point assumed at center
tmp$sf_plot <- plot |>
  filter(plot_access %in% c(0,1)) |>
  mutate(
    x = if_else(is.na(plot_starting_point_taken_x), plot_starting_point_given_x, plot_starting_point_taken_x),
    y = if_else(is.na(plot_starting_point_taken_y), plot_starting_point_given_y, plot_starting_point_taken_y)
      ) |>
  select(cluster_no, plot_no, x, y) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

# ggplot() +
#   geom_spatraster(data = anci$chave_E) +
#   geom_sf(data = sf_plot) +
#   coord_sf(xlim = c(-17, -13), ylim = c(13, 14))

tmp$plot_E <- terra::extract(anci$chave_E, terra::vect(sf_plot), bind = TRUE) |> as_tibble() |> rename(chaveE = E)

tree <- tree |>
  mutate(chaveE = NA) |>
  left_join(tmp$plot_E, by = join_by(cluster_no, plot_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))



##
## Add WD to tree ######
##

tmp$sp_list <- tree |>
  filter(!is.na(tree_species_scientific_name)) |>
  summarise(tree_count_ha_unstratified = sum(tree_cluster_weight, na.rm = TRUE)/tmp$cluster_count, .by = tree_species_scientific_name) |>
  arrange(desc(tree_count))

#tmp$sp_list <- tree$tree_species_scientific_name |> unique() |> sort()
  

anci$wood_density


