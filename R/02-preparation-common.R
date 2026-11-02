

## This section combines OF Arena processing chain scripts
## On the long run, recommends to write proper scripts in the Arena processing chain



##
## !!! Corrections !!! ######
##

table(anci$sampling_points$forest_type)
table(plot_lulc$lulc_forest_label)
table(cluster$cluster_info_stratum)

anci$sampling_points <- anci$sampling_points |>
  mutate(
    forest_type = case_when(
      forest_type == "close forest" ~ "closed forest",
      TRUE ~ forest_type
    )
  )

plot_lulc <- plot_lulc |>
  mutate(
    lulc_forest_label = case_when(
      lulc_forest_label == "Close Forest" ~ "Closed Forest",
      TRUE ~ lulc_forest_label
    )
  )

cluster <- cluster |>
  mutate(
    cluster_info_stratum = case_when(
      cluster_info_stratum == "close forest" ~ "closed forest",
      TRUE ~ cluster_info_stratum
    )
  )

## 
## Defaults #####
## 

defaults <- list()

## Wood density
defaults$WD <- 0.57

## Carbon faction
defaults$CF <- 0.47

## Confidence level
defaults$conf_level <- 0.9
defaults$ci_alpha <- 1 - defaults$conf_level

## test: qt(1 - defaults$ci_alpha/2, df = Inf)

## Country area 
defaults$country_area_nowater <- anci$sf_lga |> st_area() |> units::set_units("ha") |> as.integer() |> sum()
defaults$country_area <- 1130000 ### from https://www.un.int/gambia/gambia/country-facts
defaults$main_river_area <- defaults$country_area - defaults$country_area_nowater

## Check
# anci$sf_regions |> st_area() |> units::set_units("ha") |> as.integer() |> sum()
# anci$sf_lga |> st_area() |> units::set_units("ha") |> as.integer() |> sum()

## Plot size 
defaults$plot_size_dbh10 <- 0.04 ## 20x20 m 
defaults$plot_size_dbh20 <- 0.1  ## 20x50 m



##
## Add cluster stratum and plot LULC to tree ######
##

tmp <- list()

table(cluster$cluster_info_stratum, useNA = "ifany")

tmp$cluster_stratum <- cluster |> 
  select(cluster_no, cluster_info_stratum)
tmp$cluster_count <- nrow(cluster)

## Forest type of each tree from its LULC section, non-forest land uses grouped
tmp$plot_lulc <- plot_lulc |>
  select(
    cluster_no,
    plot_no,
    tree_lulc_no = lulc_id,
    plot_lulc_forest = lulc_forest,
    plot_lulc_forest_label = lulc_forest_label
  )

tree <- tree |>
  mutate(
    cluster_info_stratum = NA,
    plot_lulc_forest = NA,
    plot_lulc_forest_label = NA
  ) |>
  left_join(
    tmp$cluster_stratum,
    by = join_by(cluster_no),
    suffix = c("_rm", "")
  ) |>
  left_join(
    tmp$plot_lulc,
    by = join_by(cluster_no, plot_no, tree_lulc_no),
    suffix = c("_rm", "")
  ) |>
  select(-ends_with("_rm")) |>
  mutate(
    plot_lulc_forest_label = replace_na(plot_lulc_forest_label, "Non-forest")
  )

table(tree$cluster_info_stratum, tree$plot_lulc_forest_label, useNA = "ifany")

## Number of trees per forest type: cluster stratum vs plot LULC section, diagonal = agreement
tmp$fig_tree_lulc <- tree |>
  count(
    stratum = str_to_title(cluster_info_stratum),
    lulc = plot_lulc_forest_label
  ) |>
  mutate(
    stratum = factor(stratum, levels = c("Closed Forest", "Open Forest", "Mangrove Forest")),
    lulc = factor(lulc, levels = c("Closed Forest", "Open Forest", "Mangrove Forest", "Non-forest"))
  ) |>
  complete(stratum, lulc, fill = list(n = 0))

## Square root color scale so that the small mismatches remain visible next to the diagonal
tmp$fig_tree_lulc |>
  ggplot(aes(x = lulc, y = stratum, fill = n)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = n, color = n > max(n) / 4), size = 3.5) +
  scale_fill_gradientn(colors = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#104281"), trans = "sqrt") +
  scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "#1a1a19"), guide = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) #+
  #scale_y_discrete(limits = rev) +
  #labs(
   # x = "Plot LULC section (plot_lulc_forest_label)",
    #y = "Cluster stratum (cluster_info_stratum)",
    #fill = "Tree records"
  #) +
  #theme(panel.grid = element_blank())



##
## Tree weight ######
##

tree <- tree |>
  mutate(
    tree_weight = if_else(tree_dbh < 20, 1 / defaults$plot_size_dbh10, 1 / defaults$plot_size_dbh20),
    tree_cluster_weight = if_else(cluster_info_stratum == "mangrove forest", tree_weight / 3, tree_weight / 5)
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
tmp$sf_plot <- anci$sampling_point |>
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

tmp$plot_E <- terra::extract(anci$chave_E, terra::vect(tmp$sf_plot), bind = TRUE) |> 
  as_tibble() |> 
  rename(chaveE = E) |>
  mutate(across(where(is.character), as.numeric))


tree <- tree |>
  mutate(chaveE = NA) |>
  left_join(tmp$plot_E, by = join_by(cluster_no, plot_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))



##
## Get GEZ to cluster level ######
##

tmp$sf_cluster <- anci$sampling_point |>
  filter(is.na(level2_code)) |>
  select(cluster_no = level1_code, x = location_x, y = location_y) |>
  mutate(cluster_no = as.numeric(cluster_no)) |>
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

st_write(ph1$sf_cluster_harmo, "results/sf_cluster_harmo.kml", delete_dsn = TRUE, quiet = TRUE)

ph1$cluster_harmo <- as_tibble(ph1$sf_cluster_harmo) |> select(-geometry)



##
## Strata Weights ######
##

ph1$strata_weights <- ph1$cluster_harmo |>
  count(ph1_stratum, name = "ph1_count") |>
  mutate(
    ph1_weights = round(ph1_count / nrow(ph1$cluster_harmo), 3),
    ph1_area    = as.integer(ph1_weights * defaults$country_area_nowater) 
  )


##
## Phase 1 stratification table ######
##

ph1_data <- ph1$cluster_harmo |> 
  select(ceo_cluster_no, ceo_id, type, ph1_stratum, cluster_no) |>
  mutate(
    PSU = cluster_no,
    subpop = 1,
    stratum = ph1_stratum
  )

table(tree$tree_status_label)


## 
## Split tables ######
## 

tree_bkp <- tree

dw_standing <- tree |> filter(tree_status_label == "Dead standing tree")
stump <- tree |> filter(tree_status_label == "Stump")
tree <- tree |> filter(tree_status_label == "Live standing tree")

## Growth forms that need specific allometries, separated from live trees by genus
## Lists include West African genera not (yet) recorded in the data
tmp$genus_bamboo   <- c("Oxytenanthera", "Bambusa")
tmp$genus_liana    <- c("Saba", "Landolphia", "Paullinia", "Cissus", "Strophanthus", "Baissea", "Hippocratea", "Loeseneriella")
tmp$genus_baobab   <- c("Adansonia")
tmp$genus_mangrove <- c("Rhizophora", "Avicennia", "Laguncularia", "Conocarpus")
tmp$genus_palm     <- c("Borassus", "Elaeis", "Phoenix", "Hyphaene", "Raphia", "Cocos")

bamboo   <- tree |> filter(word(tree_species_scientific_name) %in% tmp$genus_bamboo)
liana    <- tree |> filter(word(tree_species_scientific_name) %in% tmp$genus_liana)
baobab   <- tree |> filter(word(tree_species_scientific_name) %in% tmp$genus_baobab)
mangrove <- tree |> filter(word(tree_species_scientific_name) %in% tmp$genus_mangrove)
palm     <- tree |> filter(word(tree_species_scientific_name) %in% tmp$genus_palm)

tree <- tree |>
  filter(!word(tree_species_scientific_name) %in% c(
    tmp$genus_bamboo, tmp$genus_liana, tmp$genus_baobab, tmp$genus_mangrove, tmp$genus_palm
  ))

