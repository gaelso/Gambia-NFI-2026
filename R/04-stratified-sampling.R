

plot_vars <- tree |>
  summarise(
    plot_tree_count = n(),
    plot_tree_density = sum(tree_weight),
    plot_basal_area = sum(tree_basal_area * tree_weight),
    plot_biomass_ag = sum(tree_biomass_ag * tree_weight),
    plot_biomass_total = sum(tree_biomass_total * tree_weight),
    .by = c(cluster_no, plot_no)
  )

plot_list <- categories$sampling_point_data |>
  filter(!is.na(level2_code)) |>
  select(cluster_no = level1_code, plot_no = level2_code, plot_center_x = location_x, plot_center_y = location_y, stratum = forest_type) 

plot_info <- plot |>
  select(cluster_no, plot_no, plot_access, plot_access_label)

plot_domain <- plot_lulc |>
  select(cluster_no, plot_no, lulc_name, lulc_name_label, lulc_forest, lulc_forest_label) |>
  mutate(
    lulc_forest = if_else(is.na(lulc_forest), 4, lulc_forest),
    lulc_forest_label = if_else(is.na(lulc_forest_label), "Non-forest", lulc_forest_label)
  )

plot_data <- plot_list |>
  left_join(plot_info, by = join_by(cluster_no, plot_no)) |>
  left_join(plot_domain, by = join_by(cluster_no, plot_no)) |>
  left_join(plot_vars, by = join_by(cluster_no, plot_no)) |>
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

plot_data

plot_data |>
  filter(plot_access %in% c(0,1)) |>
  ggplot() +
  geom_point(aes(x = plot_basal_area, y = plot_biomass_ag)) +
  facet_wrap(~lulc_forest_label)
