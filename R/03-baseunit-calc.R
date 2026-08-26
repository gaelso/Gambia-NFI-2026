

## Aggregate measures
plot_vars <- tree |>
  summarise(
    plot_calc_tree_count = n(),
    plot_calc_tree_density = sum(tree_weight),
    plot_calc_basal_area = sum(tree_basal_area * tree_weight),
    plot_calc_biomass_ag = sum(tree_biomass_ag * tree_weight),
    plot_calc_biomass_total = sum(tree_biomass_total * tree_weight),
    .by = c(cluster_no, plot_no)
  )

## Get full list of expected field plots
plot_list <- categories$sampling_point_data |>
  filter(!is.na(level2_code)) |>
  select(cluster_no = level1_code, plot_no = level2_code, plot_center_x = location_x, plot_center_y = location_y, stratum = forest_type)

## Retrieve accessibility
cluster_access <- cluster |> select(cluster_no, cluster_access, cluster_access_label)
plot_access    <- plot |> select(cluster_no, plot_no, plot_access, plot_access_label)

## Assign dominant land use to plot 
plot_domain <- plot_lulc |>
  select(cluster_no, plot_no, lulc_name, lulc_name_label, lulc_forest, lulc_forest_label, lulc_percentage) |>
  mutate(
    lulc_forest = if_else(is.na(lulc_forest), 99, lulc_forest),
    lulc_forest_label = if_else(is.na(lulc_forest_label), "Non-forest", lulc_forest_label)
    # lulc_forest = if_else(is.na(lulc_forest), lulc_name, lulc_forest),
    # lulc_forest_label = if_else(is.na(lulc_forest_label), lulc_name_label, lulc_forest_label)
  ) |>
  arrange(cluster_no, plot_no, desc(lulc_percentage), lulc_forest) |>
  distinct(cluster_no, plot_no, .keep_all = TRUE)

## Check unique cluster/plot with majo LU
plot_domain |> distinct(cluster_no, plot_no) |> nrow()
nrow(plot_domain)

## Combine all
plot_analysis <- plot_list |>
  left_join(cluster_access, by = join_by(cluster_no)) |>
  left_join(plot_access, by = join_by(cluster_no, plot_no)) |>
  left_join(plot_domain, by = join_by(cluster_no, plot_no)) |>
  left_join(plot_vars, by = join_by(cluster_no, plot_no)) |>
  mutate(
    plot_access = if_else(is.na(plot_access), cluster_access, plot_access),
    plot_access_label = if_else(is.na(plot_access_label), cluster_access_label, plot_access_label),
    plot_access_lgl = if_else(plot_access %in% c(0,1), TRUE, FALSE),
    lulc_forest = if_else(is.na(lulc_forest), 99, lulc_forest),
    lulc_forest_label = if_else(is.na(lulc_forest_label), "Non-forest", lulc_forest_label)
    ) |>
  mutate(across(starts_with("plot_calc_"), \(x) replace_na(x, 0))) |>
  arrange(cluster_no, plot_no)
