
plot_analysis |>
  filter(plot_access_lgl) |>
  ggplot() +
  geom_point(aes(x = plot_calc_basal_area, y = plot_calc_biomass_ag)) +
  facet_wrap(~lulc_forest_label)

tt <- plot_analysis |> filter(plot_access_lgl, is.na(lulc_forest))


cluster_info <- plot_analysis |>
  filter(plot_access_lgl, plot_no == 1) |>
  select(
    cluster_no, stratum, cluster_center_x = plot_center_x, cluster_center_y = plot_center_y, 
    cluster_lulc_name = lulc_name, cluster_lulc_name_label = lulc_name_label
    )
  
cluster_domain <- plot_analysis |>
  filter(plot_access_lgl, plot_no == 1) |>
  select(
    cluster_no, cluster_lulc_forest = lulc_forest, cluster_lulc_forest_label = lulc_forest_label
  )

cluster_vars <- plot_analysis |>
  filter(plot_access_lgl) |>
  left_join(cluster_domain, by = join_by(cluster_no)) |>
  filter(lulc_forest_label == cluster_lulc_forest_label) |>
  summarise(
    cluster_tree_count = mean(plot_calc_tree_count),
    cluster_tree_density = mean(plot_calc_tree_density),
    cluster_basal_area = mean(plot_calc_basal_area),
    cluster_biomass_ag = mean(plot_calc_biomass_ag),
    cluster_biomass_total = mean(plot_calc_biomass_total),
    .by = c(cluster_no, cluster_lulc_forest, cluster_lulc_forest_label)
  )

cluster_analysis <- cluster_info |>
  left_join(cluster_vars, by = join_by(cluster_no))

cluster_analysis |>
  ggplot() +
  geom_point(aes(
    x = cluster_basal_area, 
    y = cluster_biomass_ag, 
    color = cluster_lulc_forest_label, 
    shape = cluster_lulc_forest_label
    )) +
  theme(legend.position = "bottom") +
  facet_wrap(~stratum) +
  labs(color = "LULC", shape = "LULC")

## Forest type estimates
## ME = Margin of Error (half-width of confidence interval) = SD / sqrt(N) * t.inv
## MEP = Margin of Error Percentage = U% (ratio of half-width of CI over mean) = ME / mean
forest_type <- cluster_analysis |>
  summarise(
    cluster_count    = n(),
    tree_count       = mean(cluster_tree_count),
    tree_count_sd    = sd(cluster_tree_count),
    tree_density     = mean(cluster_tree_density),
    tree_density_sd  = sd(cluster_tree_density),
    basal_area       = mean(cluster_basal_area),
    basal_area_sd    = sd(cluster_basal_area),
    biomass_ag       = mean(cluster_biomass_ag),
    biomass_ag_sd    = sd(cluster_biomass_ag),
    biomass_total    = mean(cluster_biomass_total),
    biomass_total_sd = sd(cluster_biomass_total),
    .by = c(cluster_lulc_forest, cluster_lulc_forest_label)
  ) |>
  mutate(
    tree_density_me = tree_density_sd / sqrt(cluster_count) * qt(1-defaults$ci_alpha/2, df = cluster_count - 1),
    tree_density_mep = round(tree_density_me / tree_density * 100, 2),
    basal_area_me = basal_area_sd / sqrt(cluster_count) * qt(1-defaults$ci_alpha/2, df = cluster_count - 1),
    basal_area_mep = round(basal_area_me / tree_density * 100, 2),
    biomass_ag_me = biomass_ag_sd / sqrt(cluster_count) * qt(1-defaults$ci_alpha/2, df = cluster_count - 1),
    biomass_ag_mep = round(biomass_ag_me / tree_density * 100, 2),
    biomass_total_me = biomass_total_sd / sqrt(cluster_count) * qt(1-defaults$ci_alpha/2, df = cluster_count - 1),
    biomass_total_mep = round(biomass_total_me / tree_density * 100, 2),
  )

# forest_type_final <- ph1$strata_weights |>
#   mutate(cluster_lulc_)

forest_type |>
  ggplot(aes(x = cluster_lulc_forest_label)) +
  geom_col(aes(y = biomass_ag, fill = cluster_lulc_forest_label)) +
  geom_errorbar(aes(ymin = biomass_ag - biomass_ag_me, ymax = biomass_ag + biomass_ag_me), width = 0.6) +
  geom_label(aes(y = - 5, label = cluster_count)) +
  theme(legend.position = "none")

forest_type
write_csv(forest_type, "results/forest_type_draft.csv")


tt <- plot_analysis |> 
  filter(lulc_forest_label == "Close Forest")


