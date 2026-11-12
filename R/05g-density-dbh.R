
## g. Tree density per diameter class ####

## Trees/ha by 10 cm DBH class, two-phase estimates (nfi_aggregate2 sub-domains).
## Classes add up to the total density of the domain.

res_density_dbh <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = tree_count, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(tree, tree_dbh_class_10cm, tree_weight, defaults$plot_size_dbh20, .name = "tree_count"),
  .class_sub = tree_dbh_class_10cm
  ) |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  mutate(density = Yd / Xd)

## Check: classes add up to the total density
res_density_dbh |> summarise(density = sum(density), .by = c(level, forest_type, region)) |> filter(level != "forest_type_region")

## + Tables ####
tab$g_density_dbh <- res_density_dbh |>
  transmute(level, forest_type, region, n_pp, dbh_class = tree_dbh_class_10cm, density_ha = round(density, 2), density_mep = Rd_mep) |>
  arrange(level, forest_type, region, dbh_class) |>
  split_by_level()
tab$g_density_dbh$forest_type

save_tables(tab$g_density_dbh, "05g_density_dbh")

## + Figures ####
fig$g_density_dbh <- tab$g_density_dbh$forest_type |>
  mutate(density_me = density_ha * density_mep / 100) |>
  ggplot(aes(x = dbh_class, y = density_ha, fill = forest_type)) +
  geom_col(width = 0.8) +
  geom_errorbar(aes(ymin = pmax(density_ha - density_me, 0), ymax = density_ha + density_me), width = 0.2) +
  scale_fill_manual(values = defaults$pal_ftype, guide = "none") +
  scale_x_discrete(labels = \(x) str_remove(x, "^0") |> str_replace(" - ", "-")) +
  facet_wrap(~forest_type, scales = "free_y") +
  labs(x = "DBH class (cm)", y = "Tree density (trees/ha)", caption = "Error bars: 90% confidence interval") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig$g_density_dbh

fig$g_density_dbh_region <- tab$g_density_dbh$forest_type_region |>
  ggplot(aes(x = dbh_class, y = density_ha, fill = forest_type)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = defaults$pal_ftype, guide = "none") +
  scale_x_discrete(labels = \(x) str_remove(x, "^0") |> str_replace(" - ", "-")) +
  facet_grid(forest_type ~ region, scales = "free_y") +
  labs(x = "DBH class (cm)", y = "Tree density (trees/ha)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), strip.text.y = element_text(size = 7))
fig$g_density_dbh_region

ggsave("results/figures/05g_density_dbh.png", fig$g_density_dbh, width = 20, height = 14, units = "cm", dpi = 300)
ggsave("results/figures/05g_density_dbh_region.png", fig$g_density_dbh_region, width = 26, height = 18, units = "cm", dpi = 300)
