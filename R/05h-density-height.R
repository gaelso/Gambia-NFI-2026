
## h. Tree density per height class ####

## Trees/ha by 5 m height class, two-phase estimates (nfi_aggregate2 sub-domains).
## Height = tree_height_corr: measured height, or local height model prediction when missing.

tmp$height_breaks <- c(0, 5, 10, 15, 20, Inf)
tmp$height_labels <- c("< 5", "5-10", "10-15", "15-20", ">= 20")

tmp$tree_height <- tree |>
  mutate(tree_height_class = cut(tree_height_corr, breaks = tmp$height_breaks, labels = tmp$height_labels, right = FALSE))

## Check: all trees with a height class
table(tmp$tree_height$tree_height_class, useNA = "ifany")

res_density_height <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = tree_count, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(tmp$tree_height, tree_height_class, tree_weight, defaults$plot_size_dbh20, .name = "tree_count"),
  .class_sub = tree_height_class
  ) |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  mutate(
    density = Yd / Xd,
    tree_height_class = factor(tree_height_class, levels = tmp$height_labels)
  )

## + Tables ####
tab$h_density_height <- res_density_height |>
  transmute(level, forest_type, region, n_pp, height_class = tree_height_class, density_ha = round(density, 2), density_mep = Rd_mep) |>
  arrange(level, forest_type, region, height_class) |>
  split_by_level()
tab$h_density_height$forest_type

save_tables(tab$h_density_height, "05h_density_height")

## + Figures ####
fig$h_density_height <- tab$h_density_height$forest_type |>
  mutate(density_me = density_ha * density_mep / 100) |>
  ggplot(aes(x = height_class, y = density_ha, fill = forest_type)) +
  geom_col(width = 0.8) +
  geom_errorbar(aes(ymin = pmax(density_ha - density_me, 0), ymax = density_ha + density_me), width = 0.2) +
  scale_fill_manual(values = defaults$pal_ftype, guide = "none") +
  facet_wrap(~forest_type, scales = "free_y") +
  labs(x = "Height class (m)", y = "Tree density (trees/ha)", caption = "Error bars: 90% confidence interval")
fig$h_density_height

fig$h_density_height_region <- tab$h_density_height$forest_type_region |>
  ggplot(aes(x = height_class, y = density_ha, fill = forest_type)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = defaults$pal_ftype, guide = "none") +
  facet_grid(forest_type ~ region, scales = "free_y") +
  labs(x = "Height class (m)", y = "Tree density (trees/ha)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), strip.text.y = element_text(size = 7))
fig$h_density_height_region

ggsave("results/figures/05h_density_height.png", fig$h_density_height, width = 20, height = 14, units = "cm", dpi = 300)
ggsave("results/figures/05h_density_height_region.png", fig$h_density_height_region, width = 26, height = 18, units = "cm", dpi = 300)
