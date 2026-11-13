
## e. Healthy vs damaged trees ####

## Tree density (trees/ha) by overall tree condition, two-phase estimates (nfi_aggregate2 sub-domains).
## Damaged = slightly or severely affected, dying standing or fallen trees.

tmp$tree_health <- tree |>
  mutate(tree_health = if_else(tree_condition_overall_label == "Healthy", "Healthy", "Damaged"))

## Check: no missing condition
table(tmp$tree_health$tree_condition_overall_label, useNA = "ifany")

## + Healthy vs damaged ####
res_health <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = tree_count, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(tmp$tree_health, tree_health, tree_weight, defaults$plot_size_dbh20, .name = "tree_count"),
  .class_sub = tree_health
  ) |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  mutate(density = Yd / Xd)

## + Detailed condition classes ####
res_condition <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = tree_count, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(tmp$tree_health, tree_condition_overall_label, tree_weight, defaults$plot_size_dbh20, .name = "tree_count"),
  .class_sub = tree_condition_overall_label
  ) |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  mutate(
    density = Yd / Xd,
    tree_condition_overall_label = factor(
      tree_condition_overall_label,
      levels = c("Healthy", "Slightly affected", "Severely affected", "Dying standing tree", "Dying fallen tree")
    )
  )

## + Tables ####
## Densities (trees/ha) with margin of error, share of healthy trees and ratio healthy / damaged
tab$e_health <- res_health |>
  select(level, forest_type, region, n_pp, tree_health, density, Rd_mep) |>
  pivot_wider(names_from = tree_health, values_from = c(density, Rd_mep), values_fill = 0) |>
  transmute(
    level, forest_type, region, n_pp,
    healthy_ha = round(density_Healthy, 1), healthy_mep = Rd_mep_Healthy,
    damaged_ha = round(density_Damaged, 1), damaged_mep = Rd_mep_Damaged,
    pct_healthy = round(density_Healthy / (density_Healthy + density_Damaged) * 100, 1),
    ratio_healthy_damaged = round(density_Healthy / density_Damaged, 2)
  ) |>
  arrange(level, forest_type, region) |>
  split_by_level()

tab$e_condition <- res_condition |>
  mutate(pct = round(density / sum(density) * 100, 1), .by = c(level, domain)) |>
  transmute(level, forest_type, region, n_pp, tree_condition_overall_label, density_ha = round(density, 1), density_mep = Rd_mep, pct) |>
  arrange(level, forest_type, region, tree_condition_overall_label) |>
  split_by_level()

tab$e_health

save_tables(tab$e_health, "05e_health")
save_tables(tab$e_condition, "05e_condition")

## + Figures ####
tmp$pal_condition <- c(
  "Healthy" = "#2e7d32", "Slightly affected" = "#fdd835", "Severely affected" = "#fb8c00",
  "Dying standing tree" = "#c62828", "Dying fallen tree" = "#6d1b1b"
)

fig$e_condition <- bind_rows(tab$e_condition$forest_type, tab$e_condition$forest_type_region) |>
  filter(forest_type != "Non-forest") |>
  ggplot(aes(x = region, y = pct, fill = tree_condition_overall_label)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = tmp$pal_condition) +
  facet_wrap(~forest_type, scales = "free_x") +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "", y = "Share of trees (%)", fill = "", caption = "All: forest type level estimate") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
fig$e_condition

ggsave("results/figures/05e_condition.png", fig$e_condition, width = 24, height = 11, units = "cm", dpi = 300)
