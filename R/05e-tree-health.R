
## e. Healthy vs damaged trees ####

## Tree density (trees/ha) by overall tree condition, two-phase estimates (nfi_aggregate2 sub-domains).
## Damaged = slightly or severely affected, dying standing or fallen trees.
## Also: crown condition of the live trees, recorded causes of damage of the damaged trees, and causes by
## severity of the damage (overall condition).

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

## + Crown condition ####
tmp$crown_levels <- c("Healthy", "Declining health", "Unhealthy", "Dying", "Dead")

## Check: crown condition of all live trees
table(tmp$tree_health$tree_condition_crown_label, useNA = "ifany")

res_crown <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = tree_count, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(
    mutate(tmp$tree_health, tree_condition_crown_label = replace_na(tree_condition_crown_label, "Not recorded")),
    tree_condition_crown_label, tree_weight, defaults$plot_size_dbh20, .name = "tree_count"
  ),
  .class_sub = tree_condition_crown_label
  ) |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  mutate(
    density = Yd / Xd,
    tree_condition_crown_label = factor(tree_condition_crown_label, levels = c(tmp$crown_levels, "Not recorded"))
  )

## + Causes of damage ####
## Damaged trees (overall condition not healthy) by recorded cause. Several causes can be recorded for a tree:
## the tree counts once for each of its causes, so the shares by cause do not add up to 100%.
## Damaged trees without a recorded cause: 'Not recorded' (01-get-data.R).
tmp$cause_levels <- c("Disease/Fungi", "Insects", "Fires", "Climate", "Humans", "Animals", "Other", "Not recorded")
tmp$severity_levels <- c("Slightly affected", "Severely affected", "Dying standing tree", "Dying fallen tree")

tmp$tree_cause <- tmp$tree_health |>
  filter(tree_health == "Damaged") |>
  separate_longer_delim(tree_condition_causes, delim = "; ") |>
  mutate(tree_cause = replace_na(tree_condition_causes, "Not recorded"))

## Check: causes as expected, no missing
table(tmp$tree_cause$tree_cause, useNA = "ifany")
setdiff(unique(tmp$tree_cause$tree_cause), tmp$cause_levels)

res_cause <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = tree_count, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(tmp$tree_cause, tree_cause, tree_weight, defaults$plot_size_dbh20, .name = "tree_count"),
  .class_sub = tree_cause
  ) |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  mutate(density = Yd / Xd)

## Causes by severity of the damage (overall condition of the damaged trees)
res_cause_severity <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = tree_count, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(
    mutate(tmp$tree_cause, cause_severity = paste(tree_cause, tree_condition_overall_label, sep = "__")),
    cause_severity, tree_weight, defaults$plot_size_dbh20, .name = "tree_count"
  ),
  .class_sub = cause_severity
  ) |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  separate_wider_delim(cause_severity, delim = "__", names = c("tree_cause", "tree_condition_overall_label")) |>
  mutate(density = Yd / Xd)

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

## Crown condition: density (trees/ha) with margin of error and share of live trees by crown class
tab$e_crown <- res_crown |>
  mutate(pct = round(density / sum(density) * 100, 1), .by = c(level, domain)) |>
  transmute(level, forest_type, region, n_pp, tree_condition_crown_label, density_ha = round(density, 1), density_mep = Rd_mep, pct) |>
  arrange(level, forest_type, region, tree_condition_crown_label) |>
  split_by_level()

## Causes: density of the damaged trees with each cause (trees/ha) with margin of error, share of the live
## trees and of the damaged trees
tmp$density_live <- res_health |>
  summarise(density_live = sum(density), .by = c(level, domain))
tmp$density_damaged <- res_health |>
  filter(tree_health == "Damaged") |>
  select(level, domain, density_damaged = density)

tab$e_cause <- res_cause |>
  left_join(tmp$density_live, by = join_by(level, domain)) |>
  left_join(tmp$density_damaged, by = join_by(level, domain)) |>
  transmute(
    level, forest_type, region, n_pp,
    tree_cause = factor(tree_cause, levels = tmp$cause_levels),
    density_ha = round(density, 1), density_mep = Rd_mep,
    pct_live = round(density / density_live * 100, 1),
    pct_damaged = round(density / density_damaged * 100, 1)
  ) |>
  arrange(level, forest_type, region, tree_cause) |>
  split_by_level()

## Causes by severity: share of the damaged trees with a cause in each severity class
tab$e_cause_severity <- res_cause_severity |>
  mutate(pct = round(density / sum(density) * 100, 1), .by = c(level, domain, tree_cause)) |>
  transmute(
    level, forest_type, region, n_pp,
    tree_cause = factor(tree_cause, levels = tmp$cause_levels),
    tree_condition_overall_label = factor(tree_condition_overall_label, levels = tmp$severity_levels),
    density_ha = round(density, 1), pct
  ) |>
  arrange(level, forest_type, region, tree_cause, tree_condition_overall_label) |>
  split_by_level()

## Check: severity shares add up to 100% for each cause, densities by cause match res_cause
tab$e_cause_severity$country |> summarise(pct = sum(pct), density_ha = sum(density_ha), .by = tree_cause)
tab$e_cause$country |> select(tree_cause, density_ha)

tab$e_health

save_tables(tab$e_health, "05e_health")
save_tables(tab$e_condition, "05e_condition")
save_tables(tab$e_crown, "05e_crown")
save_tables(tab$e_cause, "05e_cause")
save_tables(tab$e_cause_severity, "05e_cause_severity")

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

## Crown condition: share of live trees by crown class, country level and forest types
tmp$pal_crown <- c(
  "Healthy" = "#2e7d32", "Declining health" = "#fdd835", "Unhealthy" = "#fb8c00",
  "Dying" = "#c62828", "Dead" = "#6d1b1b", "Not recorded" = "#bdbdbd"
)

fig$e_crown <- bind_rows(tab$e_crown$country, tab$e_crown$forest_type) |>
  mutate(forest_type = fct_recode(forest_type, "All forest" = "All")) |>
  ggplot(aes(x = forest_type, y = pct, fill = tree_condition_crown_label)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = tmp$pal_crown) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "", y = "Share of live trees (%)", fill = "") +
  theme(legend.position = "bottom")
fig$e_crown

## Causes by severity: damaged trees (trees/ha) by cause and severity, country level
fig$e_cause_severity <- tab$e_cause_severity$country |>
  mutate(tree_cause = fct_rev(fct_drop(tree_cause))) |>
  ggplot(aes(x = density_ha, y = tree_cause, fill = tree_condition_overall_label)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = tmp$pal_condition) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(
    x = "Damaged trees (trees/ha)", y = "", fill = "",
    caption = "A tree with several causes is counted for each cause"
  ) +
  theme(legend.position = "bottom")
fig$e_cause_severity

ggsave("results/figures/05e_crown.png", fig$e_crown, width = 18, height = 11, units = "cm", dpi = 300)
ggsave("results/figures/05e_cause_severity.png", fig$e_cause_severity, width = 18, height = 11, units = "cm", dpi = 300)
