## i. Aboveground biomass ####

## AGB of live trees, two-phase estimates (nfi_aggregate2 sub-domains) for three tree groups, reported as rows:
## - Trees without baobab: the live tree table, as in the plot level totals (03-baseunit-calc.R)
## - Baobab: baobabs (Adansonia), AGB from the same Chave 2014 equation (02c-preparation-others.R)
## - All trees: both groups together

tmp$agb_groups <- c("Trees without baobab", "Baobab", "All trees")

tmp$agb_tree <- bind_rows(
  tree   |> select(cluster_no, plot_no, tree_lulc_no, tree_weight, tree_biomass_ag) |> mutate(tree_group = "Trees without baobab"),
  baobab |> select(cluster_no, plot_no, tree_lulc_no, tree_weight, tree_biomass_ag) |> mutate(tree_group = "Baobab")
)
tmp$agb_tree <- bind_rows(tmp$agb_tree, mutate(tmp$agb_tree, tree_group = "All trees"))

res_agb_levels <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = agb_tot, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(tmp$agb_tree, tree_group, tree_biomass_ag * tree_weight, defaults$plot_size_dbh20, .name = "agb_tot"),
  .class_sub = tree_group
  ) |>
  left_join(domain_lookup, by = join_by(level, domain))

## Check: trees without baobab give the same estimates as the plot level totals
res_agb_check <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = lulc_biomass_ag_tot, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater
  )
res_agb_levels |>
  filter(tree_group == "Trees without baobab") |>
  left_join(select(res_agb_check, level, domain, Yd_check = Yd), by = join_by(level, domain)) |>
  summarise(max_diff = max(abs(Yd - Yd_check)))

## + Tables ####
## AGB in t/ha, total AGB in t, uncertainty as margin of error (%) at 90% confidence
tab$i_biomass <- res_agb_levels |>
  mutate(tree_group = factor(tree_group, levels = tmp$agb_groups)) |>
  transmute(
    level, forest_type, region, n_pp, tree_group,
    agb_ha = round(Yd / Xd, 1), agb_ha_mep = Rd_mep,
    area_ha = round(Xtot), agb_tot = round(Ytot), agb_tot_mep = Yd_mep
  ) |>
  arrange(level, forest_type, region, tree_group) |>
  split_by_level()
tab$i_biomass

save_tables(tab$i_biomass, "05i_biomass")

## + Figures ####
## Stacked groups, error bars of all trees
tmp$agb_fig <- tab$i_biomass$forest_type_region |>
  mutate(agb_ha_me = agb_ha * replace_na(agb_ha_mep, 0) / 100)

fig$i_biomass <- tmp$agb_fig |>
  filter(tree_group != "All trees") |>
  mutate(tree_group = fct_rev(fct_drop(tree_group))) |>
  ggplot(aes(x = region, y = agb_ha)) +
  geom_col(aes(fill = tree_group), width = 0.7) +
  geom_errorbar(
    data = filter(tmp$agb_fig, tree_group == "All trees"),
    aes(ymin = pmax(agb_ha - agb_ha_me, 0), ymax = agb_ha + agb_ha_me), width = 0.2
  ) +
  geom_text(
    data = filter(tmp$agb_fig, tree_group == "All trees"),
    aes(y = agb_ha + agb_ha_me, label = paste0("n=", n_pp)), vjust = -0.5, size = 2.5
  ) +
  scale_fill_manual(values = c("Trees without baobab" = "#558b2f", "Baobab" = "#d4a017")) +
  facet_wrap(~forest_type, nrow = 1, scales = "free_x") +
  labs(x = "", y = "Aboveground biomass (t/ha)", fill = "", caption = "Error bars: 90% confidence interval of all trees, n = number of clusters") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
fig$i_biomass

## Country level and forest types: stacked groups, error bars of all trees
tmp$agb_fig_ftype <- bind_rows(tab$i_biomass$country, tab$i_biomass$forest_type) |>
  mutate(
    forest_type = fct_recode(forest_type, "Country" = "All"),
    agb_ha_me = agb_ha * replace_na(agb_ha_mep, 0) / 100
  )

fig$i_biomass_ftype <- tmp$agb_fig_ftype |>
  filter(tree_group != "All trees") |>
  mutate(tree_group = fct_rev(fct_drop(tree_group))) |>
  ggplot(aes(x = forest_type, y = agb_ha)) +
  geom_col(aes(fill = tree_group), width = 0.7) +
  geom_errorbar(
    data = filter(tmp$agb_fig_ftype, tree_group == "All trees"),
    aes(ymin = pmax(agb_ha - agb_ha_me, 0), ymax = agb_ha + agb_ha_me), width = 0.2
  ) +
  geom_text(
    data = filter(tmp$agb_fig_ftype, tree_group == "All trees"),
    aes(y = agb_ha + agb_ha_me, label = paste0("n=", n_pp)), vjust = -0.5, size = 2.5
  ) +
  scale_fill_manual(values = c("Trees without baobab" = "#558b2f", "Baobab" = "#d4a017")) +
  labs(x = "", y = "Aboveground biomass (t/ha)", fill = "", caption = "Error bars: 90% confidence interval of all trees, n = number of clusters") +
  theme(legend.position = "bottom")
fig$i_biomass_ftype

ggsave("results/figures/05i_biomass_ftype.png", fig$i_biomass_ftype, width = 18, height = 11, units = "cm", dpi = 300)

## + Cluster level AGB by forest type ####
## AGB (t/ha) of all trees in each field cluster and forest type: sum of the section totals / sum of the
## section areas of the cluster in the forest type. Shows the variability between clusters.
tmp$agb_cluster <- ph2_data |>
  filter(access, !virtual, forest_type != "Non-forest") |>
  select(PSU, SSU, forest_type, ssu_area) |>
  left_join(
    tree_to_ssu(filter(tmp$agb_tree, tree_group == "All trees"), tree_group, tree_biomass_ag * tree_weight, defaults$plot_size_dbh20, .name = "agb_tot") |>
      select(PSU, SSU, agb_tot),
    by = join_by(PSU, SSU)
  ) |>
  summarise(agb_ha = sum(replace_na(agb_tot, 0)) / sum(ssu_area), .by = c(PSU, forest_type)) |>
  mutate(forest_type = factor(forest_type, levels = defaults$ftype_levels))

fig$i_biomass_cluster <- tmp$agb_cluster |>
  ggplot(aes(x = forest_type, y = agb_ha, fill = forest_type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.6) +
  geom_jitter(width = 0.15, height = 0, size = 1.2, alpha = 0.7) +
  geom_text(
    data = count(tmp$agb_cluster, forest_type),
    aes(x = forest_type, y = -8, label = paste0("n=", n)), inherit.aes = FALSE, size = 2.8
  ) +
  scale_fill_manual(values = defaults$pal_ftype, guide = "none") +
  labs(x = "", y = "Cluster aboveground biomass (t/ha)", caption = "Points: field clusters (all trees), n = number of clusters")
fig$i_biomass_cluster

ggsave("results/figures/05i_biomass_cluster.png", fig$i_biomass_cluster, width = 18, height = 11, units = "cm", dpi = 300)
ggsave("results/figures/05i_biomass.png", fig$i_biomass, width = 24, height = 11, units = "cm", dpi = 300)


tree |>
  filter(cluster_info_stratum == "mangrove forest") |>
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_biomass_ag)) +
  facet_wrap(~cluster_no)

names(tree)
table(tree$cluster_info_stratum)

tree |>
  filter(cluster_info_stratum == "mangrove forest") |>
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_biomass_ag)) +
  facet_wrap(~cluster_no)