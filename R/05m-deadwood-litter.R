## m. Deadwood and litter ####

## Two-phase estimates (nfi_aggregate2 sub-domains), see the methods (data analysis, forest health).
## - Standing deadwood: dead standing trees measured with the live trees (nested plots, tree_weight).
##   Stems/ha and volume (m3/ha) by decomposition status. Stem volume = 0.67 x basal area x height (as live
##   trees, 02c-preparation-others.R). Biomass (t/ha, for 05n-carbon.R): live tree equivalent AGB (Chave 2014,
##   measured height, species wood density) x 0.80 structural reduction factor (UNFCCC 2015, AR-TOOL12).
## - Lying deadwood: pieces with small-end diameter >= 10 cm and length >= 1 m intersecting the 20 m transect
##   of each plot. Volume (m3/ha) with Smalian's formula and the line-intersect estimator (Waddell 2002):
##   V = 10,000 pi / (2 L) x sum(v_i / l_i). Biomass: volume x wood density (default, species not recorded)
##   x density reduction factor of the decomposition class (UNFCCC 2015). The transect values apply to all the
##   LULC sections of the plot (plot_to_ssu()). Accessible plots without record have no lying deadwood.
## - Stumps: measured with the live trees (nested plots, tree_weight). Stumps/ha and volume (m3/ha) by
##   decomposition status. Volume = cylinder (diameter, stump height). Biomass: volume x species wood density
##   x density reduction factor of the decomposition class.
## - Litter: depth class of the litter subplot (no litter, < 1 cm, 1 to 5 cm, > 5 cm), share of the area by
##   class as for soil (05l-soil.R): field plots with an observation only.

defaults$dw_transect_length <- 20
defaults$dw_structure_factor <- 0.80
defaults$dw_decay <- c("Sound" = 1, "Intermediate" = 0.80, "Rotten" = 0.45)
tmp$dw_levels <- c("Sound", "Intermediate", "Rotten", "Not recorded", "Total")

## + Standing deadwood ####
tmp$dw_standing <- dw_standing |>
  mutate(
    dw_decomposition = replace_na(tree_stump_decomposition_label, "Not recorded"),
    dw_volume  = 0.67 * pi * (tree_dbh / 200)^2 * tree_height,
    dw_biomass = 0.0673 * (tree_wood_density * tree_dbh^2 * tree_height)^0.976 / 1000 * defaults$dw_structure_factor
  )

## Check: no missing value
tmp$dw_standing |> summarise(across(c(dw_volume, dw_biomass, tree_weight), \(x) sum(is.na(x))))

tmp$dw_standing_class <- bind_rows(tmp$dw_standing, mutate(tmp$dw_standing, dw_decomposition = "Total"))

res_dw_stems <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = dw_tot, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(tmp$dw_standing_class, dw_decomposition, tree_weight, defaults$plot_size_dbh20, .name = "dw_tot"),
  .class_sub = dw_decomposition
  ) |>
  left_join(domain_lookup, by = join_by(level, domain))

res_dw_volume <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = dw_tot, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(tmp$dw_standing_class, dw_decomposition, dw_volume * tree_weight, defaults$plot_size_dbh20, .name = "dw_tot"),
  .class_sub = dw_decomposition
  ) |>
  left_join(domain_lookup, by = join_by(level, domain))

## + Stumps ####
## Stumps measured with the live trees (nested plots, tree_weight): diameter and stump height.
## Volume: cylinder, pi x (d / 2)^2 x stump height, missing heights replaced by the median stump height.
## Biomass (t, for 05n-carbon.R): volume x species wood density x density reduction factor of the
## decomposition class (as lying deadwood). Dead roots are not included.
tmp$stump_h_median <- median(stump$tree_height, na.rm = TRUE)

tmp$stump <- stump |>
  mutate(
    dw_decomposition = replace_na(tree_stump_decomposition_label, "Not recorded"),
    stump_height = replace_na(tree_height, tmp$stump_h_median),
    dw_volume  = pi * (tree_dbh / 200)^2 * stump_height,
    dw_biomass = dw_volume * tree_wood_density * unname(defaults$dw_decay[tree_stump_decomposition_label])
  )

## Check: no missing value
tmp$stump |> summarise(across(c(dw_volume, dw_biomass, tree_weight), \(x) sum(is.na(x))))

tmp$stump_class <- bind_rows(tmp$stump, mutate(tmp$stump, dw_decomposition = "Total"))

res_stump_n <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = dw_tot, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(tmp$stump_class, dw_decomposition, tree_weight, defaults$plot_size_dbh20, .name = "dw_tot"),
  .class_sub = dw_decomposition
  ) |>
  left_join(domain_lookup, by = join_by(level, domain))

res_stump_volume <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = dw_tot, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tree_to_ssu(tmp$stump_class, dw_decomposition, dw_volume * tree_weight, defaults$plot_size_dbh20, .name = "dw_tot"),
  .class_sub = dw_decomposition
  ) |>
  left_join(domain_lookup, by = join_by(level, domain))

## + Lying deadwood ####
tmp$dw_lying <- data_clean$deadwood |>
  semi_join(plot_info, by = join_by(cluster_no, plot_no)) |>
  mutate(
    ## Piece volume / length (m2): mean of the end cross-section areas (Smalian)
    dw_v_l = pi / 8 * ((deadwood_dia_large / 100)^2 + (deadwood_dia_small / 100)^2),
    dw_decay = unname(defaults$dw_decay[deadwood_decompose_label])
  )

## Check: all pieces in accessible field plots, decomposition class known
nrow(data_clean$deadwood) - nrow(tmp$dw_lying)
sum(is.na(tmp$dw_lying$dw_decay))

tmp$dw_lying_plot <- tmp$dw_lying |>
  summarise(
    volume_ha  = 10000 * pi / (2 * defaults$dw_transect_length) * sum(dw_v_l),
    biomass_ha = 10000 * pi / (2 * defaults$dw_transect_length) * sum(dw_v_l * defaults$WD * dw_decay),
    .by = c(cluster_no, plot_no)
  )

res_dw_lying <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = dw_tot, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = plot_to_ssu(mutate(tmp$dw_lying_plot, dw_class = "Lying deadwood"), ph2_data, dw_class, volume_ha, .name = "dw_tot"),
  .class_sub = dw_class
  ) |>
  left_join(domain_lookup, by = join_by(level, domain))

## + Litter depth ####
tmp$litter_levels <- c("No litter", "< 1 cm", "1 to 5 cm", "> 5 cm")

tmp$litter <- plot |>
  semi_join(plot_info, by = join_by(cluster_no, plot_no)) |>
  mutate(litter_class = case_when(
    litter_presence %in% FALSE ~ "No litter",
    litter_presence %in% TRUE ~ litter_depth_label
  )) |>
  filter(!is.na(litter_class)) |>
  select(cluster_no, plot_no, litter_class) |>
  mutate(presence = 1)

## Phase 2 base units: field plots with a litter observation
tmp$ph2_litter <- ph2_data |>
  left_join(distinct(tmp$litter, cluster_no, plot_no, obs = TRUE), by = join_by(cluster_no, plot_no)) |>
  mutate(access = access & !virtual & replace_na(obs, FALSE))

res_litter <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = tmp$ph2_litter, .attr_y = litter_area, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = plot_to_ssu(tmp$litter, filter(tmp$ph2_litter, access), litter_class, presence, .name = "litter_area"),
  .class_sub = litter_class
  ) |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  mutate(share = Yd / Xd)

## Check: shares add up to 100%
res_litter |> summarise(share = sum(share), .by = c(level, domain)) |> filter(abs(share - 1) > 1e-6, share > 0)

## + Tables ####
## Standing deadwood: stems/ha and volume (m3/ha) by decomposition status, with margin of error
tab$m_dw_standing <- res_dw_stems |>
  select(level, forest_type, region, n_pp, dw_decomposition, Yd, Xd, Rd_mep) |>
  transmute(level, forest_type, region, n_pp, dw_decomposition, stems_ha = round(Yd / Xd, 2), stems_mep = Rd_mep) |>
  left_join(
    res_dw_volume |> transmute(level, forest_type, region, dw_decomposition, dw_volume_ha = round(Yd / Xd, 2), dw_volume_mep = Rd_mep),
    by = join_by(level, forest_type, region, dw_decomposition)
  ) |>
  mutate(dw_decomposition = factor(dw_decomposition, levels = tmp$dw_levels)) |>
  arrange(level, forest_type, region, dw_decomposition) |>
  split_by_level()
tab$m_dw_standing$forest_type

## Stumps: stumps/ha and volume (m3/ha) by decomposition status, with margin of error
tab$m_stump <- res_stump_n |>
  transmute(level, forest_type, region, n_pp, dw_decomposition, stems_ha = round(Yd / Xd, 2), stems_mep = Rd_mep) |>
  left_join(
    res_stump_volume |> transmute(level, forest_type, region, dw_decomposition, dw_volume_ha = round(Yd / Xd, 2), dw_volume_mep = Rd_mep),
    by = join_by(level, forest_type, region, dw_decomposition)
  ) |>
  mutate(dw_decomposition = factor(dw_decomposition, levels = tmp$dw_levels)) |>
  arrange(level, forest_type, region, dw_decomposition) |>
  split_by_level()
tab$m_stump$country

## Lying deadwood: volume (m3/ha) with margin of error
tab$m_dw_lying <- res_dw_lying |>
  transmute(level, forest_type, region, n_pp, dw_volume_ha = round(Yd / Xd, 2), dw_volume_mep = Rd_mep) |>
  arrange(level, forest_type, region) |>
  split_by_level()
tab$m_dw_lying$forest_type

## Litter: share of area (%) by depth class
tab$m_litter <- res_litter |>
  filter(Xd > 0) |>
  mutate(litter_class = factor(litter_class, levels = tmp$litter_levels)) |>
  transmute(level, forest_type, region, n_pp, litter_class, litter_pct = round(share * 100, 1), litter_mep = Rd_mep) |>
  arrange(level, forest_type, region, litter_class) |>
  split_by_level()
tab$m_litter$forest_type

save_tables(tab$m_dw_standing, "05m_dw_standing")
save_tables(tab$m_stump, "05m_stump")
save_tables(tab$m_dw_lying, "05m_dw_lying")
save_tables(tab$m_litter, "05m_litter")

## + Figures ####
fig$m_litter <- bind_rows(tab$m_litter$country, tab$m_litter$forest_type) |>
  mutate(forest_type = fct_recode(forest_type, "All forest" = "All")) |>
  ggplot(aes(x = forest_type, y = litter_pct, fill = litter_class)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("No litter" = "#e0e0e0", "< 1 cm" = "#d9c8b4", "1 to 5 cm" = "#a0785a", "> 5 cm" = "#4e342e")) +
  labs(x = "", y = "Share of area (%)", fill = "Litter depth") +
  theme(legend.position = "bottom")
fig$m_litter

ggsave("results/figures/05m_litter.png", fig$m_litter, width = 18, height = 11, units = "cm", dpi = 300)
