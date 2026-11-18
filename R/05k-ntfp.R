## k. Non-timber forest products (NTFP) ####

## NTFP recorded in the main plot, with one or more use categories (edible, medicinal, craft, resins & oil,
## other) and their availability in the plot (few, moderate, abundant). No quantities are recorded.
## - Share of area with NTFP (%): two-phase estimates (nfi_aggregate2 sub-domains) of the share of the domain
##   area in plots with at least one NTFP of each use category. The plot presence applies to all the LULC
##   sections of the plot (plot_to_ssu()). Accessible plots without record have no NTFP.
## - Availability: number and share of accessible field plots with each use category, by the highest
##   availability recorded in the plot for this use (count_plots_by_level()).
## - Products: number and share of accessible field plots with each product (count_plots_by_level()).

tmp$ntfp_levels_use   <- c("Any NTFP", "Edible", "Medicinal", "Craft", "Resins & Oil", "Other")
tmp$ntfp_levels_avail <- c("Few", "Moderate", "Abundant")

## One row per NTFP record and use category
tmp$ntfp_use <- ntfp_type |>
  select(cluster_no, plot_no, ntfp_id, ntfp_use = ntfp_type_label, ntfp_status_label) |>
  bind_rows(ntfp |> select(cluster_no, plot_no, ntfp_id, ntfp_status_label) |> mutate(ntfp_use = "Any NTFP"))

## Check: no NTFP record outside the accessible field plots
tmp$ntfp_use |> anti_join(plot_info, by = join_by(cluster_no, plot_no)) |> nrow()

## + Share of area with NTFP, by use category ####
tmp$ntfp_plot_use <- tmp$ntfp_use |>
  distinct(cluster_no, plot_no, ntfp_use) |>
  mutate(presence = 1)

res_ntfp <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = ntfp_area, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = plot_to_ssu(tmp$ntfp_plot_use, ph2_data, ntfp_use, presence, .name = "ntfp_area"),
  .class_sub = ntfp_use
  ) |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  mutate(share = Yd / Xd)

## + Availability by use category ####
## Highest availability recorded in the plot for each use category
tmp$ntfp_avail <- tmp$ntfp_use |>
  mutate(ntfp_status_label = factor(ntfp_status_label, levels = tmp$ntfp_levels_avail, ordered = TRUE)) |>
  summarise(ntfp_avail = max(ntfp_status_label), .by = c(cluster_no, plot_no, ntfp_use)) |>
  mutate(use_avail = paste(ntfp_use, ntfp_avail, sep = "|"))

res_ntfp_avail <- count_plots_by_level(tmp$ntfp_avail, plot_info, use_avail) |>
  separate_wider_delim(use_avail, delim = "|", names = c("ntfp_use", "ntfp_avail")) |>
  select(level, forest_type, region, ntfp_use, ntfp_avail, n_plots, n_plots_total, pct_plots)

## + Products ####
res_ntfp_product <- count_plots_by_level(ntfp, plot_info, ntfp_name_label)

## + Tables ####
## Share of the domain area (%) with NTFP, with margin of error, and number of products recorded
tab$k_ntfp <- res_ntfp |>
  left_join(
    res_ntfp_product |> summarise(n_products = n_distinct(ntfp_name_label), .by = c(level, forest_type, region)),
    by = join_by(level, forest_type, region)
  ) |>
  mutate(
    ntfp_use = factor(ntfp_use, levels = tmp$ntfp_levels_use),
    n_products = if_else(ntfp_use == "Any NTFP", replace_na(n_products, 0L), NA_integer_)
  ) |>
  transmute(level, forest_type, region, n_pp, ntfp_use, area_ntfp_pct = round(share * 100, 1), area_ntfp_mep = Rd_mep, n_products) |>
  mutate(forest_type = factor(forest_type, levels = defaults$ftype_levels), region = factor(region, levels = defaults$region_levels)) |>
  arrange(level, forest_type, region, ntfp_use) |>
  split_by_level()
tab$k_ntfp$forest_type

## Plots by use category and highest availability, one column per availability class
tab$k_ntfp_avail <- res_ntfp_avail |>
  mutate(ntfp_avail = factor(ntfp_avail, levels = tmp$ntfp_levels_avail)) |>
  arrange(ntfp_avail) |>
  pivot_wider(id_cols = c(level, forest_type, region, ntfp_use, n_plots_total), names_from = ntfp_avail, values_from = n_plots, values_fill = 0) |>
  mutate(
    n_plots = Few + Moderate + Abundant,
    pct_plots = round(n_plots / n_plots_total * 100, 1),
    ntfp_use = factor(ntfp_use, levels = tmp$ntfp_levels_use),
    forest_type = factor(forest_type, levels = defaults$ftype_levels),
    region = factor(region, levels = defaults$region_levels)
  ) |>
  select(level, forest_type, region, ntfp_use, n_plots_total, n_plots, pct_plots, n_few = Few, n_moderate = Moderate, n_abundant = Abundant)

## Domains without NTFP (e.g. mangrove forest): one row 'Any NTFP' with 0 plots
tmp$ntfp_plots_total <- count_plots_by_level(mutate(plot_info, all = "all"), plot_info, all) |>
  mutate(forest_type = factor(forest_type, levels = defaults$ftype_levels), region = factor(region, levels = defaults$region_levels))

tab$k_ntfp_avail <- tab$k_ntfp_avail |>
  bind_rows(
    tmp$ntfp_plots_total |>
      anti_join(tab$k_ntfp_avail, by = join_by(level, forest_type, region)) |>
      transmute(
        level, forest_type, region, ntfp_use = factor("Any NTFP", levels = tmp$ntfp_levels_use), n_plots_total,
        n_plots = 0L, pct_plots = 0, n_few = 0L, n_moderate = 0L, n_abundant = 0L
      )
  ) |>
  arrange(level, forest_type, region, ntfp_use) |>
  split_by_level()
tab$k_ntfp_avail$forest_type

## Top 10 products by number of plots, country and forest type levels
tab$k_ntfp_product <- res_ntfp_product |>
  filter(level != "forest_type_region", forest_type != "Non-forest") |>
  slice_max(n_plots, n = 10, with_ties = FALSE, by = c(level, forest_type, region)) |>
  mutate(forest_type = factor(forest_type, levels = defaults$ftype_levels)) |>
  transmute(level, forest_type, region, ntfp_name_label, n_plots, n_plots_total, pct_plots) |>
  mutate(forest_type = factor(forest_type, levels = defaults$ftype_levels), region = factor(region, levels = defaults$region_levels)) |>
  arrange(level, forest_type, desc(n_plots)) |>
  split_by_level()
tab$k_ntfp_product$country

save_tables(tab$k_ntfp, "05k_ntfp")
save_tables(tab$k_ntfp_avail, "05k_ntfp_avail")
save_tables(tab$k_ntfp_product, "05k_ntfp_product")

## + Figures ####
fig$k_ntfp <- tab$k_ntfp$forest_type |>
  filter(forest_type != "Non-forest") |>
  mutate(area_ntfp_me = area_ntfp_pct * replace_na(area_ntfp_mep, 0) / 100) |>
  ggplot(aes(x = ntfp_use, y = area_ntfp_pct, fill = forest_type)) +
  geom_col(position = position_dodge(preserve = "single", width = 0.85), width = 0.8) +
  geom_errorbar(
    aes(ymin = pmax(area_ntfp_pct - area_ntfp_me, 0), ymax = pmin(area_ntfp_pct + area_ntfp_me, 100)),
    position = position_dodge(preserve = "single", width = 0.85), width = 0.2
  ) +
  scale_fill_manual(values = defaults$pal_ftype) +
  labs(x = "", y = "Share of area with NTFP (%)", fill = "", caption = "Error bars: 90% confidence interval") +
  theme(legend.position = "bottom")
fig$k_ntfp

ggsave("results/figures/05k_ntfp.png", fig$k_ntfp, width = 18, height = 11, units = "cm", dpi = 300)
