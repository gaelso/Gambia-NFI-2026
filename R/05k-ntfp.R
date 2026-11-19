## k. Non-timber forest products (NTFP) ####

## NTFP recorded in the main plot, with one or more use categories (edible, medicinal, craft, resins & oil,
## other) and their availability in the plot (few, moderate, abundant). No quantities are recorded.
## NTFP are recorded at plot level, without LULC section information.
## - Share of area with NTFP (%): two-phase estimates of the share of the domain area with at least one NTFP of
##   each use category, with the plot as base unit and the cluster as PSU (share_plot_levels()). Accessible
##   plots without record have no NTFP.
## - Availability: share of the area with each use category (%) by the highest availability recorded in the
##   plot for this use, from the two-phase shares of area of each use x availability.
## - Products: number of clusters and plots with each product (descriptive counts, count_plots_by_level()).

tmp$ntfp_levels_use   <- c("Any NTFP", "Edible", "Medicinal", "Craft", "Resins & Oil", "Other")
tmp$ntfp_levels_avail <- c("Few", "Moderate", "Abundant")

## One row per NTFP record and use category
tmp$ntfp_use <- ntfp_type |>
  select(cluster_no, plot_no, ntfp_id, ntfp_use = ntfp_type_label, ntfp_status_label) |>
  bind_rows(ntfp |> select(cluster_no, plot_no, ntfp_id, ntfp_status_label) |> mutate(ntfp_use = "Any NTFP"))

## Check: no NTFP record outside the accessible field plots
tmp$ntfp_use |> anti_join(plot_info, by = join_by(cluster_no, plot_no)) |> nrow()

## + Share of area with NTFP, by use category ####
res_ntfp <- share_plot_levels(tmp$ntfp_use, ntfp_use)

## + Availability by use category ####
## Highest availability recorded in the plot for each use category
tmp$ntfp_avail <- tmp$ntfp_use |>
  mutate(ntfp_status_label = factor(ntfp_status_label, levels = tmp$ntfp_levels_avail, ordered = TRUE)) |>
  summarise(ntfp_avail = max(ntfp_status_label), .by = c(cluster_no, plot_no, ntfp_use)) |>
  mutate(use_avail = paste(ntfp_use, ntfp_avail, sep = "|"))

res_ntfp_avail <- share_plot_levels(tmp$ntfp_avail, use_avail) |>
  separate_wider_delim(use_avail, delim = "|", names = c("ntfp_use", "ntfp_avail"))

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
  transmute(level, forest_type, region, n_pp, ntfp_use, n_clusters, n_plots, area_ntfp_pct = round(share * 100, 1), area_ntfp_mep = Rd_mep, n_products) |>
  mutate(forest_type = factor(forest_type, levels = defaults$ftype_levels), region = factor(region, levels = defaults$region_levels)) |>
  arrange(level, forest_type, region, ntfp_use) |>
  split_by_level()
tab$k_ntfp$forest_type

## Share of the area with each use category (%) by highest availability, one column per availability class.
## Domains without NTFP (e.g. mangrove forest): row 'Any NTFP' with 0% of the area.
tab$k_ntfp_avail <- res_ntfp_avail |>
  mutate(ntfp_avail = factor(ntfp_avail, levels = tmp$ntfp_levels_avail)) |>
  mutate(pct = share / sum(share) * 100, .by = c(level, domain, ntfp_use)) |>
  filter(!is.na(pct)) |>
  select(level, forest_type, region, ntfp_use, ntfp_avail, pct) |>
  pivot_wider(names_from = ntfp_avail, values_from = pct, values_fill = 0, names_expand = TRUE) |>
  right_join(
    res_ntfp |> filter(share > 0 | ntfp_use == "Any NTFP") |> select(level, forest_type, region, ntfp_use, n_pp, n_clusters, n_plots, share, Rd_mep),
    by = join_by(level, forest_type, region, ntfp_use)
  ) |>
  transmute(
    level, forest_type, region, n_pp,
    ntfp_use = factor(ntfp_use, levels = tmp$ntfp_levels_use),
    n_clusters, n_plots, area_ntfp_pct = round(share * 100, 1), area_ntfp_mep = Rd_mep,
    pct_few = round(replace_na(Few, 0), 1), pct_moderate = round(replace_na(Moderate, 0), 1), pct_abundant = round(replace_na(Abundant, 0), 1)
  ) |>
  arrange(level, forest_type, region, ntfp_use) |>
  split_by_level()
tab$k_ntfp_avail$forest_type

## Top 10 products by number of clusters and plots, country and forest type levels
tab$k_ntfp_product <- res_ntfp_product |>
  filter(level != "forest_type_region", forest_type != "Non-forest") |>
  arrange(desc(n_clusters), desc(n_plots)) |>
  slice_head(n = 10, by = c(level, forest_type, region)) |>
  transmute(level, forest_type, region, ntfp_name_label, n_clusters, n_plots) |>
  mutate(forest_type = factor(forest_type, levels = defaults$ftype_levels), region = factor(region, levels = defaults$region_levels)) |>
  arrange(level, forest_type, desc(n_clusters), desc(n_plots)) |>
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
