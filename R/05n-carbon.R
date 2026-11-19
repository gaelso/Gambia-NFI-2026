## n. Carbon stocks ####

## Carbon stocks (t C/ha and total t C) by pool and in total:
## - Aboveground biomass (AGB): live trees and baobabs (05i-biomass.R)
## - Belowground biomass (BGB): AGB x root-to-shoot ratio by forest type of the LULC section (02c-preparation-others.R)
## - Standing deadwood, stumps and lying deadwood: 05m-deadwood-litter.R. Dead roots are not included.
## - Litter: IPCC Tier 1 default litter carbon stocks of mature forests, by forest type of the LULC section:
##   - Closed and open forests: tropical dry forest, all vegetation types, 2.4 t C/ha, range 2.1-2.7
##     (IPCC 2019 Refinement, Vol. 4, Table 2.2 Updated), applied to the whole country as the
##     root-to-shoot ratio.
##   - Mangrove forest: 0.7 t C/ha, 95% CI 0-1.3 (IPCC 2013 Wetlands Supplement, Table 4.7).
##   The litter depth classes measured in the field (05m-deadwood-litter.R) have no mass and are not used.
## Carbon fraction: defaults$CF (0.47, IPCC 2006) for the biomass pools. The litter defaults are carbon stocks.
##
## Uncertainty (margin of error at 90% confidence, % of the estimate):
## - Each pool: sampling error of the two-phase estimate (nfi_aggregate2).
## - BGB and litter: sampling error combined with the uncertainty of the IPCC default (product of two
##   quantities, IPCC approach 1): U = sqrt(U_sampling^2 + U_default^2), U_default = z(0.95) x SE / default.
##   SE from the 95% CI or range (CI width / (2 x 1.96)), or the SD given by IPCC.
##   At country level, the default uncertainties are weighted by the pool totals of the forest types. Forest
##   types with the same default (closed and open forests) are fully correlated: their totals are added
##   before combining with the other defaults.
## - Total: sum of the pools, IPCC approach 1: U_total = sqrt(sum((U_i x C_i)^2)) / sum(C_i). The covariance
##   between pools (same plots, BGB from AGB) is not included.

tmp$c_pools <- c("Aboveground biomass", "Belowground biomass", "Standing deadwood", "Stumps", "Lying deadwood", "Litter")

## + IPCC Tier 1 litter carbon stocks ####
defaults$litter_c <- tibble(
  litter_class = c("Mangrove", "Dry forest"),
  litter_c     = c(0.7, 2.4),
  litter_c_se  = c((1.3 - 0) / (2 * qnorm(0.975)), (2.7 - 2.1) / (2 * qnorm(0.975)))
)

## + Pool totals of the SSUs ####
## Biomass pools in t of dry matter, litter in t C
tmp$c_tree <- bind_rows(tree, baobab) |>
  select(cluster_no, plot_no, tree_lulc_no, tree_weight, tree_biomass_ag, tree_biomass_bg)

## Litter: default (t C/ha) x area of the forest LULC sections of the accessible field plots
tmp$c_litter <- ph2_data |>
  filter(access, !virtual, forest_type != "Non-forest") |>
  transmute(
    PSU, SSU, pool = tmp$c_pools[6],
    pool_tot = ssu_area * if_else(forest_type == "Mangrove Forest", defaults$litter_c$litter_c[1], defaults$litter_c$litter_c[2])
  )

tmp$c_ssu <- bind_rows(
  tree_to_ssu(mutate(tmp$c_tree, pool = tmp$c_pools[1]), pool, tree_biomass_ag * tree_weight, defaults$plot_size_dbh20, .name = "pool_tot"),
  tree_to_ssu(mutate(tmp$c_tree, pool = tmp$c_pools[2]), pool, tree_biomass_bg * tree_weight, defaults$plot_size_dbh20, .name = "pool_tot"),
  tree_to_ssu(mutate(tmp$dw_standing, pool = tmp$c_pools[3]), pool, dw_biomass * tree_weight, defaults$plot_size_dbh20, .name = "pool_tot"),
  tree_to_ssu(mutate(tmp$stump, pool = tmp$c_pools[4]), pool, dw_biomass * tree_weight, defaults$plot_size_dbh20, .name = "pool_tot"),
  plot_to_ssu(mutate(tmp$dw_lying_plot, pool = tmp$c_pools[5]), ph2_data, pool, biomass_ha, .name = "pool_tot"),
  tmp$c_litter
) |>
  select(PSU, SSU, pool, pool_tot)

res_carbon <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = pool_tot, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = tmp$c_ssu,
  .class_sub = pool
  ) |>
  left_join(domain_lookup, by = join_by(level, domain))

## Check: AGB of the carbon pools = AGB of all trees in 05i-biomass.R
res_carbon |>
  filter(pool == tmp$c_pools[1]) |>
  left_join(filter(res_agb_levels, tree_group == "All trees") |> select(level, domain, Yd_agb = Yd), by = join_by(level, domain)) |>
  summarise(max_diff = max(abs(Yd - Yd_agb)))

## Check: litter per ha = default at forest type level
res_carbon |> filter(pool == tmp$c_pools[6], level == "forest_type") |> transmute(forest_type, litter_c_ha = Yd / Xd)

## + Uncertainty of the IPCC defaults ####
## Relative uncertainty (%) at 90% confidence of the default of a pool and forest type, and the default it
## comes from (forest types with the same default are correlated)
tmp$z90 <- qnorm(1 - defaults$ci_alpha / 2)

tmp$def_unc <- bind_rows(
  defaults$RS |> transmute(pool = tmp$c_pools[2], def_class = rs_class, def_u = tmp$z90 * rs_se / rs * 100),
  defaults$litter_c |> transmute(pool = tmp$c_pools[6], def_class = litter_class, def_u = tmp$z90 * litter_c_se / litter_c * 100)
)

tmp$def_u_ftype <- res_carbon |>
  filter(level != "country", forest_type != "Non-forest") |>
  distinct(level, forest_type, region, pool, Ytot) |>
  mutate(def_class = if_else(forest_type == "Mangrove Forest", "Mangrove", "Dry forest")) |>
  left_join(tmp$def_unc, by = join_by(pool, def_class)) |>
  mutate(def_u = replace_na(def_u, 0))

## Country level: totals of the forest types added by default, then combined in quadrature
tmp$def_u_country <- tmp$def_u_ftype |>
  filter(level == "forest_type") |>
  summarise(Ytot = sum(Ytot), def_u = first(def_u), .by = c(pool, def_class)) |>
  summarise(def_u = sqrt(sum((def_u * Ytot)^2)) / sum(Ytot), .by = pool) |>
  mutate(level = "country", forest_type = "All", region = "All")

tmp$def_u <- bind_rows(select(tmp$def_u_ftype, level, forest_type, region, pool, def_u), tmp$def_u_country)

## + Carbon by pool and total ####
tmp$carbon_pool <- res_carbon |>
  left_join(tmp$def_u, by = join_by(level, forest_type, region, pool)) |>
  mutate(
    cf    = if_else(pool == tmp$c_pools[6], 1, defaults$CF),
    c_ha  = Yd / Xd * cf,
    c_tot = Ytot * cf,
    def_u = replace_na(def_u, 0),
    ## Sampling error only for the domains with at least 2 plots, as the other estimates
    u_ha  = sqrt(Rd_mep^2 + def_u^2),
    u_tot = sqrt(Yd_mep^2 + def_u^2)
  ) |>
  select(level, forest_type, region, n_pp, pool, c_ha, u_ha, c_tot, u_tot)

tmp$carbon_total <- tmp$carbon_pool |>
  summarise(
    u_ha  = sqrt(sum((u_ha * c_ha)^2)) / sum(c_ha),
    u_tot = sqrt(sum((u_tot * c_tot)^2)) / sum(c_tot),
    c_ha  = sum(c_ha),
    c_tot = sum(c_tot),
    .by = c(level, forest_type, region, n_pp)
  ) |>
  mutate(pool = "Total")

## + Tables ####
## Forest type and region back to factors (character after the join with the default uncertainties)
tab$n_carbon <- bind_rows(tmp$carbon_pool, tmp$carbon_total) |>
  mutate(
    pool = factor(pool, levels = c(tmp$c_pools, "Total")),
    forest_type = factor(forest_type, levels = defaults$ftype_levels),
    region = factor(region, levels = defaults$region_levels)
  ) |>
  transmute(
    level, forest_type, region, n_pp, pool,
    carbon_ha = round(c_ha, 2), carbon_ha_mep = round(u_ha, 2),
    carbon_tot = round(c_tot), carbon_tot_mep = round(u_tot, 2)
  ) |>
  arrange(level, forest_type, region, pool) |>
  split_by_level()
tab$n_carbon$country
tab$n_carbon$forest_type

save_tables(tab$n_carbon, "05n_carbon")

## + Figures ####
tmp$carbon_fig <- bind_rows(tab$n_carbon$country, tab$n_carbon$forest_type) |>
  mutate(forest_type = fct_recode(forest_type, "Country" = "All"))

fig$n_carbon <- tmp$carbon_fig |>
  filter(pool != "Total") |>
  mutate(pool = fct_rev(fct_drop(pool))) |>
  ggplot(aes(x = forest_type, y = carbon_ha)) +
  geom_col(aes(fill = pool), width = 0.7) +
  geom_errorbar(
    data = filter(tmp$carbon_fig, pool == "Total") |> mutate(carbon_me = carbon_ha * replace_na(carbon_ha_mep, 0) / 100),
    aes(ymin = pmax(carbon_ha - carbon_me, 0), ymax = carbon_ha + carbon_me), width = 0.2
  ) +
  scale_fill_manual(values = c(
    "Aboveground biomass" = "#558b2f", "Belowground biomass" = "#8d6e63",
    "Standing deadwood" = "#ffb74d", "Stumps" = "#c62828", "Lying deadwood" = "#e65100",
    "Litter" = "#d7ccc8"
  )) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2)) +
  labs(x = "", y = "Carbon stock (t C/ha)", fill = "", caption = "Error bars: 90% confidence interval of the total (IPCC approach 1)") +
  theme(legend.position = "bottom")
fig$n_carbon

ggsave("results/figures/05n_carbon.png", fig$n_carbon, width = 18, height = 12, units = "cm", dpi = 300)
