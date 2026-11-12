
## i. Aboveground biomass ####

res_agb_levels <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = lulc_biomass_ag_tot, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater
  ) |>
  left_join(domain_lookup, by = join_by(level, domain))

## + Tables ####
## AGB in t/ha, total AGB in t, uncertainty as margin of error (%) at 90% confidence
tab$i_biomass <- res_agb_levels |>
  transmute(
    level, forest_type, region, n_pp,
    agb_ha = round(Yd / Xd, 1), agb_ha_mep = Rd_mep,
    area_ha = round(Xtot), agb_tot = round(Ytot), agb_tot_mep = Yd_mep
  ) |>
  arrange(level, forest_type, region) |>
  split_by_level()
tab$i_biomass

save_tables(tab$i_biomass, "05i_biomass")

## + Figures ####
fig$i_biomass <- tab$i_biomass$forest_type_region |>
  mutate(agb_ha_me = agb_ha * agb_ha_mep / 100) |>
  ggplot(aes(x = region, y = agb_ha, fill = forest_type)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = pmax(agb_ha - agb_ha_me, 0), ymax = agb_ha + agb_ha_me), width = 0.2) +
  geom_text(aes(y = coalesce(agb_ha + agb_ha_me, agb_ha), label = paste0("n=", n_pp)), vjust = -0.5, size = 2.5) +
  scale_fill_manual(values = defaults$pal_ftype, guide = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~forest_type, nrow = 1, scales = "free_x") +
  labs(x = "", y = "Aboveground biomass (t/ha)", caption = "Error bars: 90% confidence interval, n = number of plots")
fig$i_biomass

ggsave("results/figures/05i_biomass.png", fig$i_biomass, width = 24, height = 10, units = "cm", dpi = 300)
