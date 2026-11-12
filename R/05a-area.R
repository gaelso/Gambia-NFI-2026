
## a. Area of forest types ####
## Domain area = country area x domain share (Step 8 of the methods)

## + Country: forest, non-forest and total ####
res_area_forest <- ph2_data |>
  mutate(dom_forest = if_else(forest_type == "Non-forest", "Non-forest", "Forest")) |>
  nfi_aggregate_levels(
    .levels = c(country = "dom_forest"),
    .ph1_df = ph1_data, .ph2_sp = _, .attr_y = ssu_area, .attr_x = ssu_area,
    .aoi_area = defaults$country_area_nowater
  )

## + Forest type and forest type x region ####
res_area <- nfi_aggregate_levels(
  .levels = vec_levels[c("forest_type", "forest_type_region")],
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = ssu_area, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater
  ) |>
  left_join(domain_lookup, by = join_by(level, domain))

## + Tables ####
tab$a_area <- list(
  country = res_area_forest |>
    transmute(land_cover = domain, n_pp, area_ha = round(Xtot), area_mep = Xd_mep, area_pct = round(Xd * 100, 1)) |>
    bind_rows(tibble(land_cover = "Total", area_ha = defaults$country_area_nowater, area_pct = 100)),
  forest_type = res_area |>
    filter(level == "forest_type") |>
    transmute(forest_type, n_pp, area_ha = round(Xtot), area_mep = Xd_mep, area_pct = round(Xd * 100, 1)) |>
    arrange(forest_type),
  forest_type_region = res_area |>
    filter(level == "forest_type_region") |>
    transmute(forest_type, region, n_pp, area_ha = round(Xtot), area_mep = Xd_mep, area_pct = round(Xd * 100, 1)) |>
    arrange(forest_type, region)
)
tab$a_area

save_tables(tab$a_area, "05a_area")

## + Figures ####
fig$a_area <- tab$a_area$forest_type_region |>
  filter(forest_type != "Non-forest") |>
  mutate(area_me = area_ha * area_mep / 100) |>
  ggplot(aes(x = region, y = area_ha / 1000, fill = forest_type)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.8) +
  geom_errorbar(
    aes(ymin = pmax(area_ha - area_me, 0) / 1000, ymax = (area_ha + area_me) / 1000),
    position = position_dodge(width = 0.8, preserve = "single"), width = 0.2
  ) +
  scale_fill_manual(values = defaults$pal_ftype) +
  labs(x = "", y = "Area (1,000 ha)", fill = "", caption = "Error bars: 90% confidence interval") +
  theme(legend.position = "bottom")
fig$a_area

ggsave("results/figures/05a_area.png", fig$a_area, width = 18, height = 11, units = "cm", dpi = 300)
