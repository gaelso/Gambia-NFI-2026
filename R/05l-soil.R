## l. Soil condition and characteristics ####

## Visual soil assessment at the measurement point of each plot: soil surface condition, organic layer depth,
## topsoil (0-5 cm) and subsoil (5-30 cm) texture, and topsoil colour.
## - Share of area by class (%): two-phase estimates (nfi_aggregate2 sub-domains) of the share of the domain
##   area in each class. The plot class applies to all the LULC sections of the plot (plot_to_ssu()).
## - Only the field plots with an observation of the attribute are used: plots without observation are
##   treated as inaccessible plots, and the virtual plots of the non-forest stratum are not used (no soil
##   observation). The country level is the national forest, as for the other results.

tmp$soil_attr <- tribble(
  ~soil_attr_col,              ~soil_attribute,          ~soil_classes,
  "soil_surf_condition_label", "Surface condition",      list(c("Poor", "Moderate", "Good")),
  "soil_depth_label",          "Organic layer depth",    list(c("< 1 cm", "1 to 5 cm", "> 5 cm")),
  "soil_top_tex_label",        "Topsoil texture",        list(c("Sand", "Sandy loam / loamy sand", "Loam", "Clay loam", "Clay", "Rock/Stone")),
  "soil_sub_tex_label",        "Subsoil texture",        list(c("Sand", "Sandy loam / loamy sand", "Loam", "Clay loam", "Clay", "Rock/Stone")),
  "soil_color_label",          "Topsoil colour",         list(c("Pale brown/greyish", "Brown/light brown", "Dark/very dark brown"))
) |>
  mutate(soil_classes = map(soil_classes, 1))

## One row per field plot and attribute, colour labels without the condition interpretation
tmp$soil <- plot |>
  select(cluster_no, plot_no, all_of(tmp$soil_attr$soil_attr_col)) |>
  pivot_longer(-c(cluster_no, plot_no), names_to = "soil_attr_col", values_to = "soil_class") |>
  filter(!is.na(soil_class)) |>
  mutate(
    soil_class = str_remove(soil_class, "-.*condition$") |> str_squish(),
    soil_class = recode(soil_class, "Dark color/ very dark brown" = "Dark/very dark brown"),
    presence = 1
  ) |>
  left_join(select(tmp$soil_attr, soil_attr_col, soil_attribute), by = join_by(soil_attr_col))

## Check: all classes in the class list
tmp$soil |>
  anti_join(unnest(tmp$soil_attr, soil_classes), by = join_by(soil_attr_col, soil_class == soil_classes)) |>
  count(soil_attribute, soil_class)

## + Share of area by class ####
res_soil <- map(tmp$soil_attr$soil_attribute, \(attr_name) {

  soil_attr <- tmp$soil |> filter(soil_attribute == attr_name)

  ## Phase 2 base units: field plots with an observation of the attribute
  ph2_soil <- ph2_data |>
    left_join(distinct(soil_attr, cluster_no, plot_no, obs = TRUE), by = join_by(cluster_no, plot_no)) |>
    mutate(access = access & !virtual & replace_na(obs, FALSE))

  nfi_aggregate_levels(
    .levels = vec_levels,
    .ph1_df = ph1_data, .ph2_sp = ph2_soil, .attr_y = soil_area, .attr_x = ssu_area,
    .aoi_area = defaults$country_area_nowater,
    .ph2_sub = plot_to_ssu(soil_attr, ph2_soil |> filter(access), soil_class, presence, .name = "soil_area"),
    .class_sub = soil_class
    ) |>
    mutate(soil_attribute = attr_name)
}) |>
  list_rbind() |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  mutate(share = Yd / Xd)

## Check: shares add up to 100% in each domain with observations
res_soil |>
  summarise(share = sum(share), .by = c(level, domain, soil_attribute)) |>
  filter(abs(share - 1) > 1e-6, share > 0)

## + Tables ####
tmp$soil_class_levels <- unique(unlist(tmp$soil_attr$soil_classes))

tab$l_soil <- res_soil |>
  filter(Xd > 0) |>
  mutate(
    soil_attribute = factor(soil_attribute, levels = tmp$soil_attr$soil_attribute),
    soil_class = factor(soil_class, levels = tmp$soil_class_levels)
  ) |>
  transmute(level, forest_type, region, n_pp, soil_attribute, soil_class, soil_pct = round(share * 100, 1), soil_mep = Rd_mep) |>
  arrange(level, forest_type, region, soil_attribute, soil_class) |>
  split_by_level()
tab$l_soil$country

save_tables(tab$l_soil, "05l_soil")

## + Figures ####
tmp$pal_soil <- c(
  "Poor" = "#d7301f", "Moderate" = "#fdcc8a", "Good" = "#31a354",
  "< 1 cm" = "#f6e8c3", "1 to 5 cm" = "#bf812d", "> 5 cm" = "#543005",
  "Sand" = "#fee391", "Sandy loam / loamy sand" = "#fec44f", "Loam" = "#d95f0e", "Clay loam" = "#993404",
  "Clay" = "#662506", "Rock/Stone" = "#969696",
  "Pale brown/greyish" = "#d9c8b4", "Brown/light brown" = "#a0785a", "Dark/very dark brown" = "#4e342e"
)

fig$l_soil <- bind_rows(tab$l_soil$country, tab$l_soil$forest_type) |>
  filter(forest_type != "Non-forest") |>
  mutate(forest_type = fct_recode(forest_type, "All forest" = "All")) |>
  ggplot(aes(x = forest_type, y = soil_pct, fill = soil_class)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = tmp$pal_soil) +
  facet_wrap(~soil_attribute, ncol = 3) +
  guides(fill = guide_legend(ncol = 3)) +
  labs(x = "", y = "Share of area (%)", fill = "", caption = "All forest: country level, all forest types together") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1), legend.text = element_text(size = 7))
fig$l_soil

ggsave("results/figures/05l_soil.png", fig$l_soil, width = 24, height = 16, units = "cm", dpi = 300)
