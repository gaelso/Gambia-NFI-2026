## j. Forest regeneration ####

## Seedlings (height < 1.3 m) and saplings (height >= 1.3 m and DBH < 10 cm) of tree species, counted by species
## in the circular regeneration subplot of each plot (radius 3.99 m, about 50 m2).
## - Density (individuals/ha): two-phase estimates (nfi_aggregate2 sub-domains). The subplot density applies to
##   all the LULC sections of the plot (plot_to_ssu()). Accessible plots without record have no regeneration.
## - Species: density by species (two-phase estimates) and occurrence (number of clusters and accessible
##   field plots with the species, count_plots_by_level()). Richness: identified species recorded in the domain.

defaults$regen_radius <- 3.99
defaults$regen_factor <- 10000 / (pi * defaults$regen_radius^2)

tmp$regen <- regen |>
  select(cluster_no, plot_no, species = regen_species_scientific_name, Seedlings = seedling_count, Saplings = sapling_count) |>
  pivot_longer(c(Seedlings, Saplings), names_to = "regen_class", values_to = "regen_n") |>
  filter(regen_n > 0) |>
  mutate(regen_ha = regen_n * defaults$regen_factor)

## Check: no regeneration record outside the accessible field plots
tmp$regen |> anti_join(plot_info, by = join_by(cluster_no, plot_no)) |> nrow()

## + Density of seedlings and saplings ####
tmp$regen_class <- bind_rows(
  tmp$regen |> summarise(regen_ha = sum(regen_ha), .by = c(cluster_no, plot_no, regen_class)),
  tmp$regen |> summarise(regen_ha = sum(regen_ha), .by = c(cluster_no, plot_no)) |> mutate(regen_class = "Total")
)

res_regen <- nfi_aggregate_levels(
  .levels = vec_levels,
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = regen_count, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = plot_to_ssu(tmp$regen_class, ph2_data, regen_class, regen_ha, .name = "regen_count"),
  .class_sub = regen_class
  ) |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  mutate(density = Yd / Xd)

## Check: seedlings + saplings = total
res_regen |>
  summarise(diff = sum(density[regen_class != "Total"]) - density[regen_class == "Total"], .by = c(level, domain)) |>
  pull(diff) |> abs() |> max()

## + Species richness ####
## Identified species with at least one seedling or sapling in the accessible field plots of the domain
tmp$regen_rich <- tmp$regen |>
  filter(!species %in% defaults$species_unidentified) |>
  bind_rows(tmp$regen |> filter(!species %in% defaults$species_unidentified) |> mutate(regen_class = "Total")) |>
  mutate(class_species = paste(regen_class, species, sep = "|"))

res_regen_rich <- count_plots_by_level(tmp$regen_rich, plot_info, class_species) |>
  separate_wider_delim(class_species, delim = "|", names = c("regen_class", "species")) |>
  summarise(richness = n_distinct(species), .by = c(level, forest_type, region, regen_class))

## + Density by species ####
## Country and forest type levels only
tmp$regen_species <- tmp$regen |>
  summarise(regen_ha = sum(regen_ha), .by = c(cluster_no, plot_no, species))

res_regen_species <- nfi_aggregate_levels(
  .levels = vec_levels[c("country", "forest_type")],
  .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = regen_count, .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater,
  .ph2_sub = plot_to_ssu(tmp$regen_species, ph2_data, species, regen_ha, .name = "regen_count"),
  .class_sub = species
  ) |>
  left_join(domain_lookup, by = join_by(level, domain)) |>
  mutate(density = Yd / Xd)

res_regen_species_freq <- count_plots_by_level(tmp$regen_species, plot_info, species) |>
  filter(level != "forest_type_region")

## + Tables ####
## Density (individuals/ha) with margin of error and richness, by regeneration class
tab$j_regen <- res_regen |>
  left_join(res_regen_rich, by = join_by(level, forest_type, region, regen_class)) |>
  mutate(
    regen_class = factor(regen_class, levels = c("Seedlings", "Saplings", "Total")),
    richness = replace_na(richness, 0L)
  ) |>
  transmute(level, forest_type, region, n_pp, regen_class, regen_ha = round(density), regen_mep = Rd_mep, richness) |>
  mutate(forest_type = factor(forest_type, levels = defaults$ftype_levels), region = factor(region, levels = defaults$region_levels)) |>
  arrange(level, forest_type, region, regen_class) |>
  split_by_level()
tab$j_regen$forest_type

## Top 10 identified species by density (seedlings + saplings), country and forest type levels
## Relative density: share of the total regeneration density of the domain (unidentified species included)
tab$j_regen_species <- res_regen_species |>
  mutate(rel_density = round(density / sum(density) * 100, 1), .by = c(level, domain)) |>
  filter(!species %in% defaults$species_unidentified, density > 0) |>
  left_join(res_regen_species_freq, by = join_by(level, forest_type, region, species)) |>
  slice_max(density, n = 10, with_ties = FALSE, by = c(level, domain)) |>
  filter(forest_type != "Non-forest") |>
  transmute(level, forest_type, region, species, regen_ha = round(density), regen_mep = Rd_mep, rel_density, n_clusters, n_plots) |>
  mutate(forest_type = factor(forest_type, levels = defaults$ftype_levels), region = factor(region, levels = defaults$region_levels)) |>
  arrange(level, forest_type, desc(regen_ha)) |>
  split_by_level()
tab$j_regen_species$country

save_tables(tab$j_regen, "05j_regen")
save_tables(tab$j_regen_species, "05j_regen_species")

## + Figures ####
fig$j_regen <- bind_rows(tab$j_regen$forest_type, tab$j_regen$forest_type_region) |>
  filter(forest_type != "Non-forest", regen_class != "Total") |>
  mutate(regen_me = regen_ha * replace_na(regen_mep, 0) / 100) |>
  ggplot(aes(x = region, y = regen_ha, fill = regen_class)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_errorbar(
    aes(ymin = pmax(regen_ha - regen_me, 0), ymax = regen_ha + regen_me),
    position = position_dodge(width = 0.85), width = 0.2
  ) +
  scale_fill_manual(values = c(Seedlings = "#a5d6a7", Saplings = "#2e7d32")) +
  facet_wrap(~forest_type, scales = "free_x") +
  labs(x = "", y = "Density (individuals/ha)", fill = "", caption = "All: forest type level estimate. Error bars: 90% confidence interval") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
fig$j_regen

ggsave("results/figures/05j_regen.png", fig$j_regen, width = 24, height = 11, units = "cm", dpi = 300)
