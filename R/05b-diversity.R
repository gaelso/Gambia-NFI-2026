
## b. Importance Value Index (IVI) and Shannon index ####

## IVI = relative density + relative frequency + relative dominance (0-300), by species, genus and family.
## - density: trees/ha by taxon, two-phase estimate (nfi_aggregate2 sub-domains)
## - dominance: basal area (m2/ha) by taxon, two-phase estimate
## - frequency: number of plot LULC sections in the domain where the taxon is recorded
## Shannon index of the domain from the taxon densities, with linearized variance (nfi_aggregate2 .index = "shannon").
## Unidentified trees (defaults$species_unidentified) are removed.

tmp$tree_div <- tree |> filter(!is.na(tree_species_name), !tree_species_name %in% defaults$species_unidentified)

## Domain columns of each LULC section, for the frequency
tmp$ssu_domains <- ph2_data |>
  select(cluster_no, plot_no, tree_lulc_no = lulc_id, all_of(unname(vec_levels))) |>
  pivot_longer(all_of(unname(vec_levels)), names_to = "dom_col", values_to = "domain") |>
  mutate(level = names(vec_levels)[match(dom_col, vec_levels)]) |>
  select(-dom_col)

## + IVI and Shannon by taxon level ####
res_div <- map(c(species = "tree_species_name", genus = "tree_genus", family = "tree_family"), \(col) {

  ## Density and Shannon index
  res_dens <- nfi_aggregate_levels(
    .levels = vec_levels,
    .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = tree_count, .attr_x = ssu_area,
    .aoi_area = defaults$country_area_nowater,
    .ph2_sub = tree_to_ssu(tmp$tree_div, !!sym(col), tree_weight, defaults$plot_size_dbh20, .name = "tree_count"),
    .class_sub = !!sym(col),
    .index = "shannon",
    .out = c("totals_d", "shannon_d")
  )

  ## Dominance
  res_ba <- nfi_aggregate_levels(
    .levels = vec_levels,
    .ph1_df = ph1_data, .ph2_sp = ph2_data, .attr_y = basal_area, .attr_x = ssu_area,
    .aoi_area = defaults$country_area_nowater,
    .ph2_sub = tree_to_ssu(tmp$tree_div, !!sym(col), tree_basal_area * tree_weight, defaults$plot_size_dbh20, .name = "basal_area"),
    .class_sub = !!sym(col)
  )

  ## Frequency
  freq <- tmp$tree_div |>
    distinct(cluster_no, plot_no, tree_lulc_no, taxon = !!sym(col)) |>
    inner_join(tmp$ssu_domains, by = join_by(cluster_no, plot_no, tree_lulc_no), relationship = "many-to-many") |>
    count(level, domain, taxon, name = "n_sections")

  ivi <- res_dens$totals_d |>
    select(level, domain, taxon = all_of(col), n_pp, Yd, Xd, density_mep = Rd_mep) |>
    mutate(density = Yd / Xd) |>
    left_join(
      res_ba |> mutate(basal_area = Yd / Xd) |> select(level, domain, taxon = all_of(col), basal_area),
      by = join_by(level, domain, taxon)
    ) |>
    left_join(freq, by = join_by(level, domain, taxon)) |>
    filter(density > 0) |>
    mutate(
      n_sections    = replace_na(n_sections, 0),
      rel_density   = density / sum(density) * 100,
      rel_frequency = n_sections / sum(n_sections) * 100,
      rel_dominance = basal_area / sum(basal_area) * 100,
      ivi           = rel_density + rel_frequency + rel_dominance,
      .by = c(level, domain)
    ) |>
    left_join(domain_lookup, by = join_by(level, domain)) |>
    select(level, forest_type, region, n_pp, taxon, density, density_mep, basal_area, n_sections, starts_with("rel_"), ivi) |>
    mutate(across(c(density, basal_area, starts_with("rel_"), ivi), \(x) round(x, 2))) |>
    arrange(level, forest_type, region, desc(ivi))

  shannon <- res_dens$shannon_d |>
    left_join(domain_lookup, by = join_by(level, domain)) |>
    select(level, forest_type, region, n_pp, H, H_me, H_mep, richness, evenness) |>
    arrange(level, forest_type, region)

  list(ivi = ivi, shannon = shannon)
})

## + Tables ####
## Top 20 species, top 10 genera and families by IVI in each domain
tmp$top_ivi <- \(.df, .n, .name) .df |>
  slice_max(ivi, n = .n, by = c(level, forest_type, region), with_ties = FALSE) |>
  rename(!!.name := taxon) |>
  split_by_level()

tab$b_ivi_species <- tmp$top_ivi(res_div$species$ivi, 20, "species")
tab$b_ivi_genus   <- tmp$top_ivi(res_div$genus$ivi, 10, "genus")
tab$b_ivi_family  <- tmp$top_ivi(res_div$family$ivi, 10, "family")

tab$b_shannon <- imap(res_div, \(x, tx) x$shannon |> mutate(taxon_level = tx, .after = n_pp)) |>
  list_rbind() |>
  mutate(across(c(H, H_me, evenness), \(x) round(x, 3))) |>
  split_by_level()

tab$b_ivi_species$country
tab$b_shannon

save_tables(tab$b_ivi_species, "05b_ivi_species")
save_tables(tab$b_ivi_genus, "05b_ivi_genus")
save_tables(tab$b_ivi_family, "05b_ivi_family")
save_tables(tab$b_shannon, "05b_shannon")

## + Figures ####
tmp$ivi_long <- \(.df, .taxon) .df |>
  pivot_longer(c(rel_density, rel_frequency, rel_dominance), names_to = "component", values_to = "value") |>
  mutate(
    component = factor(component, levels = c("rel_dominance", "rel_frequency", "rel_density"), labels = c("Relative dominance", "Relative frequency", "Relative density")),
    ## Order taxa within each forest type
    taxon_ord = paste(.data[[.taxon]], forest_type, sep = "___"),
    taxon_ord = fct_reorder(taxon_ord, ivi)
  )

fig$b_ivi_species <- tab$b_ivi_species$country |>
  tmp$ivi_long("species") |>
  ggplot(aes(x = value, y = taxon_ord, fill = component)) +
  geom_col() +
  scale_y_discrete(labels = \(x) str_remove(x, "___.*")) +
  scale_fill_manual(values = c("#9ecae1", "#4292c6", "#08519c"), breaks = c("Relative density", "Relative frequency", "Relative dominance")) +
  labs(x = "Importance Value Index", y = "", fill = "") +
  theme(legend.position = "bottom", axis.text.y = element_text(face = "italic"))
fig$b_ivi_species

fig$b_ivi_species_ftype <- tab$b_ivi_species$forest_type |>
  filter(forest_type != "Non-forest") |>
  slice_max(ivi, n = 10, by = forest_type) |>
  tmp$ivi_long("species") |>
  ggplot(aes(x = value, y = taxon_ord, fill = component)) +
  geom_col() +
  scale_y_discrete(labels = \(x) str_remove(x, "___.*")) +
  scale_fill_manual(values = c("#9ecae1", "#4292c6", "#08519c"), breaks = c("Relative density", "Relative frequency", "Relative dominance")) +
  facet_wrap(~forest_type, scales = "free_y", ncol = 1) +
  labs(x = "Importance Value Index", y = "", fill = "", subtitle = "Top 10 species per forest type") +
  theme(legend.position = "bottom", axis.text.y = element_text(face = "italic"))
fig$b_ivi_species_ftype

fig$b_shannon <- tab$b_shannon$forest_type_region |>
  filter(taxon_level == "species") |>
  ggplot(aes(x = region, y = H, color = forest_type)) +
  geom_pointrange(aes(ymin = H - H_me, ymax = H + H_me), position = position_dodge(width = 0.6)) +
  scale_color_manual(values = defaults$pal_ftype) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y = "Shannon index (H')", color = "", caption = "Species level, error bars: 90% confidence interval") +
  theme(legend.position = "bottom")
fig$b_shannon

ggsave("results/figures/05b_ivi_species.png", fig$b_ivi_species, width = 18, height = 16, units = "cm", dpi = 300)
ggsave("results/figures/05b_ivi_species_ftype.png", fig$b_ivi_species_ftype, width = 18, height = 24, units = "cm", dpi = 300)
ggsave("results/figures/05b_shannon.png", fig$b_shannon, width = 18, height = 11, units = "cm", dpi = 300)
