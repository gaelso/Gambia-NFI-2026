
## c. Species in the IUCN Red List threatened categories ####

## IUCN category from GBIF (see 05-double-sampling.R). Threatened = CR (Critically Endangered),
## EN (Endangered) and VU (Vulnerable). Occurrence = number of accessible field plots with at least one
## live tree of the species, forest type of the plot main LULC section.

tmp$iucn_threatened <- c("CR", "EN", "VU")

## + All species with their category, for reference ####
tab$c_species_iucn <- tree |>
  filter(!is.na(tree_species_name), !tree_species_name %in% defaults$species_unidentified) |>
  summarise(
    n_trees = n(),
    n_plots = n_distinct(cluster_no, plot_no),
    .by = c(tree_family, tree_species_name, tree_iucn_code, tree_iucn_category)
  ) |>
  arrange(tree_family, tree_species_name)

table(tab$c_species_iucn$tree_iucn_code, useNA = "ifany")

## + Threatened species occurrences ####
tmp$tree_iucn <- tree |> filter(tree_iucn_code %in% tmp$iucn_threatened)

res_iucn <- tmp$tree_iucn |>
  count_plots_by_level(.plot_info = plot_info, .cat = tree_species_name) |>
  left_join(
    tab$c_species_iucn |> select(tree_species_name, tree_family, tree_iucn_code, tree_iucn_category),
    by = join_by(tree_species_name)
  ) |>
  mutate(
    forest_type = factor(forest_type, levels = defaults$ftype_levels),
    region = factor(region, levels = defaults$region_levels),
    tree_iucn_code = factor(tree_iucn_code, levels = tmp$iucn_threatened)
  ) |>
  select(level, forest_type, region, tree_species_name, tree_family, tree_iucn_code, tree_iucn_category, n_plots, n_plots_total, pct_plots) |>
  arrange(level, forest_type, region, tree_iucn_code, desc(n_plots))

## + Tables ####
tab$c_iucn <- split_by_level(res_iucn)
tab$c_iucn$country

save_tables(tab$c_iucn, "05c_iucn")
write_csv(tab$c_species_iucn, "results/tables/05c_species_iucn_all.csv")

## + Figures ####
fig$c_iucn <- tab$c_iucn$forest_type |>
  mutate(tree_species_name = fct_reorder(tree_species_name, n_plots, .fun = sum)) |>
  ggplot(aes(x = n_plots, y = tree_species_name, fill = forest_type)) +
  geom_col() +
  scale_fill_manual(values = defaults$pal_ftype) +
  facet_grid(tree_iucn_code ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Number of plots", y = "", fill = "", caption = "IUCN category: CR Critically Endangered, EN Endangered, VU Vulnerable") +
  theme(legend.position = "bottom", axis.text.y = element_text(face = "italic"))
fig$c_iucn

ggsave("results/figures/05c_iucn.png", fig$c_iucn, width = 18, height = 12, units = "cm", dpi = 300)
