
## d. Plots with signs of wildlife ####

## Number and share of accessible field plots with at least one wildlife record, by wildlife type and
## by type of sign. Forest type of the plot main LULC section.

## + By wildlife type ####
tmp$wildlife_type <- bind_rows(
  wildlife |> select(cluster_no, plot_no, wildlife_type_label),
  wildlife |> distinct(cluster_no, plot_no) |> mutate(wildlife_type_label = "Any wildlife")
)

res_wildlife <- count_plots_by_level(tmp$wildlife_type, plot_info, wildlife_type_label) |>
  mutate(
    forest_type = factor(forest_type, levels = defaults$ftype_levels),
    region = factor(region, levels = defaults$region_levels)
  ) |>
  arrange(level, forest_type, region, wildlife_type_label)

## + By type of sign ####
tmp$wildlife_sign <- wildlife_sign |>
  filter(!is.na(wildlife_sign_label)) |>
  left_join(wildlife |> select(cluster_no, plot_no, wildlife_id, wildlife_type_label), by = join_by(cluster_no, plot_no, wildlife_id)) |>
  select(cluster_no, plot_no, wildlife_sign_label)

res_wildlife_sign <- count_plots_by_level(tmp$wildlife_sign, plot_info, wildlife_sign_label) |>
  mutate(
    forest_type = factor(forest_type, levels = defaults$ftype_levels),
    region = factor(region, levels = defaults$region_levels)
  ) |>
  arrange(level, forest_type, region, desc(n_plots))

## + Tables ####
tab$d_wildlife      <- split_by_level(res_wildlife)
tab$d_wildlife_sign <- split_by_level(res_wildlife_sign)
tab$d_wildlife$forest_type

save_tables(tab$d_wildlife, "05d_wildlife")
save_tables(tab$d_wildlife_sign, "05d_wildlife_sign")

## + Scientific names and IUCN Red List category of the wildlife ####
## Standardized wildlife names (wildlife_name, 01-get-data.R) are scientific or common names. Common names are
## given their scientific name below (to confirm by DoF), following the IUCN Red List / BirdLife taxonomy.
## Names not identified to species (bees, grasshopper, yellow butterfly) have no scientific name.
## IUCN category from anci$iucn (kingdom ANIMALIA), matched on genus + species epithet: accepted species first,
## then synonyms (e.g. Psittacula krameri, accepted as Alexandrinus krameri). Species not in the Red List:
## 'Not assessed' (Crocodylus suchus, split from the Nile crocodile; Naja guineensis, split from N. melanoleuca;
## Afruca tangeri).
tmp$wildlife_common_sci <- tribble(
  ~wildlife_name,                 ~wildlife_sci_name,
  "African grey hornbill",        "Lophoceros nasutus",
  "Grey-headed woodpecker",       "Dendropicos goertae",     ## African grey woodpecker, the species of The Gambia
  "Guinea Fowl",                  "Numida meleagris",
  "Lesser Blue-eared Starling",   "Lamprotornis chloropterus",
  "Long-tailed Glossy Starling",  "Lamprotornis caudatus",
  "Northern Red Bishop",          "Euplectes franciscanus",
  "Red-billed Hornbill",          "Tockus erythrorhynchus",  ## Western red-billed hornbill, T. kempi, not split in the Red List
  "Snowy-crowned Robin-Chat",     "Cossypha niveicapilla",
  "Spur-winged Plover",           "Vanellus spinosus",
  "Village Weaver",               "Ploceus cucullatus",
  "Western Grey Plantain-Eater",  "Crinifer piscator",
  "Yellow-crowned Gonolek",       "Laniarius barbarus",
  "African Clawless Otter",       "Aonyx capensis",
  "West African Crocodile",       "Crocodylus suchus",
  "West African Fiddler Crab",    "Afruca tangeri"
)

tmp$wildlife_not_species <- c("bees", "grasshopper", "yellow butterfly")

tmp$iucn_animal <- anci$iucn |>
  filter(kingdom == "ANIMALIA") |>
  arrange(iucn_status != "accepted", iucn_rank != "species") |>
  distinct(iucn_name, .keep_all = TRUE) |>
  select(wildlife_sci_name = iucn_name, iucn_accepted_name, wildlife_iucn_code = iucn_code, wildlife_iucn_category = iucn_category)

tmp$wildlife_iucn_levels <- c("CR", "EN", "VU", "NT", "LC", "DD", "Not assessed")

tmp$wildlife_species <- wildlife |>
  distinct(wildlife_type_label, wildlife_name) |>
  filter(wildlife_name != "Unknown", !wildlife_name %in% tmp$wildlife_not_species) |>
  left_join(tmp$wildlife_common_sci, by = join_by(wildlife_name)) |>
  mutate(wildlife_sci_name = coalesce(wildlife_sci_name, wildlife_name)) |>
  left_join(tmp$iucn_animal, by = join_by(wildlife_sci_name)) |>
  mutate(
    iucn_accepted_name = if_else(iucn_accepted_name == wildlife_sci_name, NA, iucn_accepted_name),
    wildlife_iucn_code = replace_na(wildlife_iucn_code, "Not assessed"),
    wildlife_iucn_category = replace_na(wildlife_iucn_category, "Not assessed")
  )

## Check: species names not in the Red List, and names not scientific (common names to add above)
tmp$wildlife_species |> filter(wildlife_iucn_code == "Not assessed")
tmp$wildlife_species |> filter(str_count(wildlife_sci_name, " ") != 1 | str_detect(wildlife_sci_name, "^[a-z]|[A-Z].* [A-Z]"))

## + Frequency of occurrence of the wildlife species ####
## Presence-only records (signs, no counts): the frequency of occurrence replaces the density and dominance
## components of the IVI. For each species: plots with a record (n and % of the accessible field plots) and
## relative frequency (% of the plot records of all identified species). Forest type of the plot main LULC
## section. Records not identified to species are left out.
tmp$wildlife_occ <- wildlife |>
  inner_join(select(tmp$wildlife_species, wildlife_name, wildlife_sci_name), by = join_by(wildlife_name)) |>
  select(cluster_no, plot_no, wildlife_sci_name)

res_wildlife_freq <- count_plots_by_level(tmp$wildlife_occ, plot_info, wildlife_sci_name) |>
  left_join(
    tmp$wildlife_species |> distinct(wildlife_sci_name, wildlife_type_label, wildlife_iucn_code),
    by = join_by(wildlife_sci_name)
  ) |>
  mutate(
    rel_freq = round(n_plots / sum(n_plots) * 100, 1),
    .by = c(level, forest_type, region)
  ) |>
  mutate(
    forest_type = factor(forest_type, levels = defaults$ftype_levels),
    region = factor(region, levels = defaults$region_levels),
    wildlife_iucn_code = factor(wildlife_iucn_code, levels = tmp$wildlife_iucn_levels)
  ) |>
  select(level, forest_type, region, wildlife_sci_name, wildlife_type_label, wildlife_iucn_code, n_plots, n_plots_total, pct_plots, rel_freq) |>
  arrange(level, forest_type, region, desc(n_plots), wildlife_sci_name)

## Threatened and near threatened species (CR, EN, VU, NT)
res_wildlife_iucn <- res_wildlife_freq |>
  filter(wildlife_iucn_code %in% c("CR", "EN", "VU", "NT")) |>
  left_join(distinct(tmp$wildlife_species, wildlife_sci_name, wildlife_iucn_category), by = join_by(wildlife_sci_name)) |>
  select(level, forest_type, region, wildlife_sci_name, wildlife_type_label, wildlife_iucn_code, wildlife_iucn_category, n_plots, n_plots_total, pct_plots) |>
  arrange(level, forest_type, region, wildlife_iucn_code, desc(n_plots))

tab$d_wildlife_freq <- split_by_level(res_wildlife_freq)
tab$d_wildlife_iucn <- split_by_level(res_wildlife_iucn)
tab$d_wildlife_freq$country
tab$d_wildlife_iucn$country

save_tables(tab$d_wildlife_freq, "05d_wildlife_freq")
save_tables(tab$d_wildlife_iucn, "05d_wildlife_iucn")

## + Full list of wildlife recorded ####
## Standardized names, with scientific name and IUCN category of the identified species. Records not
## identified ('Unknown') counted by wildlife type
tab$d_wildlife_list <- wildlife |>
  semi_join(plot_info, by = join_by(cluster_no, plot_no)) |>
  mutate(wildlife_name = if_else(wildlife_name == "Unknown", "Not identified", wildlife_name)) |>
  summarise(n_records = n(), n_plots = n_distinct(cluster_no, plot_no), .by = c(wildlife_type_label, wildlife_name)) |>
  left_join(
    select(tmp$wildlife_species, wildlife_name, wildlife_sci_name, iucn_accepted_name, wildlife_iucn_code),
    by = join_by(wildlife_name)
  ) |>
  ## Recorded name shown only when it differs from the scientific name (common names, not identified)
  mutate(wildlife_name = if_else(!is.na(wildlife_sci_name) & wildlife_name == wildlife_sci_name, NA, wildlife_name)) |>
  select(wildlife_type_label, wildlife_sci_name, wildlife_name, iucn_accepted_name, wildlife_iucn_code, n_records, n_plots) |>
  arrange(wildlife_type_label, is.na(wildlife_sci_name), wildlife_sci_name, wildlife_name)

save_tables(list(all = tab$d_wildlife_list), "05d_wildlife_list")

## + Figures ####
fig$d_wildlife <- tab$d_wildlife$forest_type |>
  ggplot(aes(x = wildlife_type_label, y = pct_plots, fill = forest_type)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.8) +
  scale_fill_manual(values = defaults$pal_ftype) +
  labs(x = "", y = "Plots with signs (%)", fill = "") +
  theme(legend.position = "bottom")
fig$d_wildlife

fig$d_wildlife_region <- tab$d_wildlife$forest_type_region |>
  filter(wildlife_type_label != "Any wildlife") |>
  ggplot(aes(x = region, y = wildlife_type_label, fill = pct_plots)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(n_plots, "/", n_plots_total)), size = 2.5) +
  scale_fill_gradientn(colors = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab")) +
  facet_wrap(~forest_type) +
  labs(x = "", y = "", fill = "Plots (%)", caption = "Labels: plots with signs / accessible plots") +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
fig$d_wildlife_region

ggsave("results/figures/05d_wildlife.png", fig$d_wildlife, width = 18, height = 11, units = "cm", dpi = 300)
ggsave("results/figures/05d_wildlife_region.png", fig$d_wildlife_region, width = 22, height = 14, units = "cm", dpi = 300)
