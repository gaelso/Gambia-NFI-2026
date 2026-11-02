
## Clean NFI data prepared by Iveren
## Repo: https://github.com/iverenc/GAMBIA-NFA-2025, git submodule in 'external/iveren-checks'
##
## Code copied from 'external/iveren-checks/scripts/' keeping only the steps that build the final
## clean tables. Data inspection, QA/QC tables, figures and exports are left out.
## Section numbers and titles match the source scripts.
##
## Conventions:
## - raw tables in 'data_raw', reference tables in 'anci', temporary tables for joins and binds in 'tmp'
## - each table is modified under its own name in the environment (cluster, plot, tree, ...)
## - corrected columns keep their raw name, the original values are saved first as '<colname>_old'
##
## Output: list 'data_clean' with tables cluster, plot, plot_lulc, ntfp, ntfp_type, wildlife,
## wildlife_sign, tree, regen and deadwood.

dir_data <- "external/iveren-checks/data"

data_raw   <- list()
data_clean <- list()
anci       <- list()
tmp        <- list()



##
## Read raw data from OF Arena DB, manually stored as CSV ######
##

data_raw <- list(
  cluster       = read_csv(file.path(dir_data,"raw/01_cluster.csv"), show_col_types = FALSE),
  cluster_rp    = read_csv(file.path(dir_data,"raw/02_cluster_rp.csv"), show_col_types = FALSE),
  cluster_team  = read_csv(file.path(dir_data,"raw/03_cluster_team.csv"), show_col_types = FALSE),
  plot          = read_csv(file.path(dir_data,"raw/04_plot.csv"), show_col_types = FALSE),
  rp_marker     = read_csv(file.path(dir_data,"raw/05_rp_marker.csv"), show_col_types = FALSE),
  plot_lulc     = read_csv(file.path(dir_data,"raw/06_plot_lulc.csv"), show_col_types = FALSE),
  ntfp          = read_csv(file.path(dir_data,"raw/07_ntfp.csv"), show_col_types = FALSE),
  wildlife      = read_csv(file.path(dir_data,"raw/08_wildlife.csv"), show_col_types = FALSE),
  regen         = read_csv(file.path(dir_data,"raw/09_regeneration.csv"), show_col_types = FALSE),
  deadwood      = read_csv(file.path(dir_data,"raw/10_deadwood.csv"), show_col_types = FALSE),
  tree          = read_csv(file.path(dir_data,"raw/11_tree.csv"), show_col_types = FALSE),
  ntfp_type     = read_csv(file.path(dir_data,"raw/12_ntfp_type.csv"), show_col_types = FALSE),
  wildlife_sign = read_csv(file.path(dir_data,"raw/13_wildlife_sign.csv"), show_col_types = FALSE),
  tree_causes   = read_csv(file.path(dir_data,"raw/14_tree_condition_causes.csv"), show_col_types = FALSE)
)

anci$sampling_points   <- read_excel(file.path(dir_data, "reference/sampling_point_data.xlsx"))
anci$ntfp_name_map     <- read_excel(file.path(dir_data, "reference/NTFP_Name_Map.xlsx"), sheet = "NTFP_Name_Map")
anci$wildlife_name_map <- read_excel(file.path(dir_data, "reference/Wildlife_Name_Map.xlsx"), sheet = "Wildlife_Name_Map")
anci$tree_species_map  <- read_excel(file.path(dir_data, "reference/Tree_Species_DoF_Name_Map.xlsx"), sheet = "Species_Name_Map")



##
## Prepare inital tables ######
##

## + Cluster info ######

tmp$cluster_stratum_lookup <- anci$sampling_points %>%
  filter(is.na(level2_code)) %>%
  transmute(
    cluster_no = as.numeric(label_en),
    cluster_design_stratum = forest_type
  ) %>%
  arrange(cluster_no)

cluster <- data_raw$cluster %>%
  left_join(
    tmp$cluster_stratum_lookup,
    by = "cluster_no",
    relationship = "one-to-one"
  ) |>
  arrange(cluster_no) |>
  rowwise() |>
  mutate(flag_stratum = if_else(cluster_info_stratum == cluster_design_stratum, 0, 1)) |>
  rename(
    cluster_info_stratum_old = cluster_info_stratum,
    cluster_info_stratum = cluster_design_stratum
  )

table(cluster$flag_stratum, useNA = "ifany")
cluster |> select(cluster_no, cluster_info_stratum_old, cluster_info_stratum)

data_clean$cluster <- cluster


## + Plot ######

## Disturbance sign recorded as FALSE while an intensity is recorded
plot <- data_raw$plot %>%
  mutate(
    plot_grazing_sign_old = plot_grazing_sign,
    plot_grazing_sign = case_when(
      plot_grazing_sign_old %in% FALSE & !is.na(plot_grazing_intensity) ~ TRUE,
      TRUE ~ plot_grazing_sign_old
    ),

    plot_fire_sign_old = plot_fire_sign,
    plot_fire_sign = case_when(
      plot_fire_sign_old %in% FALSE & !is.na(plot_fire_intensity) ~ TRUE,
      TRUE ~ plot_fire_sign_old
    ),

    plot_branch_removal_sign_old = plot_branch_removal_sign,
    plot_branch_removal_sign = case_when(
      plot_branch_removal_sign_old %in% FALSE & !is.na(plot_branch_removal_intensity) ~ TRUE,
      TRUE ~ plot_branch_removal_sign_old
    ),

    plot_tree_felling_sign_old = plot_tree_felling_sign,
    plot_tree_felling_sign = case_when(
      plot_tree_felling_sign_old %in% FALSE & !is.na(plot_tree_felling_intensity) ~ TRUE,
      TRUE ~ plot_tree_felling_sign_old
    )
  )

## Other tree felling type only kept when type is 99 "Others"
plot <- plot %>%
  mutate(
    plot_tree_felling_other_old = plot_tree_felling_other,
    plot_tree_felling_other = case_when(
      plot_tree_felling_type == 99 &
        !is.na(plot_tree_felling_other_old) &
        str_squish(plot_tree_felling_other_old) != "" ~
        str_squish(plot_tree_felling_other_old),

      TRUE ~ NA_character_
    )
  )

## Temporary variable for text matching, removed at the end
plot <- plot %>%
  mutate(
    other_disturbance_text = str_to_lower(str_squish(plot_other_disturbances))
  )

## Fire and tree felling signs recovered from the other disturbances text
plot <- plot %>%
  mutate(
    plot_fire_sign = case_when(
      plot_access %in% c(0, 1) &
        plot_fire_sign %in% FALSE &
        str_detect(other_disturbance_text, "\\bburned\\b|\\bburnt\\b") ~ TRUE,

      TRUE ~ plot_fire_sign
    ),

    plot_tree_felling_sign = case_when(
      plot_access %in% c(0, 1) &
        plot_tree_felling_sign %in% FALSE &
        str_detect(
          other_disturbance_text,
          paste(
            "clear felling",
            "clear cutting",
            "only felling",
            "firewood",
            "cutting etc",
            sep = "|"
          )
        ) ~ TRUE,

      TRUE ~ plot_tree_felling_sign
    )
  )

## Other disturbances standardized to categories
plot <- plot %>%
  mutate(
    plot_other_disturbances_old = plot_other_disturbances,
    plot_other_disturbances = case_when(
      # Termites
      str_detect(other_disturbance_text, "termite") ~ "Termite infestation",

      # Other insects
      str_detect(other_disturbance_text, "insect") ~ "Insect infestation",

      # Charcoal activities
      str_detect(other_disturbance_text, "charcoal") ~ "Charcoal production",

      # Human disturbance
      str_detect(other_disturbance_text, "human intrusion|human activity is high") ~
        "Human disturbance or intrusion",

      # General resource exploitation
      str_detect(other_disturbance_text, "too much of exploitation") ~ "Resource extraction",

      # Water pollution is checked before general dumping
      str_detect(other_disturbance_text, "river pollution|sea waves|littering from river") ~
        "Water pollution",

      # Waste dumping
      str_detect(other_disturbance_text, "\\bdumping\\b") ~ "Waste dumping",

      # Invasive species
      str_detect(other_disturbance_text, "invasive species") ~ "Invasive species",

      # Livestock trampling
      str_detect(other_disturbance_text, "trampling") ~ "Livestock trampling",

      # Everything else is not retained as a confirmed other disturbance
      TRUE ~ NA_character_
    )
  )

## 20. SOIL AND LITTER COMPLETENESS

## Litter presence recorded as FALSE while litter depth and composition are recorded
plot <- plot %>%
  mutate(
    litter_presence_old = litter_presence,
    litter_presence = case_when(
      litter_presence_old %in% FALSE &
        !is.na(litter_depth) &
        !is.na(litter_composition) ~ TRUE,

      TRUE ~ litter_presence_old
    )
  )

## 22. Remove temporary variable

plot <- plot %>%
  select(-other_disturbance_text)

data_clean$plot <- plot


## + Plot LULC ######

## Source: scripts/01_cluster_plot_lulc_data_prep.R
## Recovered records (21.7 and 21.11) are added before cleaning the forest subtypes (21.6),
## so that the cleaning applies to all records at once.

## 21.7 RECOVER CLUSTER 72, PLOT 5 LULC RECORD

tmp$cluster72_plot5_lulc <- tibble(
  cluster_no = 72,
  cluster_no_label = 72,
  plot_no = 5,
  plot_no_label = 5,
  lulc_id = 1,
  lulc_name = 4,
  lulc_name_label = "Wetlands",
  lulc_forest = NA_real_,
  lulc_forest_label = NA_character_,
  lulc_percentage = 100,
  lulc_note = paste(
    "Recovered from plot note:",
    "The plot comprise 100% Wetland"
  )
)

tmp$cluster72_plot5_lulc_to_add <- tmp$cluster72_plot5_lulc %>%
  anti_join(
    data_raw$plot_lulc %>% select(cluster_no, plot_no, lulc_id),
    by = c("cluster_no", "plot_no", "lulc_id")
  )

plot_lulc <- data_raw$plot_lulc %>%
  bind_rows(tmp$cluster72_plot5_lulc_to_add) %>%
  arrange(cluster_no, plot_no, lulc_id)

## 21.11 RECOVER OMITTED LULC SECTIONS SUPPORTED BY NOTES

tmp$lulc_percentage_recoveries <- tribble(
  ~cluster_no, ~plot_no, ~lulc_id, ~lulc_name,
  ~lulc_name_label, ~lulc_forest, ~lulc_forest_label,
  ~lulc_percentage, ~recovery_evidence,

  14, 1, 2, 5, "Settlements",  NA, NA, 30,
  "LULC note states that the remaining 30% is residential area.",

  45, 4, 2, 1, "Forest Land", 3,  "Open Forest",   20,
  "LULC note states: 20% open forest.",

  68, 3, 2, 1, "Forest Land", 2,  "Close Forest",  20,
  "LULC note states: 20% close forest.",

  91, 1, 2, 1, "Forest Land", 3,  "Open Forest",   40,
  "LULC note states that 40% of the area is open forest.",

  91, 1, 3, 2, "Cropland",    NA, NA,              20,
  "LULC note states that the remaining 20% is farmland.",

  91, 2, 2, 1, "Forest Land", 3,  "Open Forest",   70,
  "LULC note states: 70% open forest.",

  91, 3, 2, 1, "Forest Land", 3,  "Open Forest",   40,
  "LULC note states: 40% open forest.",

  91, 5, 2, 1, "Forest Land", 3,  "Open Forest",   50,
  "LULC note states: 50% open forest.",

  91, 5, 3, 2, "Cropland",    NA, NA,              20,
  "LULC note states: 20% crop land.",

  93, 1, 2, 1, "Forest Land", 3,  "Open Forest",   90,
  "LULC note states: 90% open forest.",

  93, 3, 2, 1, "Forest Land", 3,  "Open Forest",    5,
  "LULC note states: 5% open forest.",

  93, 3, 3, 2, "Cropland",    NA, NA,              90,
  "LULC note states: 90% crop land.",

  93, 4, 2, 1, "Forest Land", 3,  "Open Forest",   85,
  "LULC note states: 85% open forest.",

  93, 5, 2, 1, "Forest Land", 3,  "Open Forest",   75,
  "LULC note states: 75% open forest."
)

tmp$lulc_percentage_rows_to_add <- tmp$lulc_percentage_recoveries %>%
  transmute(
    cluster_no,
    cluster_no_label = cluster_no,
    plot_no,
    plot_no_label = plot_no,
    lulc_id,
    lulc_name,
    lulc_name_label,
    lulc_forest = as.numeric(lulc_forest),
    lulc_forest_label,
    lulc_percentage,
    lulc_note = paste("Recovered from existing LULC note:", recovery_evidence)
  ) %>%
  anti_join(
    plot_lulc %>% select(cluster_no, plot_no, lulc_id),
    by = c("cluster_no", "plot_no", "lulc_id")
  )

plot_lulc <- plot_lulc %>%
  bind_rows(tmp$lulc_percentage_rows_to_add) %>%
  arrange(cluster_no, plot_no, lulc_id)

## 21.6 CLEAN INAPPLICABLE FOREST-SUBTYPE VALUES

## Forest subtype only applies to Forest Land
plot_lulc <- plot_lulc %>%
  mutate(
    lulc_forest_old = lulc_forest,
    lulc_forest = case_when(
      lulc_name_label != "Forest Land" ~ NA_real_,
      TRUE ~ lulc_forest_old
    ),

    lulc_forest_label_old = lulc_forest_label,
    lulc_forest_label = case_when(
      lulc_name_label != "Forest Land" ~ NA_character_,
      TRUE ~ lulc_forest_label_old
    )
  )

## 21.13.3 Record the decisions reached from the review

tmp$mangrove_decisions <- bind_rows(
  crossing(
    cluster_no = c(5, 11, 19, 21, 22, 25, 39, 40, 51, 55),
    plot_no = 1:3
  ),

  crossing(
    cluster_no = c(26, 52),
    plot_no = 1:2
  ),

  tibble(
    cluster_no = c(10, 14, 50, 57),
    plot_no = 1
  )
) %>%
  mutate(
    lulc_id = 1,
    corrected_lulc_name_code = 1,
    corrected_lulc_name = "Forest Land",
    corrected_forest_type_code = 1,
    corrected_forest_type = "Mangrove Forest"
  )

tmp$open_forest_decisions <- bind_rows(
  tibble(cluster_no = 110, plot_no = 1:5),
  tibble(cluster_no = 4, plot_no = 4)
) %>%
  mutate(
    lulc_id = 1,
    corrected_lulc_name_code = 1,
    corrected_lulc_name = "Forest Land",
    corrected_forest_type_code = 3,
    corrected_forest_type = "Open Forest"
  )

tmp$close_forest_decisions <- tibble(
  cluster_no = 72,
  plot_no = 2,
  lulc_id = 1,
  corrected_lulc_name_code = 1,
  corrected_lulc_name = "Forest Land",
  corrected_forest_type_code = 2,
  corrected_forest_type = "Close Forest"
)

tmp$cropland_decisions <- tibble(
  cluster_no = 4,
  plot_no = 2,
  lulc_id = 1,
  corrected_lulc_name_code = 2,
  corrected_lulc_name = "Cropland",
  corrected_forest_type_code = NA_real_,
  corrected_forest_type = NA_character_
)

tmp$lulc_classification_decisions <- bind_rows(
  tmp$mangrove_decisions,
  tmp$open_forest_decisions,
  tmp$close_forest_decisions,
  tmp$cropland_decisions
) %>%
  left_join(
    plot_lulc %>%
      select(
        cluster_no,
        plot_no,
        lulc_id,
        original_lulc_name_code = lulc_name,
        original_lulc_name = lulc_name_label,
        original_forest_type_code = lulc_forest,
        original_forest_type = lulc_forest_label
      ),
    by = c("cluster_no", "plot_no", "lulc_id"),
    relationship = "one-to-one"
  ) %>%
  arrange(cluster_no, plot_no, lulc_id)

## 21.13.9 Apply the verified corrections

plot_lulc <- plot_lulc %>%
  left_join(
    tmp$lulc_classification_decisions %>%
      select(cluster_no, plot_no, lulc_id, starts_with("corrected_")),
    by = c("cluster_no", "plot_no", "lulc_id"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    lulc_name_old = lulc_name,
    lulc_name = coalesce(corrected_lulc_name_code, lulc_name_old),

    lulc_name_label_old = lulc_name_label,
    lulc_name_label = coalesce(corrected_lulc_name, lulc_name_label_old),

    lulc_forest = case_when(
      !is.na(corrected_forest_type_code) ~ corrected_forest_type_code,
      lulc_name != 1 ~ NA_real_,
      TRUE ~ lulc_forest
    ),

    lulc_forest_label = case_when(
      !is.na(corrected_forest_type) ~ corrected_forest_type,
      lulc_name != 1 ~ NA_character_,
      TRUE ~ lulc_forest_label
    )
  ) %>%
  select(-starts_with("corrected_")) %>%
  arrange(cluster_no, plot_no, lulc_id)

data_clean$plot_lulc <- plot_lulc



##
## NTFP ######
##

## Source: scripts/02_ntfp_data_prep.R

tmp$key_ntfp <- c("cluster_no", "plot_no", "ntfp_id")

## ATTRIBUTES 18a AND 18b – NTFP NAME

## 8. Clean the NTFP name fields (trim spaces, empty to NA)
ntfp <- data_raw$ntfp %>%
  mutate(
    # Official main NTFP name from Attribute 18a
    ntfp_name_label_old = ntfp_name_label,
    ntfp_name_label = na_if(str_squish(as.character(ntfp_name_label_old)), ""),

    # Official supplementary fruits/nuts name from Attribute 18b
    ntfp_name_fruits_other_label_old = ntfp_name_fruits_other_label,
    ntfp_name_fruits_other_label = na_if(str_squish(as.character(ntfp_name_fruits_other_label_old)), ""),

    # Manually entered name where Attribute 18a = Code 99
    ntfp_name_other_old = ntfp_name_other,
    ntfp_name_other = na_if(str_squish(as.character(ntfp_name_other_old)), "")
  )

## 14. Apply verified correction to the NTFP name-entry route
## Cluster 43, Plot 1, NTFP 1 is "Kutifingo", an other fruit/nut (code 90, sub-code 11)
ntfp <- ntfp %>%
  mutate(
    ntfp_name_old = ntfp_name,
    ntfp_name = case_when(
      cluster_no == 43 & plot_no == 1 & ntfp_id == 1 ~ 90,
      TRUE ~ ntfp_name_old
    ),

    ntfp_name_fruits_other_old = ntfp_name_fruits_other,
    ntfp_name_fruits_other = case_when(
      cluster_no == 43 & plot_no == 1 & ntfp_id == 1 ~ 11,
      TRUE ~ ntfp_name_fruits_other_old
    ),

    ntfp_name_label = case_when(
      cluster_no == 43 & plot_no == 1 & ntfp_id == 1 ~ "Other fruits/ nuts",
      TRUE ~ ntfp_name_label
    ),

    ntfp_name_fruits_other_label = case_when(
      cluster_no == 43 & plot_no == 1 & ntfp_id == 1 ~ "Kutifingo",
      TRUE ~ ntfp_name_fruits_other_label
    )
  )

## 15. Create one consolidated recorded NTFP name
ntfp <- ntfp %>%
  mutate(
    ntfp_name_recorded = case_when(
      # Code 90: use the official Attribute 18b supplementary fruits/nuts name
      ntfp_name == 90 ~ ntfp_name_fruits_other_label,

      # Code 99: use the manually entered name
      ntfp_name == 99 ~ ntfp_name_other,

      # All other Attribute 18a codes: retain the official Arena-listed name
      TRUE ~ ntfp_name_label
    )
  )

## 18-21. Prepare the DoF-validated NTFP name mapping table
tmp$clean_ntfp_key <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    str_replace("[,;\\.]+\\s*$", "") %>%
    str_to_lower()
}

tmp$ntfp_name_map <- anci$ntfp_name_map %>%
  transmute(
    ntfp_name_key = tmp$clean_ntfp_key(ntfp_name_other_recorded),
    ntfp_name_standardized = na_if(str_squish(as.character(ntfp_name_standardized)), ""),
    mapping_comment = na_if(str_squish(as.character(comments)), "")
  ) %>%
  group_by(ntfp_name_key) %>%
  summarise(
    ntfp_name_standardized = first(ntfp_name_standardized),
    mapping_comment = paste(sort(unique(na.omit(mapping_comment))), collapse = " | "),
    .groups = "drop"
  )

## 22-23. Apply the mapping to manually entered names and create the final NTFP name
## The final name replaces 'ntfp_name_label': the Arena label for listed NTFPs,
## the 18b name for other fruits/nuts (code 90) and the standardized manual name for code 99
ntfp <- ntfp %>%
  mutate(
    ntfp_name_key = case_when(
      ntfp_name == 99 ~ tmp$clean_ntfp_key(ntfp_name_other),
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(tmp$ntfp_name_map, by = "ntfp_name_key") %>%
  mutate(
    ntfp_name_label = case_when(
      ntfp_name == 99 ~ coalesce(ntfp_name_standardized, ntfp_name_recorded),
      TRUE ~ ntfp_name_recorded
    )
  )

## 27. Remove temporary name-cleaning variables
ntfp <- ntfp %>%
  select(-ntfp_name_key, -ntfp_name_standardized, -mapping_comment)

## 35. Prepare the cleaned long NTFP Type table
ntfp_type <- data_raw$ntfp_type %>%
  select(all_of(tmp$key_ntfp), ntfp_type, ntfp_type_label) %>%
  arrange(cluster_no, plot_no, ntfp_id, ntfp_type)

## 36-37. Combine multiple types into one row per NTFP and join to the main table
tmp$ntfp_type_summary <- ntfp_type %>%
  group_by(cluster_no, plot_no, ntfp_id) %>%
  arrange(ntfp_type, .by_group = TRUE) %>%
  summarise(
    n_ntfp_types = n_distinct(ntfp_type),
    ntfp_type_codes = paste(unique(ntfp_type), collapse = " | "),
    ntfp_type_labels = paste(unique(ntfp_type_label), collapse = " | "),
    .groups = "drop"
  )

ntfp <- ntfp %>%
  left_join(tmp$ntfp_type_summary, by = tmp$key_ntfp)

data_clean$ntfp <- ntfp

## 44. Prepare the cleaned long NTFP Type table for analysis
ntfp_type <- ntfp_type %>%
  left_join(
    ntfp %>%
      select(all_of(tmp$key_ntfp), ntfp_name_label, ntfp_status, ntfp_status_label, ntfp_note),
    by = tmp$key_ntfp
  ) %>%
  select(
    all_of(tmp$key_ntfp),
    ntfp_name_label,
    ntfp_type,
    ntfp_type_label,
    ntfp_status,
    ntfp_status_label,
    ntfp_note
  )

data_clean$ntfp_type <- ntfp_type



##
## Wildlife ######
##

## Source: scripts/03_wildlife_data_prep.R

tmp$key_wildlife <- c("cluster_no", "plot_no", "wildlife_id")

## ATTRIBUTE 22 - WILDLIFE NAME

## Trim the name fields, empty to NA
tmp$wildlife_name_cols <- c(
  "wildlife_bird_scientific_name",
  "wildlife_mammal_scientific_name",
  "wildlife_rep_amp_fis",
  "wildlife_other",
  "wildlife_note"
)

wildlife <- data_raw$wildlife %>%
  mutate(
    across(all_of(tmp$wildlife_name_cols), ~ .x, .names = "{.col}_old"),
    across(all_of(tmp$wildlife_name_cols), ~ na_if(str_trim(as.character(.x)), ""))
  )

## 22.2 CONSOLIDATE WILDLIFE NAME FIELDS
## Select the wildlife-name field corresponding to the wildlife type recorded in Arena
wildlife <- wildlife %>%
  mutate(
    wildlife_name_recorded = case_when(
      wildlife_type_label == "Birds" ~ wildlife_bird_scientific_name,
      wildlife_type_label == "Mammals" ~ wildlife_mammal_scientific_name,
      wildlife_type_label %in% c("Reptiles", "Amphibians", "Fishes") ~ wildlife_rep_amp_fis,
      wildlife_type_label %in% c("Other", "Others") ~ wildlife_other,
      TRUE ~ NA_character_
    )
  )

## "Unlisted" species with a name in the note
wildlife <- wildlife %>%
  mutate(
    wildlife_name_recorded = case_when(
      str_to_lower(wildlife_name_recorded) == "unlisted" & !is.na(wildlife_note) ~ wildlife_note,
      TRUE ~ wildlife_name_recorded
    )
  )

## Standardize manually entered names with the wildlife name map
tmp$wildlife_name_map <- anci$wildlife_name_map %>%
  mutate(wildlife_name_manually_recorded = str_squish(wildlife_name_manually_recorded)) %>%
  rename(
    wildlife_name_dof_confirmation = dof_confirmation,
    wildlife_name_map_comments = comments
  )

wildlife <- wildlife %>%
  mutate(
    wildlife_name_map_key = case_when(
      # Bird UNL: manually entered name stored directly in the scientific-name field
      wildlife_type_label == "Birds" &
        wildlife_bird == "UNL" &
        !is.na(wildlife_bird_scientific_name) &
        str_to_lower(wildlife_bird_scientific_name) != "unlisted" ~
        str_squish(wildlife_bird_scientific_name),

      # Bird UNL: scientific-name field remained "Unlisted";
      # use wildlife_note where a usable name was entered
      wildlife_type_label == "Birds" &
        wildlife_bird == "UNL" &
        !is.na(wildlife_note) ~
        str_squish(wildlife_note),

      # Mammal UNL: manually entered name stored directly in the scientific-name field
      wildlife_type_label == "Mammals" &
        wildlife_mammal == "UNL" &
        !is.na(wildlife_mammal_scientific_name) &
        str_to_lower(wildlife_mammal_scientific_name) != "unlisted" ~
        str_squish(wildlife_mammal_scientific_name),

      # Mammal UNL: use wildlife_note where applicable
      wildlife_type_label == "Mammals" &
        wildlife_mammal == "UNL" &
        !is.na(wildlife_note) ~
        str_squish(wildlife_note),

      # Reptiles, Amphibians and Fishes were manually entered
      wildlife_type_label %in% c("Reptiles", "Amphibians", "Fishes") &
        !is.na(wildlife_rep_amp_fis) ~
        str_squish(wildlife_rep_amp_fis),

      # Wildlife Type = Others
      wildlife_type_label %in% c("Other", "Others") &
        !is.na(wildlife_other) ~
        str_squish(wildlife_other),

      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    tmp$wildlife_name_map,
    by = c("wildlife_name_map_key" = "wildlife_name_manually_recorded")
  ) %>%
  mutate(
    wildlife_name = coalesce(wildlife_name_standardized, wildlife_name_recorded)
  )

## Unlisted species without usable name are set to Unknown
wildlife <- wildlife %>%
  mutate(
    wildlife_name_cleaning_note = case_when(
      (wildlife_type_label == "Birds" & wildlife_bird == "UNL" & is.na(wildlife_name_map_key)) |
        (wildlife_type_label == "Mammals" & wildlife_mammal == "UNL" & is.na(wildlife_name_map_key)) ~
        "Arena species recorded as Unlisted; no usable wildlife name provided. Standardized to Unknown.",

      TRUE ~ NA_character_
    ),

    wildlife_name = case_when(
      !is.na(wildlife_name_cleaning_note) ~ "Unknown",
      TRUE ~ wildlife_name
    )
  )

## ATTRIBUTE 23 – WILDLIFE TYPE

wildlife <- wildlife %>%
  mutate(
    wildlife_type_old = wildlife_type,
    wildlife_type = case_when(
      # Bush pig / Warthog records incorrectly entered as Others
      wildlife_type_old == 99 & wildlife_name == "Phacochoerus africanus" ~ 2,

      # Fiddler Crab incorrectly entered as Fish
      wildlife_type_old == 5 & wildlife_name == "West African Fiddler Crab" ~ 99,

      TRUE ~ wildlife_type_old
    ),

    wildlife_type_label_old = wildlife_type_label,
    wildlife_type_label = case_when(
      wildlife_type == 1 ~ "Birds",
      wildlife_type == 2 ~ "Mammals",
      wildlife_type == 3 ~ "Reptiles",
      wildlife_type == 4 ~ "Amphibians",
      wildlife_type == 5 ~ "Fishes",
      wildlife_type == 99 ~ "Others",
      TRUE ~ NA_character_
    ),

    wildlife_type_cleaning_note = case_when(
      wildlife_type_old == 99 & wildlife_name == "Phacochoerus africanus" ~
        "Recorded as Others but identified as Phacochoerus africanus; Wildlife Type corrected to Mammals.",

      wildlife_type_old == 5 & wildlife_name == "West African Fiddler Crab" ~
        "Recorded as Fishes but identified as West African Fiddler Crab; Wildlife Type corrected to Others.",

      TRUE ~ NA_character_
    )
  )

data_clean$wildlife <- wildlife

## ATTRIBUTE 24 – WILDLIFE SIGN

tmp$wildlife_sign_expected <- tibble(
  wildlife_sign = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 99),
  wildlife_sign_expected_label = c(
    "Visual sightings",
    "Sounds",
    "Tracks",
    "Scat",
    "Burrows, dens, and nests",
    "Feeding signs",
    "Scratching and claw marks",
    "Feathers",
    "Trails",
    "Holes in the ground or trees",
    "Droppings",
    "Scrapes or diggings",
    "Bones, fur, and skin",
    "Urine",
    "Others"
  )
)

wildlife_sign <- data_raw$wildlife_sign %>%
  left_join(
    wildlife %>% select(all_of(tmp$key_wildlife), wildlife_name, wildlife_sign_other),
    by = tmp$key_wildlife
  ) %>%
  mutate(
    wildlife_sign_old = wildlife_sign,
    wildlife_sign = case_when(
      # Village Weaver: Birds nests
      cluster_no == 17 & plot_no == 3 & wildlife_id == 1 & wildlife_sign_old == 99 ~ 5,

      # Mungos mungo: Droppings already recorded separately;
      # retain the additional "diggings" information here
      cluster_no == 35 & plot_no == 2 & wildlife_id == 1 & wildlife_sign_old == 99 ~ 12,

      TRUE ~ wildlife_sign_old
    ),

    # Flag duplicate Others record for exclusion
    wildlife_sign_keep = case_when(
      # Warthog "footprints" duplicates an existing Tracks record
      cluster_no == 107 & plot_no == 1 & wildlife_id == 1 & wildlife_sign_old == 99 ~ FALSE,

      TRUE ~ TRUE
    ),

    # Document changes
    wildlife_sign_cleaning_note = case_when(
      cluster_no == 17 & plot_no == 3 & wildlife_id == 1 & wildlife_sign_old == 99 ~
        "Recorded as Others with 'Birds nests'; corrected to Burrows, dens, and nests.",

      cluster_no == 35 & plot_no == 2 & wildlife_id == 1 & wildlife_sign_old == 99 ~
        "Recorded as Others with 'Droppings, diggings'; Droppings already recorded separately, so this record was corrected to Scrapes or diggings.",

      cluster_no == 107 & plot_no == 1 & wildlife_id == 1 & wildlife_sign_old == 99 ~
        "Recorded as Others with 'footprints'; Tracks already recorded separately, so duplicate Others record excluded from cleaned sign table.",

      TRUE ~ NA_character_
    )
  )

## Assign the labels from the validated sign lookup
wildlife_sign <- wildlife_sign %>%
  left_join(tmp$wildlife_sign_expected, by = "wildlife_sign") %>%
  mutate(
    wildlife_sign_label_old = wildlife_sign_label,
    wildlife_sign_label = wildlife_sign_expected_label
  ) %>%
  select(-wildlife_sign_expected_label)

## Mammals recorded with Feathers, verified by DoF
tmp$wildlife_sign_dof_feedback <- tibble(
  cluster_no  = c(84, 80, 80, 80, 80),
  plot_no     = c(1,  2,  4,  5,  5),
  wildlife_id = c(2,  2,  2,  2,  3),
  wildlife_sign = c(8, 8, 8, 8, 8),
  dof_sign_confirmation = c("Unresolved", "Tracks", "Unresolved", "Unresolved", "Tracks")
)

wildlife_sign <- wildlife_sign %>%
  left_join(
    tmp$wildlife_sign_dof_feedback,
    by = c(tmp$key_wildlife, "wildlife_sign_old" = "wildlife_sign")
  ) %>%
  mutate(
    wildlife_sign = case_when(
      dof_sign_confirmation == "Tracks" ~ 3,
      dof_sign_confirmation == "Unresolved" ~ NA_real_,
      TRUE ~ wildlife_sign
    ),

    wildlife_sign_label = case_when(
      dof_sign_confirmation == "Tracks" ~ "Tracks",
      dof_sign_confirmation == "Unresolved" ~ NA_character_,
      TRUE ~ wildlife_sign_label
    ),

    wildlife_sign_cleaning_note = case_when(
      dof_sign_confirmation == "Tracks" ~
        "Recorded as Feathers; corrected to Tracks based on DoF verification.",

      dof_sign_confirmation == "Unresolved" ~
        "Mammal recorded with Feathers; inconsistency unresolved. Cleaned sign set to NA because no replacement sign was confirmed.",

      TRUE ~ wildlife_sign_cleaning_note
    )
  )

## Final long table: remove only the duplicated footprints row
data_clean$wildlife_sign <- wildlife_sign %>%
  filter(wildlife_sign_keep) %>%
  select(
    all_of(tmp$key_wildlife),
    wildlife_name,
    wildlife_sign,
    wildlife_sign_label,
    wildlife_sign_other,
    wildlife_sign_cleaning_note,
    dof_sign_confirmation,
    wildlife_sign_old,
    wildlife_sign_label_old
  )



##
## Tree ######
##

## Source: scripts/04_tree_data_prep.R

tmp$key_tree <- c("cluster_no", "plot_no", "tree_no")

## IMPORT DATASET

## Cluster and plot context from the clean tables
tree <- data_raw$tree %>%
  left_join(
    data_clean$cluster %>% select(cluster_no, cluster_info_stratum, cluster_info_region_label),
    by = "cluster_no",
    relationship = "many-to-one"
  ) %>%
  left_join(
    data_clean$plot %>% select(cluster_no, plot_no, plot_access_label),
    by = c("cluster_no", "plot_no"),
    relationship = "many-to-one"
  ) %>%
  relocate(cluster_info_stratum, cluster_info_region_label, .before = cluster_no) %>%
  relocate(plot_access_label, .before = plot_no)

## ATTRIBUTE 3: LAND COVER NO

## Tree LULC IDs not found in the plot LULC table
tmp$tree_lulc_links_not_found <- tree %>%
  anti_join(
    data_clean$plot_lulc,
    by = c("cluster_no", "plot_no", "tree_lulc_no" = "lulc_id")
  )

## LULC ID 2 does not exist for Cluster 15, Plot 4. The plot contains only LULC ID 1,
## classified as 100% Forest Land - Open Forest.
tmp$tree_lulc_link_corrections <- tmp$tree_lulc_links_not_found %>%
  filter(cluster_no == 15, plot_no == 4, tree_lulc_no == 2) %>%
  transmute(
    cluster_no,
    plot_no,
    tree_no,
    corrected_tree_lulc_no = 1,
    corrected_tree_lulc_no_label = "LU/LC id 1"
  )

tree <- tree %>%
  left_join(
    tmp$tree_lulc_link_corrections,
    by = tmp$key_tree,
    relationship = "one-to-one"
  ) %>%
  mutate(
    tree_lulc_no_old = tree_lulc_no,
    tree_lulc_no = coalesce(corrected_tree_lulc_no, tree_lulc_no_old),

    tree_lulc_no_label_old = tree_lulc_no_label,
    tree_lulc_no_label = coalesce(corrected_tree_lulc_no_label, tree_lulc_no_label_old)
  ) %>%
  select(-corrected_tree_lulc_no, -corrected_tree_lulc_no_label)

## ATTRIBUTE 4: TREE STATUS

## 11 records originally classified in Arena as stumps, verified by DoF as dead standing trees
tmp$dof_stumps_to_dead <- tribble(
  ~cluster_no, ~plot_no, ~tree_no,
  16,          2,         1,
  28,          2,         7,
  29,          2,        19,
  29,          2,        20,
  34,          4,         8,
  43,          4,         1,
  45,          1,         4,
  58,          1,         1,
  65,          5,         1,
  97,          1,         5,
  98,          4,        10
) %>%
  mutate(dof_status_decision = "DoF verified: reclassify as dead standing tree")

tmp$dead_standing_code <- tree %>%
  filter(tree_status_label == "Dead standing tree") %>%
  distinct(tree_status) %>%
  pull(tree_status)

tree <- tree %>%
  left_join(tmp$dof_stumps_to_dead, by = tmp$key_tree) %>%
  mutate(
    tree_status_old = tree_status,
    tree_status = case_when(
      !is.na(dof_status_decision) ~ tmp$dead_standing_code,
      TRUE ~ tree_status_old
    ),

    tree_status_label_old = tree_status_label,
    tree_status_label = case_when(
      !is.na(dof_status_decision) ~ "Dead standing tree",
      TRUE ~ tree_status_label_old
    )
  )

## ATTRIBUTE 5: SPECIES SCIENTIFIC NAME

tmp$tree_species_map <- anci$tree_species_map %>%
  select(
    map_key,
    species_code_clean,
    scientific_name_clean,
    scientific_name_report,
    local_name_clean,
    growth_form,
    include_in_tree_species_analysis
  )

tree <- tree %>%
  mutate(
    local_name_key = case_when(
      is.na(tree_local_name) | str_squish(tree_local_name) == "" ~ "__NO_NAME__",
      TRUE ~ str_to_lower(str_squish(tree_local_name))
    ),

    map_key = case_when(
      # Missing Arena species code cannot be used to construct a species-map key
      is.na(tree_species) | str_squish(tree_species) == "" ~ NA_character_,

      # UNL and UNK require the recorded local name to distinguish the different field identifications
      str_to_upper(str_squish(tree_species)) %in% c("UNL", "UNK") ~
        paste0(str_to_upper(str_squish(tree_species)), "|", local_name_key),

      # Existing Arena-listed species are mapped using species code
      TRUE ~ str_squish(tree_species)
    )
  ) %>%
  left_join(tmp$tree_species_map, by = "map_key") %>%
  mutate(
    tree_species_old = tree_species,
    tree_species = coalesce(species_code_clean, tree_species_old),

    tree_species_scientific_name_old = tree_species_scientific_name,
    tree_species_scientific_name = coalesce(scientific_name_clean, tree_species_scientific_name_old),

    # Scientific name for reporting
    # Where an accepted reporting name is supplied by the map, use it
    # Otherwise use the cleaned NFI scientific name
    tree_species_scientific_name_report = coalesce(
      scientific_name_report,
      scientific_name_clean,
      tree_species_scientific_name_old
    )
  )

## ATTRIBUTE 6: SPECIES LOCAL NAME

tree <- tree %>%
  mutate(
    tree_local_name_old = tree_local_name,
    tree_local_name = coalesce(local_name_clean, tree_local_name_old)
  ) %>%
  select(
    -local_name_key, -map_key,
    -species_code_clean, -scientific_name_clean, -scientific_name_report, -local_name_clean
  )

## ATTRIBUTE 9: DIAMETER MEASUREMENT HEIGHT

tree <- tree %>%
  mutate(
    # Stump-height QA flag
    stump_height_flag = case_when(
      tree_status_label == "Stump" & !is.na(tree_height) & tree_height < 0.10 ~
        "VERIFY: unusually low stump height",
      tree_status_label == "Stump" ~ "OK",
      TRUE ~ NA_character_
    ),

    # Stump diameter measurement-height QA flag
    stump_diameter_height_flag = case_when(
      tree_status_label == "Stump" &
        !is.na(tree_diameter_height) &
        !is.na(tree_height) &
        tree_diameter_height > tree_height &
        tree_diameter_height != 1.30 ~ "VERIFY",
      tree_status_label == "Stump" ~ "OK",
      TRUE ~ NA_character_
    )
  )

tree <- tree %>%
  mutate(
    tree_diameter_height_old = tree_diameter_height,
    tree_diameter_height = case_when(
      # Questionable very-low stump height
      tree_status_label == "Stump" & stump_height_flag != "OK" ~ NA_real_,

      # Unresolved non-default physical conflict
      tree_status_label == "Stump" & stump_diameter_height_flag == "VERIFY" ~ NA_real_,

      # Short stump retaining Arena's default 1.30 m
      tree_status_label == "Stump" &
        tree_height >= 0.10 &
        tree_height < 1.30 &
        tree_diameter_height_old == 1.30 ~ tree_height,

      # Otherwise retain the recorded value
      TRUE ~ tree_diameter_height_old
    ),

    # Audit variable explaining how each value was treated
    tree_diameter_height_status = case_when(
      tree_status_label == "Stump" & stump_height_flag != "OK" ~
        "Unresolved: questionable stump height",

      tree_status_label == "Stump" & stump_diameter_height_flag == "VERIFY" ~
        "Unresolved: stump measurement-height conflict",

      tree_status_label == "Stump" &
        tree_height >= 0.10 &
        tree_height < 1.30 &
        tree_diameter_height_old == 1.30 ~
        "Corrected: Arena 1.30-m default replaced using recorded stump height",

      TRUE ~ "Recorded value retained"
    ),

    reclassified_dead_dbh_flag = case_when(
      !is.na(dof_status_decision) & tree_diameter_height_old != 1.30 ~
        "REVIEW DURING ANALYSIS: diameter measured away from 1.30 m",
      TRUE ~ NA_character_
    )
  )

## ATTRIBUTE 10: DECOMPOSITION STATUS

## Live trees with decomposition status confirmed by DoF
tmp$dof_live_decomposition <- tribble(
  ~cluster_no, ~plot_no, ~tree_no,
  13,          2,        8,
  18,          4,        4,
  71,          1,        8,
  117,         3,       16
) %>%
  mutate(dof_live_decomposition_confirmed = TRUE)

tree <- tree %>%
  left_join(tmp$dof_live_decomposition, by = tmp$key_tree) %>%
  mutate(
    # Decomposition is not applicable to live standing trees
    tree_stump_decomposition_old = tree_stump_decomposition,
    tree_stump_decomposition = case_when(
      tree_status_label == "Live standing tree" ~ NA_real_,
      TRUE ~ tree_stump_decomposition_old
    ),

    tree_stump_decomposition_label_old = tree_stump_decomposition_label,
    tree_stump_decomposition_label = case_when(
      tree_status_label == "Live standing tree" ~ NA_character_,
      TRUE ~ tree_stump_decomposition_label_old
    )
  )

## ATTRIBUTE 11: TREE HEIGHT

## Standing trees with total height below the diameter measurement height
tmp$standing_height_conflict <- tree %>%
  filter(
    tree_status_label %in% c("Live standing tree", "Dead standing tree"),
    !is.na(tree_height),
    !is.na(tree_diameter_height),
    tree_height < tree_diameter_height
  )

tree <- tree %>%
  mutate(
    standing_height_flag = case_when(
      tree_status_label %in% c("Live standing tree", "Dead standing tree") &
        !is.na(tree_height) &
        !is.na(tree_diameter_height) &
        tree_height < tree_diameter_height ~
        "VERIFY: total height below diameter measurement height",
      TRUE ~ NA_character_
    )
  )

## DBH-height outliers: live tree heights compared within stratum and 5-cm DBH class
tmp$dbh_height_check <- tree %>%
  filter(
    tree_status_label == "Live standing tree",
    !is.na(tree_dbh),
    !is.na(tree_height),
    tree_dbh > 0,
    tree_height > 0,
    is.na(standing_height_flag)
  ) %>%
  mutate(
    # 5-cm DBH classes, e.g. 10 = 10.0–14.9 cm
    dbh_class_5cm = floor(tree_dbh / 5) * 5
  )

tmp$min_comparison_n <- 10

tmp$dbh_height_limits <- tmp$dbh_height_check %>%
  group_by(cluster_info_stratum, dbh_class_5cm) %>%
  summarise(
    n_trees = n(),
    height_q1 = quantile(tree_height, 0.25, na.rm = TRUE),
    height_median = median(tree_height, na.rm = TRUE),
    height_q3 = quantile(tree_height, 0.75, na.rm = TRUE),
    height_iqr = IQR(tree_height, na.rm = TRUE),
    lower_limit_1_5 = height_q1 - 1.5 * height_iqr,
    upper_limit_1_5 = height_q3 + 1.5 * height_iqr,
    lower_limit_3 = height_q1 - 3 * height_iqr,
    upper_limit_3 = height_q3 + 3 * height_iqr,
    .groups = "drop"
  )

tmp$dbh_height_outliers <- tmp$dbh_height_check %>%
  left_join(tmp$dbh_height_limits, by = c("cluster_info_stratum", "dbh_class_5cm")) %>%
  mutate(
    dbh_height_review_flag = case_when(
      n_trees < tmp$min_comparison_n ~ "INSUFFICIENT COMPARISON DATA",
      tree_height > upper_limit_3 ~ "PRIORITY VERIFY: extremely tall for DBH",
      tree_height < lower_limit_3 ~ "PRIORITY VERIFY: extremely short for DBH",
      tree_height > upper_limit_1_5 ~ "REVIEW: unusually tall for DBH",
      tree_height < lower_limit_1_5 ~ "REVIEW: unusually short for DBH",
      TRUE ~ "OK"
    )
  )

## Outlier heights confirmed by DoF field measurement
tmp$dof_height_confirmed <- tribble(
  ~cluster_no, ~plot_no, ~tree_no,
  46,          4,        4,
  46,          4,        5,
  90,          5,        2,
  18,          2,        1,
  15,          2,        9,
  55,          3,       16,
  102,         5,        8,
  80,          3,       10,
  55,          1,       11,
  55,          2,       19,
  72,          1,       33,
  35,          5,        6,
  33,          1,        2
) %>%
  mutate(dof_height_status = "DoF confirmed recorded height")

## Unresolved height issues
tmp$dbh_height_unresolved_dof <- tmp$dbh_height_outliers %>%
  filter(grepl("REVIEW|PRIORITY VERIFY", dbh_height_review_flag)) %>%
  anti_join(tmp$dof_height_confirmed, by = tmp$key_tree) %>%
  transmute(
    cluster_no,
    plot_no,
    tree_no,
    height_issue = "Unresolved: DBH-height outlier not confirmed by field measurement"
  ) %>%
  distinct()

tmp$physical_height_unresolved <- tmp$standing_height_conflict %>%
  transmute(
    cluster_no,
    plot_no,
    tree_no,
    height_issue = "Unresolved: total height below diameter measurement height"
  ) %>%
  distinct()

tmp$stump_height_unresolved <- tree %>%
  filter(
    tree_status_label == "Stump",
    stump_height_flag != "OK" | stump_diameter_height_flag == "VERIFY"
  ) %>%
  transmute(
    cluster_no,
    plot_no,
    tree_no,
    height_issue = case_when(
      stump_height_flag != "OK" ~ "Unresolved: questionable stump height",
      stump_diameter_height_flag == "VERIFY" ~ "Unresolved: stump height/measurement-height conflict"
    )
  ) %>%
  distinct()

## One issue per tree, in priority order
tmp$height_unresolved_lookup <- bind_rows(
  tmp$dbh_height_unresolved_dof,
  tmp$physical_height_unresolved,
  tmp$stump_height_unresolved
) %>%
  distinct(cluster_no, plot_no, tree_no, .keep_all = TRUE)

tmp$dbh_height_flag_lookup <- tmp$dbh_height_outliers %>%
  select(cluster_no, plot_no, tree_no, dbh_height_review_flag) %>%
  distinct()

tree <- tree %>%
  left_join(tmp$dbh_height_flag_lookup, by = tmp$key_tree) %>%
  left_join(tmp$dof_height_confirmed, by = tmp$key_tree) %>%
  left_join(tmp$height_unresolved_lookup, by = tmp$key_tree) %>%
  mutate(
    tree_height_old = tree_height,
    tree_height = case_when(
      !is.na(height_issue) ~ NA_real_,
      TRUE ~ tree_height_old
    ),

    # Audit variable explaining how the height was treated
    tree_height_status = case_when(
      !is.na(height_issue) ~ height_issue,
      !is.na(dof_height_status) ~ "Recorded height retained: explicitly confirmed by DoF",
      dbh_height_review_flag == "INSUFFICIENT COMPARISON DATA" ~
        "Recorded height retained: insufficient comparison data for statistical screening",
      TRUE ~ "Recorded height retained"
    )
  )

## ATTRIBUTE 12: STEM QUALITY

## Stem quality, crown and overall condition only apply to live standing trees
tree <- tree %>%
  mutate(
    tree_stem_quality_old = tree_stem_quality,
    tree_stem_quality = case_when(
      tree_status_label == "Live standing tree" ~ tree_stem_quality_old,
      TRUE ~ NA_real_
    ),

    tree_stem_quality_label_old = tree_stem_quality_label,
    tree_stem_quality_label = case_when(
      tree_status_label == "Live standing tree" ~ tree_stem_quality_label_old,
      TRUE ~ NA_character_
    ),

    tree_stem_quality_remark = case_when(
      tree_status_label == "Dead standing tree" & !is.na(tree_stem_quality_label_old) ~
        "Stem quality removed: not applicable to dead standing tree",
      tree_status_label == "Stump" & !is.na(tree_stem_quality_label_old) ~
        "Stem quality removed: not applicable to stump",
      TRUE ~ NA_character_
    )
  )

## ATTRIBUTE 13: CROWN CONDITION

tree <- tree %>%
  mutate(
    tree_condition_crown_old = tree_condition_crown,
    tree_condition_crown = case_when(
      tree_status_label == "Live standing tree" ~ tree_condition_crown_old,
      TRUE ~ NA_real_
    ),

    tree_condition_crown_label_old = tree_condition_crown_label,
    tree_condition_crown_label = case_when(
      tree_status_label == "Live standing tree" ~ tree_condition_crown_label_old,
      TRUE ~ NA_character_
    ),

    tree_condition_crown_remark = case_when(
      tree_status_label == "Dead standing tree" & !is.na(tree_condition_crown_label_old) ~
        "Crown condition removed: not applicable to dead standing tree",
      tree_status_label == "Stump" & !is.na(tree_condition_crown_label_old) ~
        "Crown condition removed: not applicable to stump",
      TRUE ~ NA_character_
    )
  )

## ATTRIBUTE 14: OVERALL TREE CONDITION

tree <- tree %>%
  mutate(
    tree_condition_overall_old = tree_condition_overall,
    tree_condition_overall = case_when(
      tree_status_label == "Live standing tree" ~ tree_condition_overall_old,
      TRUE ~ NA_real_
    ),

    tree_condition_overall_label_old = tree_condition_overall_label,
    tree_condition_overall_label = case_when(
      tree_status_label == "Live standing tree" ~ tree_condition_overall_label_old,
      TRUE ~ NA_character_
    ),

    tree_condition_overall_remark = case_when(
      tree_status_label == "Dead standing tree" & !is.na(tree_condition_overall_label_old) ~
        "Overall condition removed: not applicable to dead standing tree",
      tree_status_label == "Stump" & !is.na(tree_condition_overall_label_old) ~
        "Overall condition removed: not applicable to stump",
      TRUE ~ NA_character_
    )
  )

## ATTRIBUTE 15: CAUSATIVE AGENT(S)

tmp$tree_cause_flags <- data_raw$tree_causes %>%
  group_by(cluster_no, plot_no, tree_no) %>%
  summarise(
    has_actual_cause = any(
      !is.na(tree_condition_causes_label) & tree_condition_causes_label != "Not applicable"
    ),
    has_not_applicable = any(tree_condition_causes_label == "Not applicable", na.rm = TRUE),
    has_other = any(tree_condition_causes_label == "Other", na.rm = TRUE),
    .groups = "drop"
  )

## Multiple field-recorded causes collapsed to one semicolon-separated value
tmp$tree_causes_collapsed <- data_raw$tree_causes %>%
  filter(
    !is.na(tree_condition_causes_label),
    tree_condition_causes_label != "Not applicable"
  ) %>%
  group_by(cluster_no, plot_no, tree_no) %>%
  summarise(
    tree_condition_causes_recorded = paste(sort(unique(tree_condition_causes_label)), collapse = "; "),
    tree_condition_causes_n_recorded = n_distinct(tree_condition_causes_label),
    .groups = "drop"
  )

tree <- tree %>%
  left_join(tmp$tree_causes_collapsed, by = tmp$key_tree) %>%
  left_join(tmp$tree_cause_flags, by = tmp$key_tree) %>%
  mutate(
    tree_condition_causes = case_when(
      # Healthy live trees do not require a causative agent
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label == "Healthy" ~
        "Not applicable",

      # Affected/dying live trees with one or more genuine recorded causes retain those causes
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label != "Healthy" &
        !is.na(tree_condition_causes_recorded) ~
        tree_condition_causes_recorded,

      # Affected/dying live trees without an actual recorded cause
      # are explicitly distinguished from Healthy trees
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label != "Healthy" &
        is.na(tree_condition_causes_recorded) ~
        "Not recorded",

      # Causative-agent information is not applicable to non-live records
      TRUE ~ NA_character_
    ),

    # Number of actual causative agents
    tree_condition_causes_n = case_when(
      # Healthy tree = zero applicable causative agents
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label == "Healthy" ~
        0L,

      # Where genuine cause(s) were recorded, retain their number
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label != "Healthy" &
        !is.na(tree_condition_causes_n_recorded) ~
        tree_condition_causes_n_recorded,

      # For "Not recorded", the number of actual causes is unknown
      TRUE ~ NA_integer_
    ),

    tree_condition_causes_remark = case_when(
      # A cause had been entered even though the tree was Healthy
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label == "Healthy" &
        has_actual_cause %in% TRUE ~
        "Changed to Not applicable: tree recorded as Healthy",

      # Other is retained because it is the field-recorded category, but the actual agent is unknown
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label != "Healthy" &
        has_other %in% TRUE ~
        "Other causative agent not specified",

      # An affected tree was entered as Not applicable
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label != "Healthy" &
        is.na(tree_condition_causes_recorded) &
        has_not_applicable %in% TRUE ~
        "Changed to Not recorded: affected tree had causative agent recorded as Not applicable",

      # The affected tree was completely absent from the causes table
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label != "Healthy" &
        is.na(tree_condition_causes_recorded) &
        is.na(has_actual_cause) ~
        "Changed to Not recorded: affected tree had no causative-agent record",

      TRUE ~ NA_character_
    )
  )

## ATTRIBUTE 16: RECOMMENDED MOTHER TREE

tree <- tree %>%
  mutate(
    tree_mother_tree_old = tree_mother_tree,
    tree_mother_tree = case_when(
      # Mother-tree recommendation is not applicable to non-live records
      tree_status_label != "Live standing tree" ~ NA,

      # Healthy live trees retain the field recommendation
      tree_condition_overall_label == "Healthy" ~ tree_mother_tree_old,

      # Affected or dying live trees are not retained as recommended mother trees
      tree_condition_overall_label != "Healthy" ~ FALSE,

      TRUE ~ NA
    ),

    tree_mother_tree_remark = case_when(
      # Recommendation recorded for a non-live record
      tree_status_label != "Live standing tree" & !is.na(tree_mother_tree_old) ~
        "Mother-tree recommendation removed: not applicable to non-live record",

      # Non-healthy tree had originally been recommended
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label != "Healthy" &
        tree_mother_tree_old %in% TRUE ~
        "Changed from TRUE to FALSE: tree not Healthy",

      # Non-healthy tree had no recommendation recorded
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label != "Healthy" &
        is.na(tree_mother_tree_old) ~
        "Set to FALSE: tree not Healthy",

      # Healthy tree with missing recommendation remains unresolved
      tree_status_label == "Live standing tree" &
        tree_condition_overall_label == "Healthy" &
        is.na(tree_mother_tree_old) ~
        "Recommendation not recorded: retained as NA",

      TRUE ~ NA_character_
    )
  )

## CREATE FINAL ANALYSIS-READY TREE DATASET

data_clean$tree <- tree %>%
  select(
    cluster_no,
    cluster_info_stratum,
    cluster_info_region_label,
    plot_no,
    plot_access_label,
    tree_no,
    tree_plot_type,
    tree_plot_type_label,
    tree_lulc_no,
    tree_lulc_no_label,
    tree_status,
    tree_status_label,
    tree_species,
    tree_species_scientific_name,
    tree_species_scientific_name_report,
    tree_species_vernacular_name,
    tree_local_name,
    growth_form,
    include_in_tree_species_analysis,
    tree_dbh,
    tree_diameter_height,
    tree_stump_decomposition,
    tree_stump_decomposition_label,
    tree_height,
    tree_stem_quality,
    tree_stem_quality_label,
    tree_condition_crown,
    tree_condition_crown_label,
    tree_condition_overall,
    tree_condition_overall_label,
    tree_condition_causes_recorded,
    tree_condition_causes_n_recorded,
    tree_condition_causes,
    tree_condition_causes_n,
    tree_mother_tree,
    ends_with("_old")
  )



##
## Regeneration ######
##

## Source: scripts/05_Regeneration_data prep.R

tmp$key_regen <- c("cluster_no", "plot_no", "regen_id")

## ATTRIBUTES 8 & 9: SPECIES SCIENTIFIC AND LOCAL NAME

## The DoF-verified tree species map is the primary reference for manually entered names.
## The DoF-verified NTFP name map is an additional reference, particularly for local-name
## variants not covered by the tree species map.

tmp$regen_ntfp_name_reference <- anci$ntfp_name_map %>%
  mutate(name_key = str_to_lower(str_squish(ntfp_name_other_recorded))) %>%
  filter(!is.na(name_key), name_key != "") %>%
  distinct(name_key, ntfp_name_standardized)

## Unlisted and unknown species matched to the reference maps
tmp$regen_unlisted_name_review <- data_raw$regen %>%
  filter(regen_species %in% c("UNL", "UNK")) %>%
  mutate(
    scientific_entry_key = str_to_lower(str_squish(regen_species_scientific_name)),
    local_entry_key = str_to_lower(str_squish(regen_local_name)),
    map_key = paste0(regen_species, "|", coalesce(na_if(local_entry_key, ""), "__NO_NAME__"))
  ) %>%
  left_join(
    anci$tree_species_map %>%
      select(
        map_key,
        tree_map_scientific_name = scientific_name_clean,
        tree_map_local_name = local_name_clean
      ),
    by = "map_key"
  ) %>%
  left_join(
    tmp$regen_ntfp_name_reference %>%
      rename(
        scientific_entry_key = name_key,
        ntfp_match_from_scientific_field = ntfp_name_standardized
      ),
    by = "scientific_entry_key"
  ) %>%
  left_join(
    tmp$regen_ntfp_name_reference %>%
      rename(
        local_entry_key = name_key,
        ntfp_match_from_local_field = ntfp_name_standardized
      ),
    by = "local_entry_key"
  ) %>%
  select(
    all_of(tmp$key_regen),
    regen_species,
    regen_species_scientific_name,
    regen_local_name,
    tree_map_scientific_name,
    tree_map_local_name,
    ntfp_match_from_scientific_field,
    ntfp_match_from_local_field
  )

## Name review decisions for local-name variants
tmp$regen_name_review_map <- tribble(
  ~variants, ~scientific_name_clean, ~local_name_clean,
  ~decision_source, ~decision_note, ~dof_confirmation_pending,

  "mamakunkoi|kankannanho|kankanango",
  "Guiera senegalensis", "Mamakunkoyo",
  "DoF name maps",
  "Reviewed local-name variants standardized.", FALSE,

  "sambengo|sambengho|sambango|sambem|sambemb|vitex spp|vitex sspp|vitex spp sambango|vitex spp sanbengo|vitex spp santango",
  "Vitex spp.", "Sambengo",
  "DoF NTFP map",
  "Local-name variants standardized; retain genus-level identification.", FALSE,

  "fara jambaba|fara jambabaa",
  "Piliostigma thonningii", "Fara Jambaba",
  "DoF tree map; working review decision",
  "Local-name identification retained.", FALSE,

  "fara jambamesseng|fara jambamesengo",
  "Piliostigma reticulatum", "Fara Jambamesengo",
  "DoF tree map; working review decision",
  "Retain the distinction between the two Fara names. DoF tree map.", FALSE,

  "yirindingkunango",
  "Azadirachta indica", "Yirinding kunango",
  "DoF tree map; Arena taxonomy",
  "Local and recorded scientific names identification.", FALSE,

  "kurulungo|krulung|kurulung",
  "Dichrostachys cinerea", "Kurulungo",
  "DoF tree map; Arena taxonomy",
  "Local-name spelling variants standardized.", FALSE,

  "sinjang",
  "Cassia sieberiana", "Sinjango",
  "DoF tree and NTFP maps",
  "Recorded Cacia Sibiriana corrected; local name standardized.", FALSE,

  "wulakono sunkungho|sunkugo",
  "Annona senegalensis", "Wulakono Sunkungo",
  "DoF NTFP map",
  "Local-name spelling variants standardized.", FALSE,

  "batio",
  "Nauclea latifolia", "Batio",
  "Arena taxonomy; DoF tree map",
  "Recorded Neuclar latifolia corrected.", FALSE,

  "juto",
  "Securidaca longipedunculata", "Juto",
  "2010 species list",
  "2010 local-name match.", FALSE,

  "tabo",
  "Cola cordifolia", "Tabo",
  "DoF tree map; 2010 species list",
  "Reviewed variant of Tabao/Taboo.", FALSE,

  "kewungho",
  "Trichilia emetica", "Kewungho",
  "DoF tree map",
  "Apply the documented local-name identification.", FALSE,

  "singoko|sinjoko",
  "Acacia macrostachya", "Singoko",
  "DoF tree map",
  "Local-name spelling variants standardized.", FALSE,

  "anncadium occidental cashew",
  "Anacardium occidentale", "Cashew",
  "Arena taxonomy; recorded name",
  "Scientific-name spelling and explicit common name identification.", FALSE,

  "burungbarango|burugbalango|burunbarangho|burunbararangho|burungbalago",
  "Trema guineensis", "Burumbarango",
  "DoF NTFP map; recorded scientific names",
  "Scientific and local-name spelling variants standardized.", FALSE,

  "baro",
  "Combretum micranthum", "Baro",
  "DoF NTFP map",
  "Reviewed variant of Barro; retain the recorded local name.", FALSE,

  "barrow",
  "Combretum micranthum", "Barrow",
  "DoF NTFP map",
  "Scientific name standardized; retain the recorded local name.", FALSE,

  "kinkinlinba",
  "Combretum micranthum", "Kinkinlinba",
  "DoF NTFP map",
  "Reviewed variant of Kinkinlin ba.", FALSE,

  "patekulay|paticulley",
  "Strychnos spinosa", "Patekulewo",
  "DoF NTFP map",
  "Reviewed local-name spelling variants standardized.", FALSE,

  "nyaninkoyo",
  "Maytenus senegalensis", "Nyaninkoyo",
  "DoF tree map",
  "Match to the documented cleaned local name.", FALSE,

  "vitex spp (kunjeh)|vitex spp (kunjeh )|kungeh",
  "Guiera sp.", "Kunjeh",
  "DoF tree and NTFP maps",
  "References support Guiera; correct the conflicting Vitex entry and retain genus-level identification.", FALSE,

  "duto",
  "Mangifera indica", "Duto",
  "DoF tree map",
  "Recorded Magifera corrected.", FALSE,

  "korongkondo|kronkondo|koronkondo",
  "Hymenocardia acida", "Kronkondo",
  "DoF tree map",
  "DoF tree identification adopted, local-name variants standardized.", FALSE,

  "kulugala|kulugalaho|kulugalango|kulukalang|kulungalaho",
  "Combretum nigricans", "Kulunkalango",
  "DoF tree and NTFP maps",
  "Local-name spelling variants standardized.", FALSE,

  "mbembo",
  "Lannea acida", "Bembo",
  "Reviewed reference match",
  "Local-name variant standardized.", FALSE,

  "nganingkoyo",
  "Acacia seyal", "Nganing-koyo",
  "Reviewed reference match",
  "Local-name spelling standardized.", FALSE,

  "sangkalama",
  "Albizia zygia", "Sangkalama",
  "Recorded scientific name",
  "Recorded Abizia zygia corrected.", FALSE,

  "gipangpango",
  "Calotropis procera", "Gipangpango",
  "Recorded scientific name; Kew POWO",
  "Recorded Caltropis crosera corrected.", FALSE,

  "kanifingo",
  "Vitex doniana", "Kutufingo",
  "Cleaning review",
  "Scientific identification resolved; local name standardized.", FALSE,

  "sulu tomborongho|sulu tomborongo",
  "Ziziphus jujuba", "Sulu Tomborongo",
  "Recent field identification",
  "Recent identification retained; local-name variants standardized.", FALSE,

  "wala|wallayero|walle yero|voganga africana walla yero",
  "Celtis toka", "Wala yiro",
  "DoF tree validation; Kew POWO",
  "DoF confirmed local identification adopted.", FALSE,

  "jabalulo|jambalulo",
  "Unknown", "Jambalulo",
  "Cleaning review",
  "Local-name spelling variants standardized.", TRUE,

  "konidindolo|kunenindolo|kunigding dolo|kunigdolo|kuninding dolo|kuningdindolo|unending dolo",
  "Unknown", "kuninding dolo",
  "Cleaning review",
  "Local-name spelling variants standardized. Verification will be requested from DoF.", TRUE
) %>%
  separate_rows(variants, sep = "\\|") %>%
  rename(local_name_key = variants) %>%
  mutate(local_name_key = str_to_lower(str_squish(local_name_key)))

tmp$regen_ntfp_name_lookup <- tribble(
  ~ntfp_label, ~ntfp_scientific_name, ~ntfp_local_name,

  "Guiera senegalensis/Mamakunkoyo",
  "Guiera senegalensis", "Mamakunkoyo",

  "Combretum glutinosum/Jambakatango",
  "Combretum glutinosum", "Jambakatango",

  "Combretum micranthum/Kinkiliba leaves",
  "Combretum micranthum", "Barro",

  "Vitex spp/Sambengo",
  "Vitex spp.", "Sambengo",

  "Guiera spp/Kunjeh",
  "Guiera sp.", "Kunjeh",

  "Landolphia heudelotii/Folay",
  "Landolphia heudelotii", "Folay",

  "Combretum nigricans/Kulunkalang jambo",
  "Combretum nigricans", "Kulunkalango",

  "Annona senegalensis/Wulakono Sunkungo",
  "Annona senegalensis", "Wulakono Sunkungo"
)

## Name decision for each unlisted and unknown record, new names in '<colname>_new'
tmp$regen_name_decisions <- tmp$regen_unlisted_name_review %>%
  mutate(
    local_name_key = str_to_lower(str_squish(regen_local_name)),
    ntfp_label = coalesce(
      na_if(str_squish(ntfp_match_from_local_field), ""),
      na_if(str_squish(ntfp_match_from_scientific_field), "")
    ),
    tree_map_scientific_name = na_if(str_squish(tree_map_scientific_name), ""),
    tree_map_local_name = na_if(str_squish(tree_map_local_name), "")
  ) %>%
  left_join(tmp$regen_name_review_map, by = "local_name_key", relationship = "many-to-one") %>%
  left_join(tmp$regen_ntfp_name_lookup, by = "ntfp_label", relationship = "many-to-one") %>%
  mutate(
    # Unknown/Unlisted in a reference does not identify a species
    tree_identification = case_when(
      str_to_lower(tree_map_scientific_name) %in% c("unknown", "unlisted") ~ NA_character_,
      TRUE ~ tree_map_scientific_name
    ),

    regen_species_scientific_name_new = coalesce(
      scientific_name_clean,
      tree_identification,
      ntfp_scientific_name,
      "Unknown"
    ),

    regen_local_name_new = coalesce(
      local_name_clean,
      if_else(!is.na(tree_identification), tree_map_local_name, NA_character_),
      ntfp_local_name,
      na_if(str_squish(regen_local_name), "")
    ),

    # Retain UNL only where a scientific identification is available
    # Recovered identifications previously coded UNK also become UNL
    regen_species_new = if_else(regen_species_scientific_name_new == "Unknown", "UNK", "UNL"),

    regen_name_source = case_when(
      !is.na(scientific_name_clean) ~ decision_source,
      !is.na(tree_identification) ~ "DoF tree species map",
      !is.na(ntfp_scientific_name) ~ "DoF NTFP name map",
      TRUE ~ "No supported scientific identification"
    ),

    regen_name_note = case_when(
      !is.na(scientific_name_clean) ~ decision_note,
      !is.na(tree_identification) ~ "Exact match to the verified tree species map.",
      !is.na(ntfp_scientific_name) ~
        "Exact NTFP reference match; scientific and local names separated for regeneration.",
      TRUE ~ "Scientific identification unresolved; set to UNK / Unknown."
    ),

    regen_name_verification_required = regen_species_scientific_name_new == "Unknown"
  ) %>%
  select(
    all_of(tmp$key_regen),
    regen_species_new,
    regen_species_scientific_name_new,
    regen_local_name_new,
    regen_name_source,
    regen_name_note,
    regen_name_verification_required
  )

regen <- data_raw$regen %>%
  left_join(tmp$regen_name_decisions, by = tmp$key_regen, relationship = "many-to-one") %>%
  mutate(
    regen_species_old = regen_species,
    regen_species = coalesce(regen_species_new, regen_species_old),

    regen_species_scientific_name_old = regen_species_scientific_name,
    regen_species_scientific_name = coalesce(regen_species_scientific_name_new, regen_species_scientific_name_old),

    regen_local_name_old = regen_local_name,
    regen_local_name = coalesce(regen_local_name_new, regen_local_name_old),

    regen_name_source = coalesce(regen_name_source, "Arena-listed species"),
    regen_name_verification_required = coalesce(regen_name_verification_required, FALSE)
  ) %>%
  select(-ends_with("_new"))

## ATTRIBUTE 10 & 11: SEEDLING AND SAPLING COUNTS

## Blank counts set to zero, unless both counts are blank
regen <- regen %>%
  mutate(
    seedling_count_old = seedling_count,
    sapling_count_old = sapling_count,

    seedling_count = if_else(
      is.na(seedling_count_old) & is.na(sapling_count_old),
      NA_real_,
      coalesce(seedling_count_old, 0)
    ),

    sapling_count = if_else(
      is.na(seedling_count_old) & is.na(sapling_count_old),
      NA_real_,
      coalesce(sapling_count_old, 0)
    ),

    regen_count_qa_issue = case_when(
      is.na(seedling_count_old) & is.na(sapling_count_old) ~
        "Both original counts blank; verification required.",
      seedling_count == 0 & sapling_count == 0 ~
        "No seedlings or saplings recorded; verification required.",
      TRUE ~ NA_character_
    )
  )

data_clean$regen <- regen



##
## Deadwood ######
##

## Source: scripts/06_Deadwood_data_check.R
## All checks passed, no correction needed: raw data used as is

data_clean$deadwood <- data_raw$deadwood
