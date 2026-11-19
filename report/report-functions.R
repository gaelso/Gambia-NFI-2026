##
## Table formatting functions for the Quarto report ######
##

## Sourced by the setup chunk of 'Gambia-NFI-2026-report.qmd'. Tables (tab) and figures (fig) come from the
## analysis, see 'R/07-report-objects.R'.

library(knitr)
options(knitr.kable.NA = "–")

## Same ggplot theme as the analysis (themes apply at print time, not saved with the figures)
theme_set(theme_bw())


## Column labels for display
col_labels <- c(
  forest_type = "Forest type", region = "Region", n_pp = "Plots (n)", land_cover = "Land cover",
  area_ha = "Area (ha)", area_mep = "Area ME (%)", area_pct = "Share of country (%)",
  species = "Species", genus = "Genus", family = "Family", taxon_level = "Taxon level",
  density = "Density (trees/ha)", density_ha = "Density (trees/ha)", density_mep = "Density ME (%)",
  basal_area = "Basal area (m²/ha)", n_sections = "Sections (n)",
  rel_density = "Rel. density (%)", rel_frequency = "Rel. frequency (%)", rel_dominance = "Rel. dominance (%)", ivi = "IVI",
  H = "Shannon H'", H_me = "H' ME", H_mep = "H' ME (%)", richness = "Richness", evenness = "Evenness",
  tree_species_name = "Species", tree_family = "Family", tree_iucn_code = "IUCN", tree_iucn_category = "IUCN category",
  n_trees = "Trees (n)", n_plots = "Plots with record (n)", n_plots_total = "Accessible plots (n)", pct_plots = "Plots with record (%)",
  wildlife_type_label = "Wildlife type", wildlife_sign_label = "Type of sign",
  healthy_ha = "Healthy (trees/ha)", healthy_mep = "Healthy ME (%)", damaged_ha = "Damaged (trees/ha)", damaged_mep = "Damaged ME (%)",
  pct_healthy = "Healthy (%)", ratio_healthy_damaged = "Healthy / damaged",
  tree_condition_overall_label = "Condition", pct = "Share of trees (%)",
  disturbance_group = "Group", disturbance = "Disturbance",
  dbh_class = "DBH class (cm)", height_class = "Height class (m)",
  agb_ha = "AGB (t/ha)", agb_ha_mep = "AGB ME (%)", agb_tot = "Total AGB (t)", agb_tot_mep = "Total AGB ME (%)",
  hd_model = "Model", hd_nranp = "Random parameters", hd_aic = "AIC", hd_rmse = "RMSE (m)", hd_bias = "Bias (m)",
  hd_bias_ftype = "Max. forest type bias (m)", hd_n_curves = "Cluster curves (n)", hd_n_decreasing = "Decreasing curves (n)",
  regen_class = "Regeneration", regen_ha = "Density (ind./ha)", regen_mep = "Density ME (%)",
  ntfp_use = "Use category", area_ntfp_pct = "Area with NTFP (%)", area_ntfp_mep = "Area ME (%)", n_products = "Products (n)",
  ntfp_name_label = "Product", n_few = "Few (n)", n_moderate = "Moderate (n)", n_abundant = "Abundant (n)",
  tree_group = "Tree group",
  n_low = "Low (n)", n_medium = "Medium (n)", n_high = "High (n)", n_int_na = "Not recorded (n)",
  dw_decomposition = "Decomposition", stems_ha = "Stems (n/ha)", stems_mep = "Stems ME (%)",
  dw_volume_ha = "Volume (m³/ha)", dw_volume_mep = "Volume ME (%)",
  litter_class = "Litter depth", litter_pct = "Share of area (%)", litter_mep = "Share ME (%)",
  pool = "Carbon pool", carbon_ha = "Carbon (t C/ha)", carbon_ha_mep = "Carbon ME (%)",
  carbon_tot = "Total carbon (t C)", carbon_tot_mep = "Total ME (%)",
  wildlife_name = "Recorded name", n_records = "Records (n)",
  wildlife_sci_name = "Species", iucn_accepted_name = "IUCN accepted name", wildlife_iucn_code = "IUCN",
  wildlife_iucn_category = "IUCN category", rel_freq = "Relative frequency (%)",
  soil_attribute = "Attribute", soil_class = "Class", soil_pct = "Share of area (%)", soil_mep = "Share ME (%)"
)

## Reporting levels: names in tab, subsection titles and cross-reference label suffixes
level_info <- tibble(
  level  = c("country", "forest_type", "forest_type_region"),
  title  = c("Country level", "Forest type level", "Forest type and region level"),
  suffix = c("country", "ftype", "ftype-region")
)

## Format a table for display: remove the columns constant at the level and the columns in .drop,
## readable column names
fmt_tab <- function(.df, .level = NULL, .drop = NULL) {
  if (identical(.level, "country")) .df <- select(.df, -any_of(c("forest_type", "region")))
  if (identical(.level, "forest_type")) .df <- select(.df, -any_of("region"))
  .df <- .df |>
    select(-any_of(.drop)) |>
    mutate(across(where(is.factor), as.character))
  names(.df) <- coalesce(col_labels[names(.df)], names(.df))
  .df
}

## Print a table with caption and cross-reference label (chunk option results: asis)
## Simple table format: pandoc sets no column widths (pipe tables with long lines get fixed relative widths
## where numbers overflow narrow columns), so Word autofits the columns to their content.
print_tab <- function(.df, .caption, .label) {
  cat(kable(.df, format = "simple", format.args = list(big.mark = ",")), sep = "\n")
  cat("\n: ", .caption, " {#", .label, "}\n\n", sep = "")
}

## One subsection per reporting level with its table (chunk option results: asis).
## Labels: tbl-<id>-country, tbl-<id>-ftype, tbl-<id>-ftype-region
## Main text: country and forest type levels (default), annex: forest type and region level (print_annex())
## .split_ftype: TRUE for one table per forest type at forest type level, labels tbl-<id>-ftype-<forest type>
## .unnumbered : TRUE for unnumbered subsection headings (annexes)
print_levels <- function(.tabs, .id, .caption, .heading = "####", .drop = NULL,
                         .levels = c("country", "forest_type"), .split_ftype = FALSE, .unnumbered = FALSE) {
  level_info |>
    filter(level %in% .levels) |>
    pwalk(\(level, title, suffix) {
      cat(.heading, " ", title, if (.unnumbered) " {.unnumbered}", "\n\n", sep = "")
      if (level == "forest_type" && .split_ftype) {
        .df <- .tabs[[level]]
        walk(ftype_names(.df), \(ftype_name) {
          print_tab(
            fmt_tab(filter(.df, forest_type == ftype_name), level, c("forest_type", .drop)),
            .caption = paste0(.caption, ", ", ftype_name),
            .label = paste0("tbl-", .id, "-", suffix, "-", region_slug(ftype_name))
          )
        })
      } else {
        print_tab(
          fmt_tab(.tabs[[level]], level, .drop),
          .caption = paste0(.caption, ", ", tolower(title)),
          .label = paste0("tbl-", .id, "-", suffix)
        )
      }
    })
}

## Forest types of a forest type level table, in factor level order
ftype_names <- function(.df) {
  .df$forest_type |> factor() |> droplevels() |> levels()
}

## Cross-reference to the forest type tables of print_levels(.split_ftype = TRUE) for inline R code:
## `r ftype_ref(tab$b_ivi_species, "ivi-species")` -> "Tables 12 to 15"
ftype_ref <- function(.tabs, .id) {
  labels <- paste0("tbl-", .id, "-ftype-", region_slug(ftype_names(.tabs$forest_type)))
  if (length(labels) == 1) return(paste0("@", labels))
  paste0("Tables [-@", first(labels), "] to [-@", last(labels), "]")
}

## First .n rows of each domain by decreasing .var, for each level of a list of tables (e.g. top 10 by IVI)
top_n_levels <- function(.tabs, .n, .var = "ivi") {
  map(.tabs, \(x) slice_max(x, .data[[.var]], n = .n, by = any_of(c("forest_type", "region")), with_ties = FALSE))
}

## Regions of a forest type and region level table, in factor level order
## (character regions are kept in their order of appearance)
annex_regions <- function(.df) {
  if (!is.factor(.df$region)) return(unique(.df$region))
  .df$region |> droplevels() |> levels()
}

## Cross-reference label suffix of a region: "West Coast" -> "west-coast"
region_slug <- function(.region) str_replace_all(str_to_lower(.region), "[^a-z0-9]+", "-")

## Forest type and region level tables for the annex, one table per region, without level subsection
## (chunk option results: asis). Labels: tbl-<id>-ftype-region-<region>, e.g. tbl-area-ftype-region-west-coast
print_annex <- function(.tabs, .id, .caption, .drop = NULL) {
  .df <- .tabs$forest_type_region
  walk(annex_regions(.df), \(region_name) {
    print_tab(
      fmt_tab(filter(.df, region == region_name), "forest_type_region", c("region", .drop)),
      .caption = paste0(.caption, ", ", region_name, " region"),
      .label = paste0("tbl-", .id, "-ftype-region-", region_slug(region_name))
    )
  })
}

## Cross-reference to the annex tables of print_annex() for inline R code in the text:
## `r annex_ref(tab$a_area, "area")` -> "Tables 40 to 44"
annex_ref <- function(.tabs, .id) {
  labels <- paste0("tbl-", .id, "-ftype-region-", region_slug(annex_regions(.tabs$forest_type_region)))
  if (length(labels) == 1) return(paste0("@", labels))
  paste0("Tables [-@", first(labels), "] to [-@", last(labels), "]")
}
