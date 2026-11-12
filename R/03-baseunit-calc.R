

## Aggregate measures ####
plot_vars <- tree |>
  summarise(
    plot_calc_tree_count = n(),
    plot_calc_basal_area_sum = sum(tree_basal_area),
    plot_calc_biomass_ag_sum = sum(tree_biomass_ag),
    plot_calc_biomass_total_sum = sum(tree_biomass_total),
    plot_calc_tree_density = sum(tree_weight),
    plot_calc_basal_area = sum(tree_basal_area * tree_weight),
    plot_calc_biomass_ag = sum(tree_biomass_ag * tree_weight),
    plot_calc_biomass_total = sum(tree_biomass_total * tree_weight),
    .by = c(cluster_no, plot_no)
  )

## Get full list of expected field plots ####
plot_list <- anci$sampling_point |>
  filter(!is.na(level2_code)) |>
  select(
    cluster_no = level1_code,
    plot_no = level2_code,
    plot_center_x = location_x,
    plot_center_y = location_y,
    stratum = forest_type
  ) |>
  mutate(across(c(cluster_no, plot_no), as.numeric))



## Retrieve accessibility ####
cluster_access <- cluster |> select(cluster_no, cluster_access, cluster_access_label, cluster_info_stratum, cluster_info_region_label)
plot_access    <- plot |> select(cluster_no, plot_no, plot_access, plot_access_label)

## Assign dominant land use to plot #### 
plot_domain <- plot_lulc |>
  select(cluster_no, plot_no, lulc_name, lulc_name_label, lulc_forest, lulc_forest_label, lulc_percentage) |>
  mutate(
    lulc_forest = if_else(is.na(lulc_forest), 99, lulc_forest),
    lulc_forest_label = if_else(is.na(lulc_forest_label), "Non-forest", lulc_forest_label)
    # lulc_forest = if_else(is.na(lulc_forest), lulc_name, lulc_forest),
    # lulc_forest_label = if_else(is.na(lulc_forest_label), lulc_name_label, lulc_forest_label)
  ) |>
  arrange(cluster_no, plot_no, desc(lulc_percentage), lulc_forest) |>
  distinct(cluster_no, plot_no, .keep_all = TRUE)

## Check unique cluster/plot with majo LU
plot_domain |> distinct(cluster_no, plot_no) |> nrow()
nrow(plot_domain)

## Combine all ####
plot_analysis_init <- plot_list |>
  left_join(cluster_access, by = join_by(cluster_no)) |>
  left_join(plot_access, by = join_by(cluster_no, plot_no)) |>
  left_join(plot_domain, by = join_by(cluster_no, plot_no)) |>
  left_join(plot_vars, by = join_by(cluster_no, plot_no)) |>
  arrange(cluster_no, plot_no) |>
  mutate(
    plot_id = paste0(case_when(
      cluster_no < 10 ~ "00",
      cluster_no < 100 ~ "0",
      TRUE ~ ""
      ), cluster_no, "_",plot_no)) |>
  select(plot_id, everything())

unique(plot_analysis_init$plot_id)
names(plot_analysis_init)

## APPLY CORRECTIONS ####
plot_analysis <- plot_analysis_init |>
  mutate(
    ## Bring cluster level accessibility if plot level missing
    plot_access = if_else(is.na(plot_access), cluster_access, plot_access),
    plot_access_label = if_else(is.na(plot_access_label), cluster_access_label, plot_access_label),
    plot_access_lgl = if_else(plot_access %in% c(0,1), TRUE, FALSE),
    ## Add non-forest to all NAs in forest LULC 
    lulc_forest = if_else(is.na(lulc_forest), 99, lulc_forest),
    lulc_forest_label = if_else(is.na(lulc_forest_label), "Non-forest", lulc_forest_label)
    ) |>
  mutate(
    ## Replace calculated measures NAs with 0
    across(starts_with("plot_calc_"), \(x) replace_na(x, 0))
    )



##
## Add phase 1 non-forest points as virtual plots ######
##

## The non-forest stratum has no phase 2 sample. Its phase 1 points are added as accessible plots,
## measured as non-forest with no trees (0 biomass), so that the stratum area goes to the Non-forest
## domain and domain areas sum to the country area.
## - one virtual plot per phase 1 point, cluster_no = 1000 + ceo_cluster_no (no overlap with field clusters)
## - plot_virtual flags these plots, to exclude them from phase 2 only analysis (04)

tmp$plot_virtual <- ph1$cluster_harmo |>
  filter(ph1_stratum == "nonforest", is.na(cluster_no)) |>
  mutate(
    cluster_no = 1000 + ceo_cluster_no,
    plot_no = 1,
    plot_id = paste0(cluster_no, "_", plot_no),
    plot_center_x = center_lon,
    plot_center_y = center_lat,
    stratum = ph1_stratum,
    plot_access_label = "Not visited - phase 1 non-forest",
    plot_access_lgl = TRUE,
    lulc_forest = 99,
    lulc_forest_label = "Non-forest",
    lulc_percentage = 100,
    plot_virtual = TRUE
  ) |>
  select(plot_id, cluster_no, plot_no, plot_center_x, plot_center_y, stratum, plot_access_label,
         plot_access_lgl, lulc_forest, lulc_forest_label, lulc_percentage, plot_virtual)

plot_analysis <- plot_analysis |>
  mutate(plot_virtual = FALSE) |>
  bind_rows(tmp$plot_virtual) |>
  mutate(across(starts_with("plot_calc_"), \(x) replace_na(x, 0)))

## Same IDs in the phase 1 table
ph1_data <- ph1_data |>
  mutate(PSU = if_else(stratum == "nonforest" & is.na(cluster_no), 1000 + ceo_cluster_no, PSU))

## Checks: one virtual plot per non-forest phase 1 point
nrow(tmp$plot_virtual) == sum(ph1_data$stratum == "nonforest")
table(plot_analysis$stratum, plot_analysis$plot_virtual)



##
## Add region to plots ######
##

## Region from the plot centre location, for all plots including virtual plots: field region labels are
## missing for virtual plots, and agree with the region map for 98% of field plots.
## Region names as in the field form, Greater Banjul (3 plots) is included in West Coast.
tmp$region_names <- c(
  "Western Region"       = "West Coast",
  "North Bank Region"    = "North Bank",
  "Lower River Region"   = "Lower River",
  "Central River Region" = "Central River",
  "Upper River Region"   = "Upper River"
)

tmp$plot_region <- plot_analysis |>
  select(cluster_no, plot_no, plot_center_x, plot_center_y) |>
  st_as_sf(coords = c("plot_center_x", "plot_center_y"), crs = 4326) |>
  st_join(select(anci$sf_regions, REGION), join = st_nearest_feature) |>
  as_tibble() |>
  mutate(plot_region = unname(tmp$region_names[REGION])) |>
  select(cluster_no, plot_no, plot_region)

plot_analysis <- plot_analysis |>
  mutate(plot_region = NA) |>
  left_join(tmp$plot_region, by = join_by(cluster_no, plot_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))

## Checks: all plots with a region, field vs map region
table(plot_analysis$plot_region, useNA = "ifany")
table(plot_analysis$cluster_info_region_label, plot_analysis$plot_region, useNA = "ifany")



##
## Plot x LULC section base units ######
##

## Plots split by LULC section, for domain estimates where plots overlap several forest types
## - section area share normalised to the plot (some percentages do not sum to 100%)
## - plots without LULC record (inaccessible, or accessible with no record) as one non-forest section
## - per ha values relative to the full plot area: section total = value * plot area

lulc_vars <- tree |>
  summarise(
    lulc_calc_tree_count = n(),
    lulc_calc_tree_density = sum(tree_weight),
    lulc_calc_basal_area = sum(tree_basal_area * tree_weight),
    lulc_calc_biomass_ag = sum(tree_biomass_ag * tree_weight),
    lulc_calc_biomass_total = sum(tree_biomass_total * tree_weight),
    .by = c(cluster_no, plot_no, tree_lulc_no)
  ) |>
  rename(lulc_id = tree_lulc_no)

plot_lulc_analysis <- plot_analysis |>
  select(plot_id, cluster_no, plot_no, stratum, cluster_info_stratum, plot_region, plot_virtual, plot_access, plot_access_label, plot_access_lgl) |>
  left_join(
    plot_lulc |> select(cluster_no, plot_no, lulc_id, lulc_name, lulc_name_label, lulc_forest, lulc_forest_label, lulc_percentage),
    by = join_by(cluster_no, plot_no)
  ) |>
  mutate(
    lulc_id = replace_na(lulc_id, 1),
    lulc_percentage = replace_na(lulc_percentage, 100),
    lulc_forest = replace_na(lulc_forest, 99),
    lulc_forest_label = replace_na(lulc_forest_label, "Non-forest"),
    lulc_share = lulc_percentage / sum(lulc_percentage),
    .by = c(cluster_no, plot_no)
  ) |>
  left_join(lulc_vars, by = join_by(cluster_no, plot_no, lulc_id)) |>
  mutate(across(starts_with("lulc_calc_"), \(x) replace_na(x, 0)))

## Checks: one share sum per plot, all trees allocated to a section
plot_lulc_analysis |> summarise(s = sum(lulc_share), .by = c(cluster_no, plot_no)) |> filter(abs(s - 1) > 1e-9)
sum(plot_lulc_analysis$lulc_calc_tree_count) == nrow(tree)


## List of plot_id that should go to non-forest (accessible forest with no trees)
plot_notree <- plot_analysis |> filter(plot_access_lgl, !plot_virtual, lulc_forest_label != "Non-forest", plot_calc_tree_count == 0)
table(plot_notree$lulc_forest_label)
vec_plot_notree_close_forest <- plot_notree |>filter(lulc_forest == 2) |> pull(plot_id)



##
## Join info to 'tree' ######
##

names(plot_analysis)

## All plot_analysis columns set to NA first, so the plot_analysis version replaces any existing column in tree
tmp$plot_analysis_cols <- setdiff(names(plot_analysis), c("cluster_no", "plot_no"))

tree <- tree |>
  mutate(!!!set_names(rep(list(NA), length(tmp$plot_analysis_cols)), tmp$plot_analysis_cols)) |>
  left_join(plot_analysis, by = join_by(cluster_no, plot_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))
 
names(plot_analysis)

