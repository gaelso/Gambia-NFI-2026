
## Required variables for the function:
## .ph1_df: Phase 1 plot level data frame, should contain the following columns: 
##    - PSU   : plot ID of the Phase 2 plots, 
##    - subpop : Subpopulation ID for the Phase 1 plots,  
##    - stratum: Stratum ID for the Phase 1 plots
## .ph2_sp: Phase 2 data frame at the smallest measurement area, subplot or land vegetation class. Should contain the following columns:
##    - PSU   : plot ID of the phase 2 plots, matching plot_no from the Phase 1 plot data frame, 
##    - SSU: Subplot ID (see .SSU),
##    - class_d   : domain of interest, for example forest classes.
##    - access    : accessibility of each subplot (TRUE/FALSE)
##    - An attribute x, at subplot level, denominator of the Ratio, R = Y / X (See .attr_x). 
##      It normally is the maximum measured area (in ha) at subplot level, i.e. 
##      largest nested measurement level for the attribute of interest,
##    - An attribute y, at subplot level, numerator of the Ratio, R = Y / X (See .attr_y). 
##      It is the sum of the entity that would be measured in the area mentioned in .attr_x 
##      in unit of reporting of entity (NOT per ha value). For example for AGB it should be tons.
## .class_d: domain values (in supblot table)
## .attr_y: attribute of interest (in entity table), numerator of the Ratio. Best use carbon pool name.
## .attr_x: attribute of interest (in subplot_table), denominator of the Ratio. Most often the largest measurement area at the subplot or subplot x lcs level.
## .aoi_area: total area of the area of interest (in ha). In case of NFIs, country area.

## Phase 1 stratification table
ph1_data <- ph1$cluster_harmo |> 
  select(ceo_cluster_no, ceo_id, type, ph1_stratum, cluster_no) |>
  mutate(
    PSU = cluster_no,
    subpop = 1,
    stratum = ph1_stratum
  )

## Phase 2 base unit level: plot 

plot_vars <- tree |>
  summarise(
    plot_tree_count = n(),
    plot_tree_density = sum(tree_weight),
    plot_basal_area = sum(tree_basal_area * tree_weight),
    plot_biomass_ag = sum(tree_biomass_ag * tree_weight),
    plot_biomass_total = sum(tree_biomass_total * tree_weight),
    .by = c(cluster_no, plot_no)
  )

plot_list <- categories$sampling_point_data |>
  filter(!is.na(level2_code)) |>
  select(cluster_no = level1_code, plot_no = level2_code, plot_center_x = location_x, plot_center_y = location_y, stratum = forest_type) |>
  mutate(subpop = 1)
  
plot_info <- plot |>
  select(cluster_no, plot_no, plot_access, plot_access_label)

plot_max_lu <- plot_lulc |> summarise(lulc_max = max(lulc_percentage), .by = c(cluster_no, plot_no))

plot_domain <- plot_lulc |>
  select(cluster_no, plot_no, lulc_name, lulc_name_label, lulc_forest, lulc_forest_label) |>
  mutate(
    lulc_forest = if_else(is.na(lulc_forest), lulc_name, lulc_forest),
    lulc_forest_label = if_else(is.na(lulc_forest_label), lulc_name_label, lulc_forest_label)
  )

ph2_data <- plot_list |>
  left_join(plot_info, by = join_by(cluster_no, plot_no)) |>
  left_join(plot_domain, by = join_by(cluster_no, plot_no)) |>
  left_join(plot_vars, by = join_by(cluster_no, plot_no)) |>
  mutate(
    PSU = cluster_no,
    SSU = plot_no,
    class_d = lulc_forest_label,
    access = if_else(plot_access %in% c(0, 1), TRUE, FALSE),
    ssu_area = defaults$plot_size_dbh20
  ) |>
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))


res_agb <- nfi_aggregate(.ph1_df = ph1_data, .ph2_sp = ph2_data, .class_d = lulc_forest_label, .attr_y = plot_biomass_ag, .attr_x = ssu_area, .aoi_area = defaults$country_area_nowater)
res_agb

sum(res_agb$totals_d$Xtot)
