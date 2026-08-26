
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


## Phase 2 base unit level: plot 
ph2_data <- plot_analysis |>
  mutate(
    PSU = cluster_no,
    SSU = plot_no,
    class_d = lulc_forest_label,
    access = plot_access_lgl,
    ssu_area = defaults$plot_size_dbh20,
    subpop = 1
  )

res_agb <- nfi_aggregate(
  .ph1_df = ph1_data, 
  .ph2_sp = ph2_data, 
  .class_d = lulc_forest_label, 
  .attr_y = plot_calc_biomass_ag, 
  .attr_x = ssu_area, 
  .aoi_area = defaults$country_area_nowater
  )
res_agb

res_agb$totals_d

sum(res_agb$totals_d$Xtot)
 