
## Two-phase sampling (double sampling for stratification), common setup for the 05a-05i scripts.
## See nfi_aggregate() and nfi_aggregate2() in 'R/00-functions.R' for the inputs.

## Reporting levels, as domains of the two-phase estimator:
## - country: all land area of the country
## - forest_type: forest type of the plot LULC sections (Closed, Open, Mangrove forest, Non-forest)
## - forest_type_region: forest type x administrative region



##
## Phase 2 base units and domains ######
##

## Phase 2 base unit level: plot x LULC section
ph2_data <- plot_lulc_analysis |>
  mutate(
    PSU = cluster_no,
    SSU = paste(plot_no, lulc_id, sep = "_"),
    class_d = lulc_forest_label,
    access = plot_access_lgl,
    virtual = plot_virtual,
    ## Section area (ha): plot area x section share
    ssu_area = defaults$plot_size_dbh20 * lulc_share,
    subpop = 1,
    ## Section totals (t) from per-ha values of the full plot, so that trees < 20 cm measured in the
    ## 0.04 ha nested plot are expanded to the full 0.1 ha plot
    lulc_biomass_ag_tot = lulc_calc_biomass_ag * defaults$plot_size_dbh20,
    ## Domains
    forest_type = lulc_forest_label,
    region = plot_region,
    dom_country = "Gambia",
    dom_ftype = forest_type,
    dom_ftype_region = paste(forest_type, region, sep = " - ")
  )

vec_levels <- c(country = "dom_country", forest_type = "dom_ftype", forest_type_region = "dom_ftype_region")

## Display order
defaults$ftype_levels  <- c("All", "Closed Forest", "Open Forest", "Mangrove Forest", "Non-forest")
defaults$region_levels <- c("All", "West Coast", "North Bank", "Lower River", "Central River", "Upper River")
defaults$pal_ftype     <- c("Closed Forest" = "#1b5e20", "Open Forest" = "#8bc34a", "Mangrove Forest" = "#00838f", "Non-forest" = "#bdbdbd")

## Forest type and region of each domain, to add to nfi_aggregate_levels() outputs
domain_lookup <- bind_rows(
  ph2_data |> distinct(domain = dom_country) |> mutate(level = "country", forest_type = "All", region = "All"),
  ph2_data |> distinct(domain = dom_ftype, forest_type) |> mutate(level = "forest_type", region = "All"),
  ph2_data |> distinct(domain = dom_ftype_region, forest_type, region) |> mutate(level = "forest_type_region")
) |>
  mutate(
    forest_type = factor(forest_type, levels = defaults$ftype_levels),
    region = factor(region, levels = defaults$region_levels)
  )

## Field plots for plot counts (05c, 05d, 05f): accessible plots, forest type of the main LULC section
plot_info <- plot_analysis |>
  filter(plot_access_lgl, !plot_virtual) |>
  select(cluster_no, plot_no, forest_type = lulc_forest_label, region = plot_region)



##
## Aboveground biomass, check of the aggregation functions ######
##

res_agb <- nfi_aggregate(
  .ph1_df = ph1_data,
  .ph2_sp = ph2_data,
  .class_d = lulc_forest_label,
  .attr_y = lulc_biomass_ag_tot,
  .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater
  )
res_agb$totals_d

## Areas sum to the country area
all.equal(sum(res_agb$totals_d$Xtot), defaults$country_area_nowater)

## Same results with nfi_aggregate2() without sub-domains
res_agb2 <- nfi_aggregate2(
  .ph1_df = ph1_data,
  .ph2_sp = ph2_data,
  .class_d = lulc_forest_label,
  .attr_y = lulc_biomass_ag_tot,
  .attr_x = ssu_area,
  .aoi_area = defaults$country_area_nowater
)
all.equal(res_agb$totals_d, res_agb2$totals_d)



##
## Outputs ######
##

## Tables and figures of the 05a-05i scripts, also saved in results/
if (!exists("tab")) tab <- list()
if (!exists("fig")) fig <- list()

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)
