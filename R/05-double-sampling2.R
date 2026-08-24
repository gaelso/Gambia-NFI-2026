## =====================================================================
## Two-phase (double sampling for stratification) estimation of
## DOMAIN areas -- forest types as observed in the FIELD -- from an NFI
## with clusters of subplots.
##
##   Phase 1 : systematic grid, every unit carries a stratum label.
##   Phase 2 : stratified subsample of phase-1 units, field-measured.
##             Each phase-2 unit is a CLUSTER of subplots; the domain
##             (class_d) is observed at SUBPLOT level and may disagree
##             with the phase-1 stratum of its parent plot.
##
## Principle: the phase-1 stratum builds the WEIGHTS only. The field
## class_d is a DOMAIN, not a stratum. Disagreement between the two is
## not an error to reconcile -- it is the information that corrects the
## phase-1 map areas. Misclassification costs precision, not bias.
##
## The CLUSTER (plot_id), not the subplot, is the phase-2 sampling unit.
## Everything is aggregated to cluster level before any variance is
## computed.
##
## Run the steps in order. Each one leaves a named object you can
## inspect before moving on.
## =====================================================================

library(dplyr)
library(tidyr)


## =====================================================================
## STEP 0 -- CONFIGURATION.  Edit only this block.
## =====================================================================

## --- the three input objects ------------------------------------------
## ph1_df   : ALL phase-1 units (including those subsampled in phase 2),
##            one row per unit. Columns: plot_id, subpop, stratum.
## ph2_sp   : phase-2 data at the smallest measurement area, one row per
##            subplot (or per land/vegetation class within a subplot).
##            Columns: plot_id, subplot_id, class_d, access, x, y.
## area_df  : known area of each subpopulation.
##            Columns: subpop, area_ha.   Set to NULL for proportions only.

ph1_df  <- ph1$cluster_harmo |>
  mutate(subpop = 1) |>
  select(plot_id = cluster_no, subpop, stratum = ph1_stratum)   # <-- your phase-1 table

ph2_sp  <- ph2_sp    # <-- your phase-2 table
area_df <- area_df   # <-- your subpopulation areas, or NULL

## --- column names in your tables --------------------------------------
col_plot_id    <- "plot_id"
col_subplot_id <- "subplot_id"
col_subpop     <- "subpop"
col_stratum    <- "stratum"
col_class_d    <- "class_d"
col_access     <- "access"
col_attr_x     <- "x"          # measured area (ha) at subplot level
col_attr_y     <- "y"          # entity total in that area (e.g. tons AGB)

## --- options ----------------------------------------------------------
## access_method:
##   "exclude" -- inaccessible subplots leave the sample entirely
##                (weighting-class adjustment). Domains sum to 1.
##                Assumes inaccessible land resembles accessible land in
##                the same stratum -- often false in steep or insecure
##                terrain, so choose deliberately.
##   "zero"    -- inaccessible subplots stay in the denominator but
##                belong to no domain. Domains sum to the accessible
##                fraction and the shortfall is explicit "not assessed".
access_method <- "exclude"

min_plots  <- 2      # min phase-2 clusters in a stratum for a variance
conf_level <- 0.95

z_crit <- qnorm(1 - (1 - conf_level) / 2)


## =====================================================================
## STEP 1 -- Standardise column names, so every later step is uniform.
## =====================================================================



ph1 <- ph1_df %>%
  transmute(
    plot_id = as.character(.data[[col_plot_id]]),
    subpop  = as.character(.data[[col_subpop]]),
    stratum = as.character(.data[[col_stratum]])
  )

sp <- ph2_sp %>%
  transmute(
    plot_id    = as.character(.data[[col_plot_id]]),
    subplot_id = as.character(.data[[col_subplot_id]]),
    class_d    = as.character(.data[[col_class_d]]),
    access     = as.logical(.data[[col_access]]),
    x          = as.numeric(.data[[col_attr_x]]),
    y          = as.numeric(.data[[col_attr_y]])
  ) %>%
  mutate(
    access = coalesce(access, FALSE),   # missing accessibility = not measured
    x      = coalesce(x, 0),
    y      = coalesce(y, 0)
  )

if (!is.null(area_df)) {
  areas <- area_df %>%
    transmute(subpop  = as.character(.data[[col_subpop]]),
              area_ha = as.numeric(area_ha))
} else {
  areas <- NULL
}


## =====================================================================
## STEP 2 -- Integrity checks. Stop early rather than produce a number
##           that is quietly wrong.
## =====================================================================

if (any(duplicated(ph1$plot_id)))
  stop("ph1_df must hold exactly one row per phase-1 plot; duplicates found.")

orphans <- setdiff(unique(sp$plot_id), ph1$plot_id)
if (length(orphans))
  stop("Phase-2 plots absent from ph1_df, so their weight is undefined: ",
       paste(head(orphans, 10), collapse = ", "))

if (!is.null(areas)) {
  missing_area <- setdiff(unique(ph1$subpop), areas$subpop)
  if (length(missing_area))
    stop("No area_ha supplied for subpop: ",
         paste(missing_area, collapse = ", "))
}


## =====================================================================
## STEP 3 -- Phase-1 weights, WITHIN each subpopulation.
##           W_h = n1_h / n1   (systematic grid => equal representation)
## =====================================================================

ph1_weights <- ph1 %>%
  count(subpop, stratum, name = "n1_h") %>%
  group_by(subpop) %>%
  mutate(n1_subpop = sum(n1_h),
         W_h       = n1_h / n1_subpop) %>%
  ungroup()

## Sanity: W_h must sum to 1 inside every subpop.
ph1_weights %>% group_by(subpop) %>% summarise(sum_W = sum(W_h))


## =====================================================================
## STEP 4 -- Apply the accessibility rule at subplot level.
##
##   den_sp   goes into the DENOMINATOR (total area the cluster
##            represents)
##   in_num   flags rows allowed into a domain NUMERATOR
## =====================================================================

sp_flagged <- sp %>%
  mutate(
    den_sp = if (access_method == "exclude") if_else(access, x, 0) else x,
    in_num = access & !is.na(class_d) & x > 0
  )


## =====================================================================
## STEP 5 -- Aggregate subplots up to the CLUSTER. This is the step that
##           makes the 5 subplots one sampling unit instead of five.
##           x_c = total measured area represented by cluster c.
## =====================================================================

cluster_x <- sp_flagged %>%
  group_by(plot_id) %>%
  summarise(x_c = sum(den_sp), .groups = "drop") %>%
  filter(x_c > 0)          # clusters with nothing usable leave the sample


## =====================================================================
## STEP 6 -- Aggregate subplots up to CLUSTER x DOMAIN.
##           a_dc = area of class d inside cluster c
##           y_dc = attribute total of class d inside cluster c
## =====================================================================

cluster_domain_raw <- sp_flagged %>%
  filter(in_num) %>%
  group_by(plot_id, class_d) %>%
  summarise(a_dc = sum(x),
            y_dc = sum(y),
            .groups = "drop")

domains <- sort(unique(cluster_domain_raw$class_d))
if (!length(domains))
  stop("No accessible, measured subplot carries a domain label.")


## =====================================================================
## STEP 7 -- Complete the grid. A cluster containing no subplot of class
##           d is a genuine ZERO, not a missing value. Dropping those
##           rows would inflate every domain estimate.
## =====================================================================

cluster_dom <- expand_grid(plot_id = cluster_x$plot_id,
                           class_d = domains) %>%
  left_join(cluster_domain_raw, by = c("plot_id", "class_d")) %>%
  mutate(a_dc = coalesce(a_dc, 0),
         y_dc = coalesce(y_dc, 0)) %>%
  left_join(cluster_x,   by = "plot_id") %>%
  left_join(ph1,         by = "plot_id") %>%
  left_join(ph1_weights, by = c("subpop", "stratum"))


## =====================================================================
## STEP 8 -- Stratum-level ratios and their variances.
##
##   r_a = sum(a_dc) / sum(x_c)   proportion of stratum area in class d
##   r_y = sum(y_dc) / sum(x_c)   attribute per unit of TOTAL area
##
## Variance is the linearised ratio variance built on CLUSTER residuals
## e_c = a_dc - r_a * x_c, which is what handles unequal numbers of
## measured subplots per cluster.
## =====================================================================

stratum_est <- cluster_dom %>%
  group_by(subpop, class_d, stratum) %>%
  summarise(
    n_h  = n(),
    X_h  = sum(x_c),
    xbar = mean(x_c),
    r_a  = sum(a_dc) / X_h,
    r_y  = sum(y_dc) / X_h,
    ## s2 / n_h  -- variance of the stratum ratio itself
    v_a  = if (n_h >= min_plots && xbar > 0)
      sum((a_dc - r_a * x_c)^2) / ((n_h - 1) * n_h * xbar^2)
    else NA_real_,
    v_y  = if (n_h >= min_plots && xbar > 0)
      sum((y_dc - r_y * x_c)^2) / ((n_h - 1) * n_h * xbar^2)
    else NA_real_,
    .groups = "drop"
  ) %>%
  left_join(ph1_weights, by = c("subpop", "stratum"))


## =====================================================================
## STEP 9 -- Combine strata within each subpopulation.
##
##   p_d = sum_h W_h * r_a          (domain proportion)
##   q_d = sum_h W_h * r_y          (attribute per unit total area)
##
## Variance has two parts (Cochran, double sampling):
##   within  = sum_h W_h^2 * v_h
##   between = (1/n1) * sum_h W_h * (r_h - p_d)^2
## The second part exists because W_h is itself estimated from phase 1.
##
## W_cov renormalises over strata that actually received phase-2
## clusters; anything uncovered is reported so it cannot pass silently.
## =====================================================================

subpop_est <- stratum_est %>%
  group_by(subpop, class_d) %>%
  summarise(
    n1_subpop     = first(n1_subpop),
    n_ph2_plots   = sum(n_h),
    W_cov         = sum(W_h),
    p_d           = sum(W_h * r_a) / W_cov,
    q_d           = sum(W_h * r_y) / W_cov,
    var_p_within  = sum((W_h / W_cov)^2 * v_a, na.rm = TRUE),
    var_p_between = sum((W_h / W_cov) * (r_a - p_d)^2) / n1_subpop,
    var_q_within  = sum((W_h / W_cov)^2 * v_y, na.rm = TRUE),
    var_q_between = sum((W_h / W_cov) * (r_y - q_d)^2) / n1_subpop,
    .groups = "drop"
  ) %>%
  mutate(
    var_p       = var_p_within + var_p_between,
    var_q       = var_q_within + var_q_between,
    se_p        = sqrt(pmax(var_p, 0)),
    se_q        = sqrt(pmax(var_q, 0)),
    w_uncovered = 1 - W_cov
  )


## =====================================================================
## STEP 10 -- Ratio WITHIN the domain: R_d = Y_d / A_d, e.g. tons per
##            hectare of forest type d (not per hectare of land).
##
## R_d is a ratio of two estimates, so its variance comes from a second
## pass over the clusters using e_c = y_dc - R_d * a_dc, NOT from
## dividing var_q by var_p.
## =====================================================================

ratio_point <- subpop_est %>%
  transmute(subpop, class_d,
            R_d = if_else(p_d > 0, q_d / p_d, NA_real_))

ratio_var <- cluster_dom %>%
  left_join(ratio_point, by = c("subpop", "class_d")) %>%
  filter(!is.na(R_d)) %>%
  mutate(e_c = y_dc - R_d * a_dc) %>%          # linearised residual
  group_by(subpop, class_d, stratum) %>%
  summarise(
    n_h  = n(),
    xbar = mean(x_c),
    r_e  = sum(e_c) / sum(x_c),
    v_e  = if (n_h >= min_plots && xbar > 0)
      sum((e_c - r_e * x_c)^2) / ((n_h - 1) * n_h * xbar^2)
    else NA_real_,
    .groups = "drop"
  ) %>%
  left_join(ph1_weights, by = c("subpop", "stratum")) %>%
  group_by(subpop, class_d) %>%
  summarise(
    W_cov  = sum(W_h),
    z_hat  = sum(W_h * r_e) / W_cov,
    var_z  = sum((W_h / W_cov)^2 * v_e, na.rm = TRUE) +
      sum((W_h / W_cov) * (r_e - z_hat)^2) / first(n1_subpop),
    .groups = "drop"
  )


## =====================================================================
## STEP 11 -- Attach subpopulation areas and assemble the result table.
## =====================================================================

n_present <- cluster_dom %>%
  group_by(subpop, class_d) %>%
  summarise(n_ph2_present = sum(a_dc > 0), .groups = "drop")

estimates <- subpop_est %>%
  left_join(ratio_point, by = c("subpop", "class_d")) %>%
  left_join(select(ratio_var, subpop, class_d, var_z),
            by = c("subpop", "class_d")) %>%
  left_join(n_present, by = c("subpop", "class_d")) %>%
  { if (is.null(areas)) mutate(., area_ha_subpop = NA_real_)
    else left_join(., rename(areas, area_ha_subpop = area_ha), by = "subpop") } %>%
  mutate(
    prop        = p_d,
    prop_se     = se_p,
    area_ha     = area_ha_subpop * p_d,
    area_se     = area_ha_subpop * se_p,
    area_lci    = area_ha - z_crit * area_se,
    area_uci    = area_ha + z_crit * area_se,
    area_cv_pct = 100 * se_p / p_d,
    total_y     = area_ha_subpop * q_d,
    total_y_se  = area_ha_subpop * se_q,
    ratio_y_per_ha    = R_d,
    ratio_y_per_ha_se = sqrt(pmax(var_z, 0)) / p_d,
    var_within_pct    = 100 * var_p_within / var_p
  ) %>%
  select(subpop, class_d, n1_subpop, n_ph2_plots, n_ph2_present,
         prop, prop_se, area_ha, area_se, area_lci, area_uci, area_cv_pct,
         total_y, total_y_se, ratio_y_per_ha, ratio_y_per_ha_se,
         var_within_pct, w_uncovered) %>%
  arrange(subpop, class_d)

## Sanity: with access_method = "exclude" the proportions must sum to 1
## in every subpop. With "zero" they sum to the accessible fraction.
estimates %>% group_by(subpop) %>% summarise(sum_prop = sum(prop))


## =====================================================================
## STEP 12 -- Roll subpopulations up. Treated as independent design
##            domains, so variances add.
## =====================================================================

if (!is.null(areas)) {
  national <- estimates %>%
    group_by(class_d) %>%
    summarise(
      area_ha    = sum(area_ha, na.rm = TRUE),
      area_var   = sum(area_se^2, na.rm = TRUE),
      total_y    = sum(total_y, na.rm = TRUE),
      total_y_var = sum(total_y_se^2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      area_se     = sqrt(area_var),
      area_lci    = area_ha - z_crit * area_se,
      area_uci    = area_ha + z_crit * area_se,
      area_cv_pct = 100 * area_se / area_ha,
      total_y_se  = sqrt(total_y_var),
      ratio_y_per_ha = if_else(area_ha > 0, total_y / area_ha, NA_real_)
    ) %>%
    select(-area_var, -total_y_var)
} else {
  national <- NULL
}


## =====================================================================
## STEP 13 -- Phase-1 stratum vs phase-2 field class. Report this table:
##            its off-diagonal is exactly what drives the correction,
##            and reviewers will ask for it.
## =====================================================================

confusion <- sp_flagged %>%
  filter(in_num) %>%
  left_join(ph1, by = "plot_id") %>%
  group_by(subpop, stratum, class_d) %>%
  summarise(n_subplots  = n(),
            measured_ha = sum(x),
            .groups = "drop") %>%
  group_by(subpop, stratum) %>%
  mutate(share = measured_ha / sum(measured_ha)) %>%
  ungroup()


## =====================================================================
## STEP 14 -- Diagnostics. Read these before believing any number above.
## =====================================================================

thin_strata <- stratum_est %>%
  filter(n_h < min_plots) %>%
  distinct(subpop, stratum, n_h)

if (nrow(thin_strata)) {
  message("Strata with fewer than ", min_plots,
          " phase-2 clusters -- no estimable variance. Collapse them ",
          "BEFORE estimation, not after:")
  print(thin_strata)
}

uncovered <- estimates %>%
  filter(!is.na(w_uncovered), w_uncovered > 1e-9) %>%
  distinct(subpop, w_uncovered)

if (nrow(uncovered)) {
  message("Phase-1 weight with no phase-2 cluster at all (weights were ",
          "renormalised; that share keeps the uncorrected map class):")
  print(uncovered)
}

if (access_method == "zero")
  message("access_method = 'zero': domain proportions sum to the ",
          "accessible fraction, not to 1.")

zero_cells <- stratum_est %>% filter(r_a == 0)
if (nrow(zero_cells))
  message(nrow(zero_cells), " stratum x domain cells have r = 0 and so ",
          "contribute zero variance. That is optimistic -- consider ",
          "collapsing those strata.")


## =====================================================================
## STEP 15 -- Results.
## =====================================================================

print(estimates,   n = Inf)
print(national,    n = Inf)
print(confusion,   n = Inf)
print(stratum_est, n = Inf)      # per-stratum r_h, v_h, weights


## =====================================================================
## OPTIONAL -- synthetic self-test. Run this block FIRST, before real
## data: it has a known truth, so you can confirm the script recovers it.
## Delete the `if (FALSE)` wrapper to execute.
## =====================================================================
if (FALSE) {
  
  set.seed(42)
  
  ph1_df <- tibble(
    plot_id = sprintf("P%04d", 1:2000),
    subpop  = rep(c("North", "South"), each = 1000),
    stratum = c(sample(c("S1", "S2", "S3"), 1000, TRUE, c(.5, .3, .2)),
                sample(c("S1", "S2", "S3"), 1000, TRUE, c(.2, .3, .5)))
  )
  
  ## 20 clusters per subpop x stratum, 5 subplots of 0.05 ha each
  truth <- c(S1 = 0.85, S2 = 0.45, S3 = 0.10)   # P(field Forest | stratum)
  
  ph2_sp <- ph1_df %>%
    group_by(subpop, stratum) %>%
    slice_sample(n = 20) %>%
    ungroup() %>%
    tidyr::uncount(5, .id = "subplot_id") %>%
    mutate(
      class_d = if_else(runif(n()) < truth[stratum], "Forest", "NonForest"),
      access  = runif(n()) > 0.06,
      x       = 0.05,
      y       = if_else(class_d == "Forest", rnorm(n(), 6, 1.5), 0)
    ) %>%
    select(plot_id, subplot_id, class_d, access, x, y)
  
  area_df <- tibble(subpop  = c("North", "South"),
                    area_ha = c(600000, 400000))
  
  ## Expected Forest proportion, North: .5*.85 + .3*.45 + .2*.10 = 0.5800
  ##                             South: .2*.85 + .3*.45 + .5*.10 = 0.3550
  ## => Forest area about 600000*0.58 + 400000*0.355 = 490,000 ha
  ## Now re-run STEP 1 onward and check the estimates land near that.
}