



## This function is part of a series of aggregation functions for NFI analysis
## function 3: double sampling for stratification with ratio estimator

## This function starts at the smallest area of measurement. In cases where land cover differs within subplot,
## subplot refers to the land cover sections.

## Users are expected to aggregate entities to this subplot level as the minimum area where all entities can be combined


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


nfi_aggregate <- function(.ph1_df, .ph2_sp, .class_d, .attr_y, .attr_x, .aoi_area){
  
  ## !!! FOR TESTING ONLY
  # .ph1_df = ph1_data 
  # .ph2_sp = ph2_data
  # .class_d = quo(lulc_forest_label)
  # .attr_y = quo(plot_biomass_ag)
  # .attr_x = quo(ssu_area)
  # .aoi_area = defaults$country_area_nowater
  ## !!!
  
  
  ## Setup ####
  
  ## + Defuse input column name ####
  class_d <- enquo(.class_d)
  attr_y  <- enquo(.attr_y)
  attr_x  <- enquo(.attr_x)
  
  ## + Prepare constants ####
  Ntot  <- nrow(.ph1_df)
  Nstrat <- length(unique(.ph2_sp$stratum))
  
  vec_subpop  <- .ph2_sp |> pull(subpop) |> unique() |> sort()
  ## All phase 1 strata, including strata without phase 2 sample: they keep their weight Wh with zero values,
  ## so that strata weights sum to 1 and totals cover the whole area of interest
  vec_stratum <- c(pull(.ph1_df, stratum), pull(.ph2_sp, stratum)) |> unique() |> sort()
  vec_d       <- .ph2_sp |> pull(!!class_d) |> unique() |> sort()
  
  
  ## Aggregate subplot to plot ####
  
  plot_a <- .ph2_sp |>
    filter(access) |>
    group_by(PSU, subpop, stratum) |>
    summarise(
      ai = sum(!!attr_x), 
      .groups = "drop"
    ) 
  
  plot_attr <- .ph2_sp |>
    filter(access) |>
    group_by(PSU, subpop, stratum, !!class_d) |>
    summarise(
      count_sp = n(),
      yid = sum(!!attr_y),
      xid = sum(!!attr_x),
      .groups = "drop"
    ) |> 
    left_join(plot_a, by = join_by(PSU, subpop, stratum)) |>
    mutate(attr = as_label(attr_y)) |>
    select(attr, everything())
  
  
  ## Aggregate plot to subpop x stratum ####
  
  ## + Prepare strata weight and areas ####
  ss_ph1_totals <- .ph1_df |>
    group_by(subpop) |>
    summarise(N = n(), .groups = "drop")
  
  ss_ph1 <- .ph1_df |>
    group_by(subpop, stratum) |>
    summarise(Nh = n(), .groups = "drop") |>
    left_join(ss_ph1_totals, by = join_by(subpop)) |>
    mutate(
      Wh = Nh/N,
      Ah = .aoi_area * Nh / Ntot ## divided by overall total number of plot in Phase 1
    )
  
  ss_ph2_totals <- plot_attr |>
    distinct(PSU, subpop, stratum) |>
    group_by(subpop) |>
    summarise(n = n(), .groups = "drop")
  
  ss_ph2 <- plot_attr |>
    distinct(PSU, subpop, stratum) |>
    group_by(subpop, stratum) |>
    summarise(nh = n(), .groups = "drop") |>
    left_join(ss_ph2_totals, by = join_by(subpop)) |>
    mutate(wh = nh/n) |>
    select(-n)
  
  ## + Initiate subpop x stratum ####
  subpop_stratum_init <- expand_grid(
    !!class_d := vec_d,
    subpop = vec_subpop,
    stratum = vec_stratum,
  ) 
  
  ## + Aggregate attributes ####
  subpop_stratum_a <- plot_a |>
    group_by(subpop, stratum) |>
    summarise(
      sum_ai    = sum(ai),
      sum_ai_sq = sum(ai^2),
      .groups = "drop"
    )
  
  subpop_stratum_attr <- plot_attr |>
    group_by(subpop, stratum, !!class_d) |>
    summarise(
      count_plot_h = n(),
      count_sp     = sum(count_sp),
      sum_yid      = sum(yid),
      sum_yid_sq   = sum(yid^2),
      sum_yid_ai   = sum(yid * ai),
      sum_xid      = sum(xid),
      sum_xid_sq   = sum(xid^2),
      sum_xid_ai   = sum(xid * ai),
      sum_yid_xid  = sum(yid * xid),
      .groups = "drop"
    ) 
  
  ## + Combine all ####
  subpop_stratum_d <- subpop_stratum_init |>
    left_join(ss_ph1, by = join_by(subpop, stratum)) |>
    left_join(ss_ph2, by = join_by(subpop, stratum)) |>
    left_join(ss_ph2_totals, by = join_by(subpop)) |>
    left_join(subpop_stratum_attr, by = join_by(subpop, stratum, !!class_d)) |>
    left_join(subpop_stratum_a, by = join_by(subpop, stratum)) |>
    mutate(
      mean_yd = if_else(sum_ai > 0, sum_yid / sum_ai, 0),
      mean_xd = if_else(sum_ai > 0, sum_xid / sum_ai, 0)
    ) |>
    mutate(across(where(is.numeric), \(x) replace_na(x, 0))) |>
    arrange(!!class_d, subpop, stratum)
  
  ## + Add sub-population mean ####
  subpop_mean_d <- subpop_stratum_d |>
    group_by(!!class_d, subpop) |>
    summarise(
      subpop_mean_yd = sum(mean_yd * Wh),
      subpop_mean_xd = sum(mean_xd * Wh),
      .groups = "drop"
    )
  
  ## + Calc variance ####
  subpop_stratum_d <- subpop_stratum_d |>
    mutate(
      subpop_mean_yd = NA,
      subpop_mean_xd = NA
    ) |>
    left_join(subpop_mean_d, by = join_by(!!class_d, subpop), suffix = c("_rm", "")) |>
    select(-ends_with("_rm")) |>
    mutate(
      var_yd          = if_else(nh > 1 & sum_ai > 0, nh^2/(nh - 1) * (sum_yid_sq - 2*mean_yd*sum_yid_ai + mean_yd^2*sum_ai_sq) / (sum_ai^2), 0),
      var_xd          = if_else(nh > 1 & sum_ai > 0, nh^2/(nh - 1) * (sum_xid_sq - 2*mean_xd*sum_xid_ai + mean_xd^2*sum_ai_sq) / (sum_ai^2), 0),
      covar_xdyd      = if_else(nh > 1 & sum_ai > 0, nh^2/(nh - 1) * (sum_yid_xid - mean_yd*sum_xid_ai - mean_xd*sum_yid_ai + mean_yd*mean_xd*sum_ai_sq) / (sum_ai^2), 0),
      ## Within-stratum term only for sampled strata, between-strata term (phase 1 error) for all strata
      var_mean_yd     = if_else(Nh > 1 & n > 0, Wh * (Nh - 1) / (N - 1) * if_else(nh > 0, var_yd / nh, 0) + Wh / (N - 1) * (mean_yd - subpop_mean_yd)^2, 0),
      var_mean_xd     = if_else(Nh > 1 & n > 0, Wh * (Nh - 1) / (N - 1) * if_else(nh > 0, var_xd / nh, 0) + Wh / (N - 1) * (mean_xd - subpop_mean_xd)^2, 0),
      covar_mean_xdyd = if_else(Nh > 1 & n > 0, Wh * (Nh - 1) / (N - 1) * if_else(nh > 0, covar_xdyd / nh, 0) + Wh / (N - 1) * (mean_xd - subpop_mean_xd) * (mean_yd - subpop_mean_yd), 0)
    ) |>
    mutate(attr = as_label(attr_y)) |>
    select(attr, everything())
  
  ## Aggregate to sub-populations ####
  subpop_d <- subpop_stratum_d |>
    group_by(!!class_d, subpop) |>
    summarise(
      count_plot     = sum(count_plot_h),
      subpop_mean_yd = sum(mean_yd * Wh),
      subpop_mean_xd = sum(mean_xd * Wh),
      subpop_var_yd  = sum(var_mean_yd),
      subpop_var_xd  = sum(var_mean_xd),
      subpop_covar_xdyd = sum(covar_mean_xdyd),
      subpop_n       = sum(nh),
      subpop_N       = sum(Nh),
      subpop_A       = sum(Ah),
      .groups = "drop"
    ) |>
    mutate(
      subpop_W      = subpop_N / Ntot,
      subpop_Rd     = if_else(subpop_mean_xd > 0, subpop_mean_yd / subpop_mean_xd, 0),
      subpop_Rd_var = if_else(subpop_mean_xd > 0, abs((subpop_var_yd + subpop_Rd^2*subpop_var_xd - 2*subpop_Rd*subpop_covar_xdyd) / (subpop_mean_xd^2)), 0),
      subpop_Rd_se  = sqrt(subpop_Rd_var),
      subpop_Rd_me  = qt(1-0.1/2, df = subpop_n - Nstrat) * subpop_Rd_se,
      subpop_Rd_mep = if_else(subpop_Rd > 0, round(subpop_Rd_me / subpop_Rd * 100, 2), 0)
    ) |>
    mutate(attr = as_label(attr_y)) |>
    select(attr, everything())
  
  
  ## Totals ####
  DF_all <- sum(unique(subpop_d$subpop_n)) - length(unique(subpop_d$subpop_n))*Nstrat
  
  totals_d <- subpop_d |>
    group_by(!!class_d) |>
    summarise(
      total_plot = sum(count_plot),
      Yd         = sum(subpop_mean_yd * subpop_W),
      Yd_var     = sum(subpop_var_yd * subpop_W^2),
      Xd         = sum(subpop_mean_xd * subpop_W),
      Xd_var     = sum(subpop_var_xd * subpop_W^2),
      covar_XdYd = sum(subpop_covar_xdyd * subpop_W^2),
      Ytot       = sum(subpop_mean_yd * subpop_A),
      Xtot       = sum(subpop_mean_xd * subpop_A),
      .groups = "drop"
    ) |>
    mutate(
      Yd_mep = if_else(Yd != 0, round(qt(1-0.1/2, df = DF_all) * sqrt(Yd_var) / Yd * 100, 2), 0),
      Xd_mep = if_else(Xd != 0, round(qt(1-0.1/2, df = DF_all) * sqrt(Xd_var) / Xd * 100, 2), 0),
      Rd     = round(Yd / Xd, 3),
      Rd_var = (Yd_var + Rd^2*Xd_var - 2*Rd*covar_XdYd)/Xd^2,
      ## Margin of Error = half width of confidence interval, mep = percentage margin of error
      Rd_mep = if_else(Rd != 0, round(qt(1-0.1/2, df = DF_all) * sqrt(Rd_var) / Rd * 100, 2), 0)
    ) |>
    select(!!class_d, n_pp = total_plot, Yd, Yd_mep, Xd, Xd_mep, Rd, Rd_mep, Ytot, Xtot) |>
    mutate(attr = as_label(attr_y)) |>
    select(attr, everything())
  totals_d
  
  totals_short <- totals_d  |> 
    mutate(attr = as_label(attr_y)) |>
    select(!!class_d, attr, n_pp, Rd, Rd_mep)
  
  out <- list(
    plot = plot_attr, 
    subpop_stratum = subpop_stratum_d, 
    subpop_d = subpop_d,
    totals_d = totals_d,
    totals_short = totals_short
  )
  
  out
  
}



## Function 4: double sampling for stratification with ratio estimator, with sub-domains and Shannon index ####

## Same estimator as nfi_aggregate(), with two additions:
## - Sub-domains: the attribute y is split by a class that is not an area class, for example diameter class
##   or species. y comes at SSU x sub-class level (.ph2_sub) while x stays at SSU level (.ph2_sp), so that
##   R_dc = Y_dc / X_d is the per ha value of sub-class c in domain d (e.g. trees/ha per diameter class)
##   and sub-class values add up to the domain value. Sub-classes absent from an SSU count as 0.
## - Shannon index: H_d = - sum_c p_dc ln(p_dc), with p_dc = Y_dc / sum_c Y_dc the share of sub-class c
##   (e.g. species) in domain d, from the domain estimates (one index per domain, not a mean of plot indices).
##   Variance by Taylor linearization: each plot gets z_id = sum_c b_dc y_idc, with
##   b_dc = -(ln(p_dc) + H_d) / sum_c Y_dc, and v(H_d) = v(Z_d) with the same two-phase variance as Y_d.

## Arguments: same as nfi_aggregate(), plus:
## .ph2_sub  : (optional) entities aggregated at SSU x sub-class level, with columns PSU and SSU (matching .ph2_sp),
##             the sub-class (.class_sub) and the attribute y (.attr_y). Only accessible SSUs of .ph2_sp are used.
##             If NULL, y is taken from .ph2_sp and the results are the same as nfi_aggregate().
## .class_sub: sub-class column in .ph2_sub
## .index    : "ratio" (default) for per ha values by domain (and sub-class),
##             "shannon" to add the Shannon index by domain, based on the sub-classes (output shannon_d)
## .alpha    : 1 - confidence level, default 0.1 for 90% confidence intervals

nfi_aggregate2 <- function(.ph1_df, .ph2_sp, .class_d, .attr_y, .attr_x, .aoi_area,
                           .ph2_sub = NULL, .class_sub = NULL, .index = c("ratio", "shannon"), .alpha = 0.1){

  ## Setup ####

  ## + Defuse input column names ####
  class_d   <- enquo(.class_d)
  class_sub <- enquo(.class_sub)
  attr_y    <- enquo(.attr_y)
  attr_x    <- enquo(.attr_x)
  .index    <- match.arg(.index)
  has_sub   <- !is.null(.ph2_sub)

  if (has_sub & rlang::quo_is_null(class_sub)) stop(".class_sub is required with .ph2_sub")
  if (.index == "shannon" & !has_sub) stop("The Shannon index requires .ph2_sub and .class_sub")

  ## + Prepare constants ####
  Ntot   <- nrow(.ph1_df)
  Nstrat <- length(unique(.ph2_sp$stratum))
  t_crit <- \(df) qt(1 - .alpha/2, df = df)

  vec_subpop  <- .ph2_sp |> pull(subpop) |> unique() |> sort()
  ## All phase 1 strata, including strata without phase 2 sample (see nfi_aggregate())
  vec_stratum <- c(pull(.ph1_df, stratum), pull(.ph2_sp, stratum)) |> unique() |> sort()
  vec_d       <- .ph2_sp |> pull(!!class_d) |> unique() |> sort()

  ## + Rename outputs to input column names ####
  rename_out <- function(.df){
    .df <- .df |> rename(!!rlang::as_name(class_d) := dom)
    if (!"sub" %in% names(.df)) return(.df)
    if (has_sub) rename(.df, !!rlang::as_name(class_sub) := sub) else select(.df, -sub)
  }


  ## SSU data ####

  ssu <- .ph2_sp |>
    filter(access) |>
    select(PSU, SSU, subpop, stratum, dom = !!class_d, x = !!attr_x)

  if (has_sub) {
    n_nomatch <- .ph2_sub |> anti_join(.ph2_sp, by = join_by(PSU, SSU)) |> nrow()
    if (n_nomatch > 0) warning(n_nomatch, " rows of .ph2_sub have no matching SSU in .ph2_sp and are ignored")

    ssu_y <- .ph2_sub |>
      select(PSU, SSU, sub = !!class_sub, y = !!attr_y) |>
      inner_join(select(ssu, PSU, SSU, subpop, stratum, dom), by = join_by(PSU, SSU))
  } else {
    ssu_y <- .ph2_sp |>
      filter(access) |>
      select(PSU, SSU, subpop, stratum, dom = !!class_d, y = !!attr_y) |>
      mutate(sub = "all")
  }

  vec_sub <- ssu_y |> pull(sub) |> unique() |> sort(na.last = TRUE)


  ## Aggregate SSU to plot (PSU) ####

  plot_a <- ssu |>
    summarise(ai = sum(x), .by = c(PSU, subpop, stratum))

  plot_x <- ssu |>
    summarise(count_sp = n(), xid = sum(x), .by = c(PSU, subpop, stratum, dom))

  plot_y <- ssu_y |>
    summarise(yid = sum(y), .by = c(PSU, subpop, stratum, dom, sub))

  ## All sub-classes for each plot x domain, absent sub-classes count as 0
  plot_attr <- plot_x |>
    cross_join(tibble(sub = vec_sub)) |>
    left_join(plot_y, by = join_by(PSU, subpop, stratum, dom, sub)) |>
    mutate(yid = replace_na(yid, 0)) |>
    left_join(plot_a, by = join_by(PSU, subpop, stratum))


  ## Strata weights and sample sizes ####

  ss_ph1 <- .ph1_df |>
    summarise(Nh = n(), .by = c(subpop, stratum)) |>
    mutate(N = sum(Nh), .by = subpop) |>
    mutate(
      Wh = Nh/N,
      Ah = .aoi_area * Nh / Ntot ## divided by overall total number of plot in Phase 1
    )

  ss_ph2 <- plot_a |>
    summarise(nh = n(), .by = c(subpop, stratum))

  ## n joined by subpop only, so that strata without phase 2 sample keep their between-strata variance term
  ss_ph2_totals <- ss_ph2 |>
    summarise(n = sum(nh), .by = subpop)

  subpop_stratum_a <- plot_a |>
    summarise(sum_ai = sum(ai), sum_ai_sq = sum(ai^2), .by = c(subpop, stratum))

  DF_all <- sum(ss_ph2_totals$n) - nrow(ss_ph2_totals) * Nstrat


  ## Estimation for a plot table (PSU, subpop, stratum, dom, sub, yid, xid, ai) ####

  calc_estimates <- function(.plot){

    ## + Aggregate plot to subpop x stratum ####
    subpop_stratum_attr <- .plot |>
      summarise(
        count_plot_h = n(),
        count_sp     = sum(count_sp),
        sum_yid      = sum(yid),
        sum_yid_sq   = sum(yid^2),
        sum_yid_ai   = sum(yid * ai),
        sum_xid      = sum(xid),
        sum_xid_sq   = sum(xid^2),
        sum_xid_ai   = sum(xid * ai),
        sum_yid_xid  = sum(yid * xid),
        .by = c(subpop, stratum, dom, sub)
      )

    subpop_stratum_d <- expand_grid(dom = vec_d, sub = unique(.plot$sub), subpop = vec_subpop, stratum = vec_stratum) |>
      left_join(ss_ph1, by = join_by(subpop, stratum)) |>
      left_join(ss_ph2, by = join_by(subpop, stratum)) |>
      left_join(ss_ph2_totals, by = join_by(subpop)) |>
      left_join(subpop_stratum_attr, by = join_by(subpop, stratum, dom, sub)) |>
      left_join(subpop_stratum_a, by = join_by(subpop, stratum)) |>
      mutate(across(where(is.numeric), \(x) replace_na(x, 0))) |>
      mutate(
        mean_yd = if_else(sum_ai > 0, sum_yid / sum_ai, 0),
        mean_xd = if_else(sum_ai > 0, sum_xid / sum_ai, 0)
      ) |>
      mutate(
        subpop_mean_yd = sum(mean_yd * Wh),
        subpop_mean_xd = sum(mean_xd * Wh),
        .by = c(dom, sub, subpop)
      ) |>
      mutate(
        var_yd          = if_else(nh > 1 & sum_ai > 0, nh^2/(nh - 1) * (sum_yid_sq - 2*mean_yd*sum_yid_ai + mean_yd^2*sum_ai_sq) / (sum_ai^2), 0),
        var_xd          = if_else(nh > 1 & sum_ai > 0, nh^2/(nh - 1) * (sum_xid_sq - 2*mean_xd*sum_xid_ai + mean_xd^2*sum_ai_sq) / (sum_ai^2), 0),
        covar_xdyd      = if_else(nh > 1 & sum_ai > 0, nh^2/(nh - 1) * (sum_yid_xid - mean_yd*sum_xid_ai - mean_xd*sum_yid_ai + mean_yd*mean_xd*sum_ai_sq) / (sum_ai^2), 0),
        ## Within-stratum term only for sampled strata, between-strata term (phase 1 error) for all strata
        var_mean_yd     = if_else(Nh > 1 & n > 0, Wh * (Nh - 1) / (N - 1) * if_else(nh > 0, var_yd / nh, 0) + Wh / (N - 1) * (mean_yd - subpop_mean_yd)^2, 0),
        var_mean_xd     = if_else(Nh > 1 & n > 0, Wh * (Nh - 1) / (N - 1) * if_else(nh > 0, var_xd / nh, 0) + Wh / (N - 1) * (mean_xd - subpop_mean_xd)^2, 0),
        covar_mean_xdyd = if_else(Nh > 1 & n > 0, Wh * (Nh - 1) / (N - 1) * if_else(nh > 0, covar_xdyd / nh, 0) + Wh / (N - 1) * (mean_xd - subpop_mean_xd) * (mean_yd - subpop_mean_yd), 0)
      ) |>
      arrange(dom, sub, subpop, stratum)

    ## + Aggregate to sub-populations ####
    subpop_d <- subpop_stratum_d |>
      summarise(
        count_plot     = sum(count_plot_h),
        subpop_mean_yd = sum(mean_yd * Wh),
        subpop_mean_xd = sum(mean_xd * Wh),
        subpop_var_yd  = sum(var_mean_yd),
        subpop_var_xd  = sum(var_mean_xd),
        subpop_covar_xdyd = sum(covar_mean_xdyd),
        subpop_n       = sum(nh),
        subpop_N       = sum(Nh),
        subpop_A       = sum(Ah),
        .by = c(dom, sub, subpop)
      ) |>
      mutate(
        subpop_W      = subpop_N / Ntot,
        subpop_Rd     = if_else(subpop_mean_xd > 0, subpop_mean_yd / subpop_mean_xd, 0),
        subpop_Rd_var = if_else(subpop_mean_xd > 0, abs((subpop_var_yd + subpop_Rd^2*subpop_var_xd - 2*subpop_Rd*subpop_covar_xdyd) / (subpop_mean_xd^2)), 0),
        subpop_Rd_se  = sqrt(subpop_Rd_var),
        subpop_Rd_me  = t_crit(subpop_n - Nstrat) * subpop_Rd_se,
        subpop_Rd_mep = if_else(subpop_Rd > 0, round(subpop_Rd_me / subpop_Rd * 100, 2), 0)
      )

    ## + Totals ####
    totals_d <- subpop_d |>
      summarise(
        total_plot = sum(count_plot),
        Yd         = sum(subpop_mean_yd * subpop_W),
        Yd_var     = sum(subpop_var_yd * subpop_W^2),
        Xd         = sum(subpop_mean_xd * subpop_W),
        Xd_var     = sum(subpop_var_xd * subpop_W^2),
        covar_XdYd = sum(subpop_covar_xdyd * subpop_W^2),
        Ytot       = sum(subpop_mean_yd * subpop_A),
        Xtot       = sum(subpop_mean_xd * subpop_A),
        .by = c(dom, sub)
      ) |>
      mutate(
        Yd_mep = if_else(Yd != 0, round(t_crit(DF_all) * sqrt(Yd_var) / Yd * 100, 2), 0),
        Xd_mep = if_else(Xd != 0, round(t_crit(DF_all) * sqrt(Xd_var) / Xd * 100, 2), 0),
        Rd     = if_else(Xd > 0, round(Yd / Xd, 3), 0),
        Rd_var = if_else(Xd > 0, (Yd_var + Rd^2*Xd_var - 2*Rd*covar_XdYd)/Xd^2, 0),
        ## Margin of Error = half width of confidence interval, mep = percentage margin of error
        ## No within-domain variance with less than 2 plots: margin of error not available
        Rd_mep = case_when(total_plot < 2 ~ NA_real_, Rd != 0 ~ round(t_crit(DF_all) * sqrt(pmax(Rd_var, 0)) / Rd * 100, 2), TRUE ~ 0)
      )

    list(subpop_stratum = subpop_stratum_d, subpop_d = subpop_d, totals_d = totals_d)
  }


  ## Ratio estimates by domain (x sub-class) ####

  res <- calc_estimates(plot_attr)

  totals_d <- res$totals_d |>
    select(dom, sub, n_pp = total_plot, Yd, Yd_mep, Xd, Xd_mep, Rd, Rd_mep, Ytot, Xtot)

  out <- list(
    plot           = plot_attr,
    subpop_stratum = res$subpop_stratum,
    subpop_d       = res$subpop_d,
    totals_d       = totals_d,
    totals_short   = totals_d |> select(dom, sub, n_pp, Rd, Rd_mep)
  )


  ## Shannon index by domain ####

  if (.index == "shannon") {

    ## + Index and linearization coefficients from domain estimates ####
    shannon_p <- res$totals_d |>
      select(dom, sub, Yd) |>
      mutate(
        Yd_sum = sum(Yd),
        p      = if_else(Yd_sum > 0, Yd / Yd_sum, 0),
        .by = dom
      ) |>
      mutate(H = -sum(if_else(p > 0, p * log(p), 0)), .by = dom) |>
      mutate(b = if_else(p > 0, -(log(p) + H) / Yd_sum, 0))

    ## + Linearized variable at plot level ####
    plot_z <- plot_attr |>
      left_join(select(shannon_p, dom, sub, b), by = join_by(dom, sub)) |>
      summarise(yid = sum(b * yid), .by = c(PSU, subpop, stratum, dom, count_sp, xid, ai)) |>
      mutate(sub = "shannon")

    res_z <- calc_estimates(plot_z)

    ## + Index, richness, evenness and uncertainty ####
    out$shannon_d <- shannon_p |>
      summarise(
        H        = first(H),
        richness = sum(p > 0),
        .by = dom
      ) |>
      mutate(evenness = if_else(richness > 1, H / log(richness), NA_real_)) |>
      left_join(select(res_z$totals_d, dom, n_pp = total_plot, H_var = Yd_var), by = join_by(dom)) |>
      mutate(
        H_se  = sqrt(H_var),
        H_me  = if_else(n_pp < 2, NA_real_, t_crit(DF_all) * H_se),
        H_mep = if_else(H > 0, round(H_me / H * 100, 2), 0)
      ) |>
      select(dom, n_pp, H, H_me, H_mep, richness, evenness)
  }

  ## Outputs with input column names ####
  map(out, \(x) x |> rename_out() |> mutate(attr = as_label(attr_y)) |> select(attr, everything()))

}



## Function 5: nfi_aggregate2() for several reporting levels ####

## Runs nfi_aggregate2() once per domain column and binds the results.
## .levels: named vector of domain columns in .ph2_sp, names are the reporting level labels,
##          e.g. c(country = "dom_country", forest_type = "dom_ftype")
## ...    : other arguments of nfi_aggregate2() (.ph2_sub, .class_sub, .index, .alpha)
## .out   : output(s) of nfi_aggregate2() to return. One output returns a table, several return a list of tables.
## The domain column is renamed 'domain' and a 'level' column is added.

nfi_aggregate_levels <- function(.levels, .ph1_df, .ph2_sp, .attr_y, .attr_x, .aoi_area, ..., .out = "totals_d"){

  attr_y <- enquo(.attr_y)
  attr_x <- enquo(.attr_x)

  res <- imap(.levels, \(col, lvl) {
    nfi_aggregate2(
      .ph1_df = .ph1_df, .ph2_sp = .ph2_sp, .class_d = !!sym(col),
      .attr_y = !!attr_y, .attr_x = !!attr_x, .aoi_area = .aoi_area, ...
    )
  })

  out <- map(set_names(.out), \(o) {
    imap(res, \(r, lvl) r[[o]] |> rename(domain = all_of(.levels[[lvl]])) |> mutate(level = lvl, .after = attr)) |>
      list_rbind()
  })

  if (length(.out) == 1) out[[1]] else out

}


## Function 6: tree values to SSU x sub-class, input .ph2_sub of nfi_aggregate2() ####

## SSU = plot x LULC section (tree_lulc_no), as in plot_lulc_analysis.
## .y: tree value expanded to per ha with the tree weight (e.g. tree_weight for density,
##     tree_basal_area * tree_weight for basal area), multiplied by .plot_area to get the SSU total.
## .name: name of the output column

tree_to_ssu <- function(.tree, .class_sub, .y, .plot_area, .name = "ssu_total"){

  class_sub <- enquo(.class_sub)
  y         <- enquo(.y)

  .tree |>
    summarise(
      !!.name := sum(!!y) * .plot_area,
      .by = c(cluster_no, plot_no, tree_lulc_no, !!class_sub)
    ) |>
    mutate(PSU = cluster_no, SSU = paste(plot_no, tree_lulc_no, sep = "_"))

}


## Function 7: number and share of plots with a category, by reporting level ####

## For plot level records (wildlife, disturbance, species presence...), not design-based estimates.
## .plot_cat : records with cluster_no, plot_no and the category column (.cat), several rows per plot allowed
## .plot_info: one row per plot to include (e.g. accessible field plots), with cluster_no, plot_no,
##             forest_type and region. It also gives the total number of plots per domain (n_plots_total).

count_plots_by_level <- function(.plot_cat, .plot_info, .cat){

  cat <- enquo(.cat)

  info_levels <- list(
    country            = .plot_info |> mutate(forest_type = "All", region = "All"),
    forest_type        = .plot_info |> mutate(region = "All"),
    forest_type_region = .plot_info
  )

  imap(info_levels, \(info, lvl) {
    tot <- info |> count(forest_type, region, name = "n_plots_total")

    .plot_cat |>
      distinct(cluster_no, plot_no, category = !!cat) |>
      inner_join(info, by = join_by(cluster_no, plot_no)) |>
      count(forest_type, region, category, name = "n_plots") |>
      left_join(tot, by = join_by(forest_type, region)) |>
      mutate(level = lvl, pct_plots = round(n_plots / n_plots_total * 100, 1))
  }) |>
    list_rbind() |>
    rename(!!rlang::as_name(cat) := category) |>
    select(level, everything())

}


## Function 8: split a table with a 'level' column into one table per level ####

split_by_level <- function(.df){
  lvls <- unique(.df$level)
  map(set_names(lvls), \(l) .df |> filter(level == l) |> select(-level))
}


## Function 9: save a list of tables as csv files <prefix>_<table name>.csv ####

save_tables <- function(.tabs, .prefix, .dir = "results/tables"){
  iwalk(.tabs, \(x, nm) write_csv(x, file.path(.dir, paste0(.prefix, "_", nm, ".csv"))))
}
