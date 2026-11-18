##
## Tree Height Corrected #####
##

## + Chave et al. 2014 model for correction of NAs ######

## Missing heights of standing trees estimated with Chave et al. 2014 height-diameter model, eq. 6a:
## ln(H) = 0.893 - E + 0.760 ln(D) - 0.0340 [ln(D)]^2, RSE = 0.243
## Back-transformation bias corrected with exp(RSE^2/2) (Baskerville 1972)
## Prediction interval at the confidence level in 'defaults', exp(ln(H) +/- z * RSE), no bias correction for the bounds
tree$tree_height_chave_log <- with(tree, 0.893 - chaveE + 0.760 * log(tree_dbh) - 0.0340 * log(tree_dbh)^2)
tree$tree_height_chave <- exp(tree$tree_height_chave_log + 0.243^2 / 2)
tree$tree_height_chave_cilow <- exp(tree$tree_height_chave_log - qnorm(1 - defaults$ci_alpha / 2) * 0.243)
tree$tree_height_chave_ciupp <- exp(tree$tree_height_chave_log + qnorm(1 - defaults$ci_alpha / 2) * 0.243)

tree |>
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height)) +
  geom_line(aes(y = tree_height_chave, col = as.character(cluster_no))) +
  geom_line(
    aes(y = tree_height_chave_cilow, col = as.character(cluster_no)),
    linetype = "dotted"
  ) +
  geom_line(
    aes(y = tree_height_chave_ciupp, col = as.character(cluster_no)),
    linetype = "dotted"
  ) +
  facet_wrap(~cluster_no) +
  theme(legend.position = "none")



## + Local height model (lmfor) ######

## Missing heights predicted with a local Curtis model, H = 1.3 + a (D / (1 + D))^b, the best form in Sudanian
## savanna woodlands of Burkina Faso (Valbuena et al. 2016). Measured live trees only, forest type of each tree
## ('plot_lulc_forest_label') added in 'R/02-preparation-common.R'.
##
## Forest type curve by default, cluster curve only where the cluster data support it:
## - forest type curve: a and b by forest type (fixed effects), residual variance as a power of DBH (nranp = 0)
## - cluster curve: same model with a cluster random effect on a only (nranp = 1), i.e. height level of the site,
##   the shape comes from the forest type. Random effects on a and b (nranp = 2) were tested and not kept: they
##   gave flat or decreasing curves in clusters with a narrow DBH range (5 cluster x forest type curves with
##   Curtis, 10 with Michaelis-Menten), and the effects of a and b are strongly correlated and not identifiable
##   with few trees per cluster (Valbuena et al. 2016, random effect on one parameter only).
## - the cluster curve is used if the cluster has at least defaults$hd_min_trees measured heights over a DBH range
##   of at least defaults$hd_min_range cm, the cluster curve increases over this range, and the DBH of the tree is
##   inside this range (no extrapolation). Otherwise the forest type curve is used (tree_height_model_choice).
## Curtis kept although Weibull and Michaelis-Menten have a slightly lower AIC with a cluster effect: simplest form
## (2 parameters), selected in Sudanian savanna woodlands (Valbuena et al. 2016), and the Weibull asymptote brings
## little with the narrow DBH range of the data (few trees above 60 cm).
##
## Model forms most used for (West) African trees, compared below with 0 and 1 random parameter per cluster
## (nranp), results in tab$hd_models (methods of the report):
## - Curtis:           H = 1.3 + a (D / (1 + D))^b
## - Weibull:          H = 1.3 + a (1 - exp(-b D^c)),   best in pantropical comparisons (Feldpausch et al. 2012)
##                     c common to all forest types (no convergence with c by forest type)
## - Michaelis-Menten: H = 1.3 + a D / (b + D),         among the best in central Africa (Kearsley et al. 2016)

## Report tables (see also 05-double-sampling.R)
if (!exists("tab")) tab <- list()

defaults$hd_min_trees <- 10
defaults$hd_min_range <- 20

tmp$hd_data <- tree |>
  filter(!is.na(tree_height), tree_height > 1.3) |>
  transmute(
    h = tree_height,
    d = tree_dbh,
    ftype = factor(plot_lulc_forest_label),
    cluster = factor(cluster_no),
    h_chave = tree_height_chave
  ) |>
  as.data.frame()

tmp$hd_nftype <- nlevels(tmp$hd_data$ftype)

## Starting values from the models without forest type and random effects, forest type effects start at 0
tmp$hd_start_curtis  <- coef(nls(h ~ HDcurtis(d, a, b), data = tmp$hd_data, start = list(a = 15, b = 10)))
tmp$hd_start_weibull <- coef(nls(h ~ HDweibull(d, a, b, c), data = tmp$hd_data, start = list(a = 20, b = 0.05, c = 1)))
tmp$hd_start_micment <- coef(nls(h ~ HDmicment(d, a, b), data = tmp$hd_data, start = list(a = 20, b = 20)))

## Models, absent from the list if no convergence
tmp$hd_models <- list()
tmp$hd_ids <- paste(rep(c("curtis", "weibull", "micment"), each = 2), 0:1, sep = "_")


## ++ Curtis, nranp = 0: forest type curve ######

tmp$hd_models$curtis_0 <- gnls(
  h ~ HDcurtis(d, a, b),
  data = tmp$hd_data,
  params = list(a ~ ftype, b ~ ftype),
  weights = varPower(form = ~ d),
  start = c(tmp$hd_start_curtis["a"], rep(0, tmp$hd_nftype - 1), tmp$hd_start_curtis["b"], rep(0, tmp$hd_nftype - 1))
)


## ++ Curtis, nranp = 1: cluster effect on a ######

## Starting values from the forest type curve
tmp$hd_models$curtis_1 <- nlme(
  h ~ HDcurtis(d, a, b),
  data = tmp$hd_data,
  fixed = list(a ~ ftype, b ~ ftype),
  random = a ~ 1 | cluster,
  weights = varPower(form = ~ d),
  start = coef(tmp$hd_models$curtis_0),
  control = nlmeControl(maxIter = 200, msMaxIter = 200, pnlsMaxIter = 50)
)


## ++ Weibull, nranp = 0 ######

tmp$hd_models$weibull_0 <- tryCatch(
  gnls(
    h ~ HDweibull(d, a, b, c),
    data = tmp$hd_data,
    params = list(a ~ ftype, b ~ ftype, c ~ 1),
    weights = varPower(form = ~ d),
    start = c(tmp$hd_start_weibull["a"], rep(0, tmp$hd_nftype - 1), tmp$hd_start_weibull["b"], rep(0, tmp$hd_nftype - 1), tmp$hd_start_weibull["c"]),
    control = gnlsControl(maxIter = 200, nlsMaxIter = 50)
  ),
  error = function(e) NULL
)


## ++ Weibull, nranp = 1: cluster effect on a ######

tmp$hd_models$weibull_1 <- tryCatch(
  nlme(
    h ~ HDweibull(d, a, b, c),
    data = tmp$hd_data,
    fixed = list(a ~ ftype, b ~ ftype, c ~ 1),
    random = a ~ 1 | cluster,
    weights = varPower(form = ~ d),
    start = coef(tmp$hd_models$weibull_0),
    control = nlmeControl(maxIter = 200, msMaxIter = 200, pnlsMaxIter = 50)
  ),
  error = function(e) NULL
)


## ++ Michaelis-Menten, nranp = 0 ######

## No convergence from the common starting values: start from separate fits by forest type
## (first forest type as reference, other forest types as differences)
tmp$hd_start_micment_ftype <- map(levels(tmp$hd_data$ftype), \(x) coef(nls(
  h ~ HDmicment(d, a, b), data = filter(tmp$hd_data, ftype == x), start = as.list(tmp$hd_start_micment)
))) |>
  bind_rows()

tmp$hd_models$micment_0 <- tryCatch(
  gnls(
    h ~ HDmicment(d, a, b),
    data = tmp$hd_data,
    params = list(a ~ ftype, b ~ ftype),
    weights = varPower(form = ~ d),
    start = with(tmp$hd_start_micment_ftype, c(a[1], a[-1] - a[1], b[1], b[-1] - b[1])),
    control = gnlsControl(maxIter = 200, nlsMaxIter = 50)
  ),
  error = function(e) NULL
)


## ++ Michaelis-Menten, nranp = 1: cluster effect on a ######

tmp$hd_models$micment_1 <- tryCatch(
  nlme(
    h ~ HDmicment(d, a, b),
    data = tmp$hd_data,
    fixed = list(a ~ ftype, b ~ ftype),
    random = a ~ 1 | cluster,
    weights = varPower(form = ~ d),
    start = coef(tmp$hd_models$micment_0),
    control = nlmeControl(maxIter = 200, msMaxIter = 200, pnlsMaxIter = 50)
  ),
  error = function(e) NULL
)


## ++ Selected approach: Curtis forest type curve, cluster curve where supported ######

## Clusters with measured heights: number of trees and DBH range
tmp$hd_cluster <- tmp$hd_data |>
  summarise(hd_cl_n = n(), hd_cl_dmin = min(d), hd_cl_dmax = max(d), .by = cluster)

## Cluster curve increasing over the DBH range of the cluster, for each forest type
## (always the case with a random effect on a only, unless a cluster has a negative a)
tmp$hd_cluster_rising <- tmp$hd_cluster |>
  cross_join(tibble(ftype = factor(levels(tmp$hd_data$ftype), levels = levels(tmp$hd_data$ftype)))) |>
  mutate(
    h_dmin = as.numeric(predict(tmp$hd_models$curtis_1, data.frame(d = hd_cl_dmin, ftype, cluster), level = 1)),
    h_dmax = as.numeric(predict(tmp$hd_models$curtis_1, data.frame(d = hd_cl_dmax, ftype, cluster), level = 1)),
    hd_cl_rising = h_dmax > h_dmin
  ) |>
  transmute(
    cluster_no = as.numeric(as.character(cluster)),
    plot_lulc_forest_label = as.character(ftype),
    hd_cl_n, hd_cl_dmin, hd_cl_dmax, hd_cl_rising
  )

## Predictions of the two curves for all trees: forest type curve, and cluster curve for the clusters with
## measured heights
tmp$hd_tree_data <- data.frame(
  d = tree$tree_dbh,
  ftype = factor(tree$plot_lulc_forest_label, levels = levels(tmp$hd_data$ftype)),
  cluster = factor(tree$cluster_no, levels = levels(tmp$hd_data$cluster))
)
tmp$hd_tree_in_cluster <- !is.na(tmp$hd_tree_data$cluster)

tree$tree_height_model_ftype <- as.numeric(predict(tmp$hd_models$curtis_0, tmp$hd_tree_data))
tree$tree_height_model_cluster <- NA_real_
tree$tree_height_model_cluster[tmp$hd_tree_in_cluster] <- as.numeric(predict(
  tmp$hd_models$curtis_1, tmp$hd_tree_data[tmp$hd_tree_in_cluster, ], level = 1
))

## Curve chosen for each tree, and model height
tmp$hd_cols <- setdiff(names(tmp$hd_cluster_rising), c("cluster_no", "plot_lulc_forest_label"))

tree <- tree |>
  mutate(!!!set_names(rep(list(NA), length(tmp$hd_cols)), tmp$hd_cols)) |>
  left_join(tmp$hd_cluster_rising, by = join_by(cluster_no, plot_lulc_forest_label), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    tree_height_model_choice = case_when(
      is.na(hd_cl_n)                                   ~ "forest type: no measured height in the cluster",
      hd_cl_n < defaults$hd_min_trees                  ~ "forest type: few measured heights in the cluster",
      hd_cl_dmax - hd_cl_dmin < defaults$hd_min_range  ~ "forest type: narrow DBH range in the cluster",
      !hd_cl_rising                                    ~ "forest type: cluster curve not increasing",
      tree_dbh < hd_cl_dmin | tree_dbh > hd_cl_dmax    ~ "forest type: DBH outside the cluster range",
      TRUE                                             ~ "cluster"
    ),
    tree_height_model = case_when(
      tree_height_model_choice == "cluster" ~ tree_height_model_cluster,
      TRUE                                  ~ tree_height_model_ftype
    )
  ) |>
  select(-all_of(tmp$hd_cols))

## Checks: all trees with a model height, curve used for all trees and for the trees with missing height
sum(is.na(tree$tree_height_model))
table(tree$tree_height_model_choice, useNA = "ifany")
table(tree$tree_height_model_choice[is.na(tree$tree_height)], useNA = "ifany")

tree |>
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height)) +
  geom_line(aes(y = tree_height_chave), linetype = "dotted") +
  geom_line(aes(y = tree_height_model, color = tree_height_model_choice == "cluster", group = plot_lulc_forest_label)) +
  facet_wrap(~cluster_no) +
  theme(legend.position = "bottom") +
  labs(color = "Cluster curve")


## ++ Comparison table ######

## Fit statistics on the measured heights: AIC, RMSE, bias (prediction - measure), largest absolute bias of a
## forest type, and number of clusters with their own curve.

## Measured trees in the same order as tmp$hd_data
tmp$hd_measured <- !is.na(tree$tree_height) & tree$tree_height > 1.3

tmp$hd_preds <- c(
  list(chave = tmp$hd_data$h_chave),
  map(tmp$hd_models, \(m) as.numeric(fitted(m))),
  list(selected = tree$tree_height_model[tmp$hd_measured])
)

tmp$hd_stats <- map(tmp$hd_preds, \(p) tibble(
  hd_rmse = sqrt(mean((p - tmp$hd_data$h)^2)),
  hd_bias = mean(p - tmp$hd_data$h),
  hd_bias_ftype = max(abs(tapply(p - tmp$hd_data$h, tmp$hd_data$ftype, mean)))
)) |>
  list_rbind(names_to = "id")

tmp$hd_n_clusters <- nlevels(tmp$hd_data$cluster)

tab$hd_models <- tibble(
  id = c("chave", tmp$hd_ids, "selected"),
  hd_model = c(
    "Chave et al. (2014), eq. 6a",
    rep(c("Curtis", "Weibull", "Michaelis-Menten"), each = 2),
    "Selected: Curtis, cluster curve where supported"
  ),
  hd_nranp = c(NA, rep(0:1, 3), NA),
  hd_aic = c(NA, map_dbl(tmp$hd_ids, \(x) if (is.null(tmp$hd_models[[x]])) NA else AIC(tmp$hd_models[[x]])), NA),
  hd_n_curves = c(0, rep(c(0, tmp$hd_n_clusters), 3), n_distinct(tree$cluster_no[tree$tree_height_model_choice == "cluster"]))
) |>
  left_join(tmp$hd_stats, by = join_by(id)) |>
  mutate(
    hd_n_curves = if_else(is.na(hd_rmse), NA, hd_n_curves),
    hd_nranp = as.character(hd_nranp),
    hd_aic = round(hd_aic),
    across(c(hd_rmse, hd_bias, hd_bias_ftype), \(x) round(x, 2))
  ) |>
  select(hd_model, hd_nranp, hd_aic, hd_rmse, hd_bias, hd_bias_ftype, hd_n_curves)

tab$hd_models

## Values for the report text
tab$hd_summary <- tibble(
  n_trees_measured = nrow(tmp$hd_data),
  n_clusters       = tmp$hd_n_clusters,
  n_clusters_curve = n_distinct(tree$cluster_no[tree$tree_height_model_choice == "cluster"]),
  n_trees_missing  = sum(is.na(tree$tree_height)),
  n_missing_cluster_curve = sum(is.na(tree$tree_height) & tree$tree_height_model_choice == "cluster"),
  min_trees = defaults$hd_min_trees,
  min_range = defaults$hd_min_range
)


  ## + Corrected heights ######

  ## Stumps keep their recorded height (stump height), the model predicts total tree height
  ## Missing heights and heights outside the prediction interval replaced with the model height
  table(tree$tree_status_label)

## Measured heights trusted (screened in the data cleaning), only missing heights replaced with the local model
tree <- tree |>
  mutate(
    tree_height_corr = case_when(
      is.na(tree_height) ~ tree_height_model,
      ## Chave threshold not used as biased upward in WestAf Forest.
      # tree_height < tree_height_chave_cilow ~ tree_height_chave,
      # tree_height > tree_height_chave_ciupp ~ tree_height_chave,
      TRUE ~ tree_height
    )
  ) |>
  select(-tree_height_chave_log)

