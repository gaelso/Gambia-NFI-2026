

## This section combines OF Arena processing chain scripts
## On the long run, recommends to write proper scripts in the Arena processing chain


##
## Tree DBH Class 10 cm ######
##



tree <- tree |> mutate(
  tree_dbh_class_10cm = case_when(
    tree_dbh < 10  ~ "000 -  9.9",
    tree_dbh < 20  ~ "010 - 19.9",
    tree_dbh < 30  ~ "020 - 29.9",
    tree_dbh < 40  ~ "030 - 39.9",
    tree_dbh < 50  ~ "040 - 49.9",
    tree_dbh < 60  ~ "050 - 59.9",
    tree_dbh < 70  ~ "060 - 69.9",
    tree_dbh < 80  ~ "070 - 79.9",
    tree_dbh < 90  ~ "080 - 89.9",
    tree_dbh < 100 ~ "090 - 99.9",
    tree_dbh < 110 ~ "100 - 109.9",
    tree_dbh < 120 ~ "110 - 119.9",
    TRUE ~ "120+"
  )
)


##
## Tree Count ######
##

## Remove trees with missing DBH or DBH < 10, move to 'common.R' ?
summary(tree$tree_dbh)
tree <- tree |> filter(!is.na(tree_dbh), tree_dbh >= 10)

tree$tree_count <- 1


##
## Tree Basal Area ######
##

tree$tree_basal_area <- pi * (tree$tree_dbh/200)^2



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
  geom_line(aes(y = tree_height_chave_cilow, col = as.character(cluster_no))) +
  geom_line(aes(y = tree_height_chave_ciupp, col = as.character(cluster_no))) +
  theme(legend.position = "none")



## + Local height model (lmfor) ######

## Model forms most used for (West) African trees, fitted with random effects (Mehtätalo et al. 2015),
## grouped by cluster as the NFI aggregation unit:
## - Curtis:           H = 1.3 + a (D / (1 + D))^b,     best fit in Sudanian savanna woodlands, Burkina Faso (Valbuena et al. 2016)
## - Weibull:          H = 1.3 + a (1 - exp(-b D^c)),   best in pantropical comparisons (Ledo et al. 2016, Sullivan et al. 2018)
## - Michaelis-Menten: H = 1.3 + a D / (b + D),         among the best in central Africa (Kearsley et al. 2016)
##
## 1. Model form, goodness of fit on measured live trees (2,862 trees, 84 clusters), nranp = random parameters per cluster:
##   model     nranp     AIC   RMSE (m)  bias (m)
##   curtis        2  13411.9    2.345    -0.015   <- selected
##   weibull       2  13422.5    2.367    -0.004   (one more parameter, fitted with nlme, no convergence in fithd)
##   micment       1  13442.5    2.402    -0.029   (no convergence with nranp = 2)
##   weibull       1  13444.0    2.402    -0.009
##   curtis        1  13470.6    2.415    -0.008
##   weibull       0  14608.4    3.101     0.000   (fixed effects only)
##   curtis        0  14609.8    3.103     0.000
##   micment       0  14627.9    3.113    -0.015
##
## 2. Effects, Curtis with residual variance as a power of DBH (varf = 1 in fithd):
##   effects                                  AIC   RMSE (m)  bias closed / open / non-forest (m)
##   fixed only                            14432.2    3.103    0.96 / -0.41 / -1.18
##   forest type fixed                     14294.7    3.015   -0.01 /  0.01 /  0.01
##   forest type random (3 levels)         14317.1    3.020    0.01 /  0.00 / -0.10   (too few levels for a variance)
##   cluster random                        13237.3    2.358    0.07 /  0.02 / -0.53
##   forest type fixed + cluster random    13225.2    2.349    0.01 /  0.02 /  0.00   <- selected
## Clusters with few measured trees are shrunk towards the curve of their forest type.
## Forest type of each tree ('plot_lulc_forest_label') added in 'R/02-preparation-common.R'

tmp$hd_data <- tree |>
  filter(!is.na(tree_height), tree_height > 1.3) |>
  transmute(h = tree_height, d = tree_dbh, ftype = factor(plot_lulc_forest_label), cluster = factor(cluster_no)) |>
  as.data.frame()

## Starting values from the model without forest type, forest type effects start at 0
tmp$hd_start <- coef(nls(h ~ HDcurtis(d, a, b), data = tmp$hd_data, start = list(a = 15, b = 10)))
tmp$hd_nftype <- nlevels(tmp$hd_data$ftype)

tmp$hd_model <- nlme(
  h ~ HDcurtis(d, a, b),
  data = tmp$hd_data,
  fixed = list(a ~ ftype, b ~ ftype),
  random = a + b ~ 1 | cluster,
  weights = varPower(form = ~ d),
  start = c(tmp$hd_start["a"], rep(0, tmp$hd_nftype - 1), tmp$hd_start["b"], rep(0, tmp$hd_nftype - 1)),
  control = nlmeControl(maxIter = 200, msMaxIter = 200, pnlsMaxIter = 50)
)

## Prediction with the cluster random effects, forest type curve for clusters without measured heights
tmp$hd_newdata <- data.frame(
  d = tree$tree_dbh,
  ftype = factor(tree$plot_lulc_forest_label, levels = levels(tmp$hd_data$ftype)),
  cluster = factor(tree$cluster_no)
)

tree$tree_height_model <- coalesce(
  as.numeric(predict(tmp$hd_model, newdata = tmp$hd_newdata, level = 1)),
  as.numeric(predict(tmp$hd_model, newdata = tmp$hd_newdata, level = 0))
)

tree |>
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height)) +
  geom_line(aes(y = tree_height_chave, color = as.character(clust)), linetype = "dotted") +
  geom_line(aes(y = tree_height_model, color = as.character(cluster_no))) +
  facet_wrap(~cluster_no) +
  theme(legend.position = "none")

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



##
## Tree Stem Volume ######
##

tree$tree_stem_volume <- 0.67 * tree$tree_basal_area * tree$tree_height_corr



##
## Tree Aboveground Biomass ######
##

## Chave 2014 equation

tree$tree_biomass_ag <- with(tree, round(0.0673 * (tree_wood_density * tree_dbh^2 * tree_height_corr)^0.976 / 1000, 3))


##
## Tree Belowground Biomass ######
##

tree$tree_biomass_bg <- NA

table(cluster$cluster_info_stratum)
table(cluster$cluster_gez, cluster$cluster_gez_label, useNA = "ifany")

cluster_gez <- cluster |> select(cluster_no, cluster_gez, cluster_gez_label, cluster_info_stratum) 

##------##
## Demonstration dry forest has AGB <= 125 tdm/ha
# cluster_agb <- tree |> summarise(cluster_agb = sum(tree_biomass_ag * tree_cluster_weight), .by = c(cluster_no)) |>
#   left_join(cluster_gez, by = join_by(cluster_no))
# ggplot(cluster_agb) +
#   geom_boxplot(aes(x = cluster_info_stratum, y = cluster_agb, fill = cluster_gez))
# rm(cluster_agb)
##------##

## Add GEZ/Stratum info, get RS and calc BGB = AGB * RS
tree <- tree |>
  mutate(
    cluster_gez = NA, 
    cluster_gez_label = NA, 
    cluster_info_stratum = NA
  ) |>
  left_join(cluster_gez, by = join_by(cluster_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    tree_RS = case_when(
      cluster_info_stratum == "mangrove forest" ~ 0.49, ## Table 4.5, IPCC 2013 Wetlands Supplement 
      cluster_gez == "TAwa" ~ 0.232, ## Table 4.4 IPCC 2019 V4CH4
      cluster_gez == "TAwb" ~ 0.332, ## Table 4.4 IPCC 2019 V4CH4, considered all dry forest with AGB <= 125 tdm/ha
    ),
    tree_biomass_bg = round(tree_biomass_ag * tree_RS, 3)
  )

## remove temporary columns and objects
tree <- tree |>
  select(-cluster_gez , -cluster_gez_label, -tree_RS)



##
## Tree Total Biomass ######
## 

tree$tree_biomass_total <- tree$tree_biomass_bg + tree$tree_biomass_ag


##
## Tree Aboveground Carbon ######
##

tree$tree_carbon_ag <- tree$tree_biomass_ag * defaults$CF



##
## Tree Belowground Carbon ######
##

tree$tree_carbon_bg <- tree$tree_biomass_bg * defaults$CF



##
## Tree Total Carbon ######
##

tree$tree_carbon_total <- tree$tree_biomass_total * defaults$CF


