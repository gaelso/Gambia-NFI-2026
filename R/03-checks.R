

## list of check elements
check <- list()
gg <- list()

##
## Tree checks ######
##

summary(tree)


tree |>
  left_join(plot_analysis, by = join_by(cluster_no, plot_no)) |>
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_height, color = tree_condition_overall_label)) +
  facet_wrap(~lulc_forest_label)

ggplot(tree) +
  geom_point(aes(x = tree_dbh, y = tree_height, color = tree_condition_overall_label)) +
  facet_wrap(~cluster_no)

ggplot(tree) +
  geom_point(aes(x = tree_dbh, y = tree_height, color = tree_status_label)) +
  theme(legend.position = "none") +
  facet_wrap(~tree_status_label)

ggplot(tree) +
  geom_point(aes(x = tree_dbh, y = tree_biomass_ag))


ggplot(tree) +
  geom_point(aes(x = tree_basal_area, y = tree_biomass_ag))

##
## Plot level checks ######
##
summary(plot_analysis$plot_calc_biomass_ag)

check$plot_agb150 <- plot_analysis |> filter(plot_calc_biomass_ag >= 150)

gg$plot_agbxdens <- plot_analysis |>
  filter(plot_access_lgl) |>
  ggplot(aes(x = plot_calc_tree_density, plot_calc_biomass_ag)) +
  geom_point() +
  facet_wrap(~lulc_forest_label) +
  geom_point(data = check$plot_agb150, shape = 21, col = "red", size = 4) +
  geom_label_repel(data = check$plot_agb150, aes(label = plot_id), col = "red", min.segment.length = 0)
print(gg$plot_agbxdens)

## > 83_2 ha Baobab



##
## Check accessibility ######
## 

## Accessible plots but no LU
length(unique(plot_analysis$cluster_no))

table(plot_analysis$plot_access_label, useNA = "ifany")
table(plot_analysis$plot_access_lgl, useNA = "ifany")

table(plot_analysis$lulc_forest_label,useNA = "ifany")
table(plot_analysis$lulc_name_label,useNA = "ifany")

## >> missing LU, why?
check$plot_access_noforest <- plot_analysis |> filter(plot_access_lgl, is.na(lulc_forest))

vec_missing_LU <- check$plot_access_noforest |> pull(cluster_no)
vec_missing_LU

vec_missing_LU2 <- check$plot_access_noforest |> 
  mutate(plot_id = paste(cluster_no, plot_no, sep = "-")) |>
  pull(plot_id)
vec_missing_LU2

plot_analysis |> filter(cluster_no %in% vec_missing_LU)



##
## Check accessible forest but no tree ######
##

## Checking 
check$tt <- plot_analysis |> filter(plot_access_lgl, lulc_forest_label != "Non-forest", plot_calc_biomass_ag == 0)
check$tt
nrow(check$tt)
table(check$tt$lulc_forest_label)

## Issue with plot 72/5: accessible but no LULC and no tree
tree |> filter(cluster_no == 72, plot_no == 5)

## Issue with many plots cat. as wetland but with mangrove trees

