


##
## TREE HxD ######
##

summary(tree)

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
## Manual corrections ######
##

## Accessible plots but no LU
length(unique(plot_analysis$cluster_no))

table(plot_analysis$plot_access_label, useNA = "ifany")
table(plot_analysis$plot_access_lgl, useNA = "ifany")

table(plot_analysis$lulc_forest_label,useNA = "ifany")
table(plot_analysis$lulc_name_label,useNA = "ifany")

## >> missing LU, why?
tt <- plot_analysis |> filter(plot_access_lgl, is.na(lulc_forest))

vec_missing_LU <- tt |> pull(cluster_no)
vec_missing_LU2 <- tt |> 
  mutate(plot_id = paste(cluster_no, plot_no, sep = "-")) |>
  pull(plot_id)

tt1 <- plot_analysis |> filter(cluster_no %in% vec_missing_LU)
tt2 <- plot_analysis |> filter(plot_access_lgl)

## >> need to crosscheck field forms
tt3 <- cluster |> filter(cluster_no %in% vec_missing_LU)
tt4 <- plot |> mutate(plot_id = paste(cluster_no, plot_no, sep = "-")) |> filter(plot_id %in% vec_missing_LU2)
plot |> filter(cluster_no == 56)

table(plot_analysis$lulc_forest_label, plot_analysis$lulc_forest, plot_analysis$stratum, useNA = "ifany")
table(tt2$lulc_forest_label, tt2$lulc_forest, tt2$stratum, useNA = "ifany")

plot_analysis |> filter(!is.na(plot_access)) |> distinct(cluster_no) |> nrow()

cluster_check <- plot |> filter(plot_no == 1)
cluster_check2 <- plot_analysis |> filter(plot_no == 1)

table(cluster_check$plot_access, useNA = "ifany")

plot_analysis |> filter(lulc_forest_label)

table(plot_analysis$plot_access_label, plot_analysis$lulc_forest_label, useNA = "ifany")
table(plot_analysis$cluster_access_label, plot_analysis$lulc_forest_label, useNA = "ifany")

## Assign all NA to non-forest
## Checking one remaining NA
plot_analysis |> filter(plot_access_lgl, is.na(lulc_forest))



## Issue with plot 72/5: accessible but no LULC and no tree
tree |> filter(cluster_no == 72, plot_no == 5)

## Issue with many plots cat. as wetland but with mangrove trees

