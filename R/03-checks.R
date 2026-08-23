

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
