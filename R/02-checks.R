

summary(tree)

ggplot(tree) +
  geom_point(aes(x = tree_dbh, y = tree_height, color = tree_condition_overall_label)) +
  facet_wrap(~cluster_no)

