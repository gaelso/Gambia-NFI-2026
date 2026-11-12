
## f. Plots per type of disturbance ####

## Number and share of accessible field plots with signs of each disturbance. Main disturbances are
## recorded as yes/no, other disturbances as a type. Forest type of the plot main LULC section.

tmp$disturbance <- bind_rows(
  plot |>
    select(
      cluster_no, plot_no,
      Grazing = plot_grazing_sign, Fire = plot_fire_sign,
      `Branch removal` = plot_branch_removal_sign, `Tree felling` = plot_tree_felling_sign
    ) |>
    pivot_longer(-c(cluster_no, plot_no), names_to = "disturbance", values_to = "sign") |>
    filter(sign) |>
    mutate(disturbance_group = "Main"),
  plot |>
    filter(!is.na(plot_other_disturbances)) |>
    select(cluster_no, plot_no, disturbance = plot_other_disturbances) |>
    mutate(disturbance_group = "Other")
)

tmp$disturbance <- tmp$disturbance |>
  select(cluster_no, plot_no, disturbance) |>
  bind_rows(tmp$disturbance |> distinct(cluster_no, plot_no) |> mutate(disturbance = "Any disturbance"))

tmp$disturbance_group <- tibble(
  disturbance = c("Any disturbance", "Grazing", "Fire", "Branch removal", "Tree felling"),
  disturbance_group = c("Any", rep("Main", 4))
)

res_disturbance <- count_plots_by_level(tmp$disturbance, plot_info, disturbance) |>
  left_join(tmp$disturbance_group, by = join_by(disturbance)) |>
  mutate(
    disturbance_group = factor(replace_na(disturbance_group, "Other"), levels = c("Any", "Main", "Other")),
    forest_type = factor(forest_type, levels = defaults$ftype_levels),
    region = factor(region, levels = defaults$region_levels)
  ) |>
  select(level, forest_type, region, disturbance_group, disturbance, everything()) |>
  arrange(level, forest_type, region, disturbance_group, desc(n_plots))

## + Tables ####
tab$f_disturbance <- split_by_level(res_disturbance)
tab$f_disturbance$forest_type

save_tables(tab$f_disturbance, "05f_disturbance")

## + Figures ####
fig$f_disturbance <- tab$f_disturbance$forest_type |>
  filter(disturbance_group != "Other") |>
  mutate(disturbance = fct_relevel(disturbance, "Any disturbance")) |>
  ggplot(aes(x = disturbance, y = pct_plots, fill = forest_type)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.8) +
  scale_fill_manual(values = defaults$pal_ftype) +
  labs(x = "", y = "Plots with signs (%)", fill = "") +
  theme(legend.position = "bottom")
fig$f_disturbance

fig$f_disturbance_region <- tab$f_disturbance$forest_type_region |>
  filter(disturbance_group == "Main") |>
  ggplot(aes(x = region, y = disturbance, fill = pct_plots)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(n_plots, "/", n_plots_total)), size = 2.5) +
  scale_fill_gradientn(colors = c("#fde0c5", "#facba6", "#f59e72", "#e4572e", "#a3301b")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~forest_type) +
  labs(x = "", y = "", fill = "Plots (%)", caption = "Labels: plots with signs / accessible plots") +
  theme(panel.grid = element_blank())
fig$f_disturbance_region

ggsave("results/figures/05f_disturbance.png", fig$f_disturbance, width = 18, height = 11, units = "cm", dpi = 300)
ggsave("results/figures/05f_disturbance_region.png", fig$f_disturbance_region, width = 22, height = 12, units = "cm", dpi = 300)
