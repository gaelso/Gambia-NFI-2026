
## d. Plots with signs of wildlife ####

## Number and share of accessible field plots with at least one wildlife record, by wildlife type and
## by type of sign. Forest type of the plot main LULC section.

## + By wildlife type ####
tmp$wildlife_type <- bind_rows(
  wildlife |> select(cluster_no, plot_no, wildlife_type_label),
  wildlife |> distinct(cluster_no, plot_no) |> mutate(wildlife_type_label = "Any wildlife")
)

res_wildlife <- count_plots_by_level(tmp$wildlife_type, plot_info, wildlife_type_label) |>
  mutate(
    forest_type = factor(forest_type, levels = defaults$ftype_levels),
    region = factor(region, levels = defaults$region_levels)
  ) |>
  arrange(level, forest_type, region, wildlife_type_label)

## + By type of sign ####
tmp$wildlife_sign <- wildlife_sign |>
  filter(!is.na(wildlife_sign_label)) |>
  left_join(wildlife |> select(cluster_no, plot_no, wildlife_id, wildlife_type_label), by = join_by(cluster_no, plot_no, wildlife_id)) |>
  select(cluster_no, plot_no, wildlife_sign_label)

res_wildlife_sign <- count_plots_by_level(tmp$wildlife_sign, plot_info, wildlife_sign_label) |>
  mutate(
    forest_type = factor(forest_type, levels = defaults$ftype_levels),
    region = factor(region, levels = defaults$region_levels)
  ) |>
  arrange(level, forest_type, region, desc(n_plots))

## + Tables ####
tab$d_wildlife      <- split_by_level(res_wildlife)
tab$d_wildlife_sign <- split_by_level(res_wildlife_sign)
tab$d_wildlife$forest_type

save_tables(tab$d_wildlife, "05d_wildlife")
save_tables(tab$d_wildlife_sign, "05d_wildlife_sign")

## + Figures ####
fig$d_wildlife <- tab$d_wildlife$forest_type |>
  ggplot(aes(x = wildlife_type_label, y = pct_plots, fill = forest_type)) +
  geom_col(position = position_dodge(preserve = "single"), width = 0.8) +
  scale_fill_manual(values = defaults$pal_ftype) +
  labs(x = "", y = "Plots with signs (%)", fill = "") +
  theme(legend.position = "bottom")
fig$d_wildlife

fig$d_wildlife_region <- tab$d_wildlife$forest_type_region |>
  filter(wildlife_type_label != "Any wildlife") |>
  ggplot(aes(x = region, y = wildlife_type_label, fill = pct_plots)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(n_plots, "/", n_plots_total)), size = 2.5) +
  scale_fill_gradientn(colors = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~forest_type) +
  labs(x = "", y = "", fill = "Plots (%)", caption = "Labels: plots with signs / accessible plots") +
  theme(panel.grid = element_blank())
fig$d_wildlife_region

ggsave("results/figures/05d_wildlife.png", fig$d_wildlife, width = 18, height = 11, units = "cm", dpi = 300)
ggsave("results/figures/05d_wildlife_region.png", fig$d_wildlife_region, width = 22, height = 14, units = "cm", dpi = 300)
