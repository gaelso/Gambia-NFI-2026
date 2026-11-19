
## f. Disturbances ####

## Disturbances are recorded at plot level, without LULC section information. Main disturbances are recorded
## as yes/no with an intensity, other disturbances as a type.
## - Share of the domain area with signs of each disturbance (%): two-phase estimates with the plot as base unit
##   and the cluster as PSU (share_plot_levels()), with the number of clusters and plots with a record.
## - Intensity of the main disturbances: share of the affected area by recorded intensity (low, medium, high,
##   not recorded), from the two-phase shares of area of each disturbance x intensity.

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

res_disturbance <- share_plot_levels(tmp$disturbance, disturbance) |>
  left_join(tmp$disturbance_group, by = join_by(disturbance)) |>
  mutate(disturbance_group = factor(replace_na(disturbance_group, "Other"), levels = c("Any", "Main", "Other")))

## + Tables ####
## Share of area with signs (%) with margin of error, clusters and plots with a record
tab$f_disturbance <- res_disturbance |>
  transmute(level, forest_type, region, n_pp, disturbance_group, disturbance, n_clusters, n_plots, rec_pct = round(share * 100, 1), rec_mep = Rd_mep) |>
  arrange(level, forest_type, region, disturbance_group, desc(rec_pct)) |>
  split_by_level()
tab$f_disturbance$forest_type

save_tables(tab$f_disturbance, "05f_disturbance")

## + Intensity of the main disturbances ####
## Accessible field plots with each main disturbance, by recorded intensity (low, medium, high)
tmp$disturbance_intensity <- plot |>
  select(
    cluster_no, plot_no,
    Grazing_sign = plot_grazing_sign, Grazing_int = plot_grazing_intensity_label,
    Fire_sign = plot_fire_sign, Fire_int = plot_fire_intensity_label,
    `Branch removal_sign` = plot_branch_removal_sign, `Branch removal_int` = plot_branch_removal_intensity_label,
    `Tree felling_sign` = plot_tree_felling_sign, `Tree felling_int` = plot_tree_felling_intensity_label
  ) |>
  pivot_longer(-c(cluster_no, plot_no), names_to = c("disturbance", ".value"), names_sep = "_") |>
  filter(sign) |>
  mutate(
    intensity = replace_na(int, "Not recorded"),
    dist_int = paste(disturbance, intensity, sep = "|")
  )

tmp$disturbance_int_levels <- c("Low", "Medium", "High", "Not recorded")

res_disturbance_int <- share_plot_levels(tmp$disturbance_intensity, dist_int) |>
  separate_wider_delim(dist_int, delim = "|", names = c("disturbance", "intensity")) |>
  mutate(intensity = factor(intensity, levels = tmp$disturbance_int_levels))

## Share of the affected area (%) by intensity: share of area of the disturbance x intensity / share of area of
## the disturbance (any intensity)
tab$f_disturbance_int <- res_disturbance_int |>
  mutate(pct = share / sum(share) * 100, .by = c(level, domain, disturbance)) |>
  filter(!is.na(pct)) |>
  select(level, forest_type, region, disturbance, intensity, pct) |>
  pivot_wider(names_from = intensity, values_from = pct, values_fill = 0, names_expand = TRUE) |>
  left_join(
    res_disturbance |> select(level, forest_type, region, disturbance, n_pp, n_clusters, n_plots, share, Rd_mep),
    by = join_by(level, forest_type, region, disturbance)
  ) |>
  filter(share > 0) |>
  transmute(
    level, forest_type, region, n_pp,
    disturbance = factor(disturbance, levels = c("Grazing", "Fire", "Branch removal", "Tree felling")),
    n_clusters, n_plots, rec_pct = round(share * 100, 1), rec_mep = Rd_mep,
    pct_low = round(Low, 1), pct_medium = round(Medium, 1), pct_high = round(High, 1), pct_int_na = round(`Not recorded`, 1)
  ) |>
  arrange(level, forest_type, region, disturbance) |>
  split_by_level()
tab$f_disturbance_int$forest_type

save_tables(tab$f_disturbance_int, "05f_disturbance_int")

## + Figures ####
fig$f_disturbance <- tab$f_disturbance$forest_type |>
  filter(disturbance_group != "Other") |>
  mutate(disturbance = fct_relevel(disturbance, "Any disturbance"), rec_me = rec_pct * replace_na(rec_mep, 0) / 100) |>
  ggplot(aes(x = disturbance, y = rec_pct, fill = forest_type)) +
  geom_col(position = position_dodge(preserve = "single", width = 0.85), width = 0.8) +
  geom_errorbar(
    aes(ymin = pmax(rec_pct - rec_me, 0), ymax = pmin(rec_pct + rec_me, 100)),
    position = position_dodge(preserve = "single", width = 0.85), width = 0.2
  ) +
  scale_fill_manual(values = defaults$pal_ftype) +
  labs(x = "", y = "Share of area with signs (%)", fill = "", caption = "Error bars: 90% confidence interval") +
  theme(legend.position = "bottom")
fig$f_disturbance

fig$f_disturbance_region <- tab$f_disturbance$forest_type_region |>
  filter(disturbance_group == "Main") |>
  ggplot(aes(x = region, y = disturbance, fill = rec_pct)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(round(rec_pct), "%")), size = 2.5) +
  scale_fill_gradientn(colors = c("#fde0c5", "#facba6", "#f59e72", "#e4572e", "#a3301b")) +
  facet_wrap(~forest_type) +
  labs(x = "", y = "", fill = "Area (%)", caption = "Labels: share of area with signs") +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
fig$f_disturbance_region

ggsave("results/figures/05f_disturbance.png", fig$f_disturbance, width = 18, height = 11, units = "cm", dpi = 300)
ggsave("results/figures/05f_disturbance_region.png", fig$f_disturbance_region, width = 22, height = 12, units = "cm", dpi = 300)
