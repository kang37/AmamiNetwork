# 任务1：导出各地点轨迹点数和游客-本地比率。
temp_plt_loc_smry <- function(tar_var) {
  loc_smry_proc <- loc_smry_1 %>%
    pivot_wider(
      id_cols = loc_id,
      names_from = source, values_from = all_of(tar_var), values_fill = 0
    ) %>%
    mutate(vis_2_loc = tourist / local, num = tourist + local) %>%
    mutate(
      vis_2_loc_quan = cut(
        vis_2_loc,
        breaks = quantile(vis_2_loc, probs = seq(0, 1, 0.25), na.rm = TRUE),
        labels = 1:4,
        include.lowest = TRUE
      )
    ) %>%
    left_join(loc, by = "loc_id") %>%
    st_as_sf()
}
temp_plt_loc_smry("tp_num") %>%
  st_write("data_proc/loc_tp_num/loc_tp_num.shp")
plt_loc_smry("dailyid_num") %>%
  st_write("data_proc/loc_dailyid_num/loc_dailyid_num.shp")

# 任务2：导出各月人数。
agoop_amami %>%
  st_drop_geometry() %>%
  group_by(source, qua, month) %>%
  summarise(dailyid_num = length(unique(dailyid)), .groups = "drop") %>%
  write.csv("dailyid_num_local_tourist_month.csv")

# 任务3：分客源-季节的各指数箱型图。
lapply(
  c("indegree", "outdegree", "degree", "betweeness", "closeness", "harmonic"),
  function(x) {
    combined_data %>%
      left_join(st_drop_geometry(loc), by = c("id" = "loc_id")) %>%
      ggplot(aes(spa_group, get(x))) +
      geom_boxplot() +
      geom_jitter(aes(col = as.character(season)), alpha = 0.6, width = 0.1) +
      facet_wrap(.~ vis_src, ncol = 1) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(y = x, x = "Zone", col = "Season") +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90)
      )
  }
) %>%
  cowplot::plot_grid(plotlist = ., nrow = 2)

# 箱型图带点。
combined_data %>%
  left_join(st_drop_geometry(loc), by = c("id" = "loc_id")) %>%
  select(
    vis_src, spa_group, season, degree, betweeness, closeness, harmonic
  ) %>%
  pivot_longer(
    cols = c(degree, betweeness, closeness, harmonic),
    names_to = "index_cat", values_to = "index_val"
  ) %>%
  mutate(index_cat = factor(index_cat, levels = c(
    "degree", "betweeness", "closeness", "harmonic"
  ))) %>%
  ggplot(aes(spa_group, index_val, col = vis_src)) +
  geom_boxplot() +
  geom_jitter(size = 0.5, alpha = 0.6, width = 0.05, col = "grey") +
  facet_grid(index_cat ~ season, scale = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Index value", x = NULL, col = "Source") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90)
  )

# 箱型图不带点。
combined_data %>%
  left_join(st_drop_geometry(loc), by = c("id" = "loc_id")) %>%
  select(
    vis_src, spa_group, season, degree, betweeness, closeness, harmonic
  ) %>%
  pivot_longer(
    cols = c(degree, betweeness, closeness, harmonic),
    names_to = "index_cat", values_to = "index_val"
  ) %>%
  mutate(index_cat = factor(index_cat, levels = c(
    "degree", "betweeness", "closeness", "harmonic"
  ))) %>%
  ggplot(aes(spa_group, index_val, col = vis_src)) +
  geom_boxplot() +
  facet_grid(index_cat ~ season, scale = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Index value", x = NULL, col = "Source") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90)
  )

# 中位数条形图。
# 箱型图不带点。
combined_data %>%
  left_join(st_drop_geometry(loc), by = c("id" = "loc_id")) %>%
  select(
    vis_src, spa_group, season, degree, betweeness, closeness, harmonic
  ) %>%
  pivot_longer(
    cols = c(degree, betweeness, closeness, harmonic),
    names_to = "index_cat", values_to = "index_val"
  ) %>%
  group_by(vis_src, spa_group, season, index_cat) %>%
  summarise(index_val_mid = median(index_val), .groups = "drop") %>%
  mutate(index_cat = factor(index_cat, levels = c(
    "degree", "betweeness", "closeness", "harmonic"
  ))) %>%
  ggplot(aes(spa_group, index_val_mid, fill = vis_src)) +
  geom_col(position = "dodge") +
  facet_grid(index_cat ~ season, scale = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Index value median value", x = NULL, fill = "Source") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90)
  )

# 导出数据。
combined_data %>%
  left_join(st_drop_geometry(loc), by = c("id" = "loc_id")) %>%
  select(
    vis_src, spa_group, season, degree, betweeness, closeness, harmonic
  ) %>%
  write.csv("data_proc/loc_season_net_index.csv")
