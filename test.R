seg %>%
  left_join(select(vis_attr, dailyid, home_prefcode), by = "dailyid") %>%
  group_by(dailyid, home_prefcode) %>%
  summarise(seg_n = n(), .groups = "drop") %>%
  group_by(seg_n, home_prefcode) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot() +
  geom_col(aes(seg_n, n, fill = as.character(home_prefcode)), position = "fill")

seg %>%
  left_join(select(vis_attr, dailyid, home_prefcode), by = "dailyid") %>%
  group_by(dailyid, home_prefcode) %>%
  summarise(seg_n = n(), .groups = "drop") %>%
  group_by(seg_n, home_prefcode) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot() +
  geom_tile(aes(seg_n, home_prefcode, fill = log(n))) +
  scale_fill_gradient(low = "green", high = "red")

# 如果按照比例来呢？
seg %>%
  left_join(select(vis_attr, dailyid, home_prefcode), by = "dailyid") %>%
  group_by(dailyid, home_prefcode) %>%
  summarise(seg_n = n(), .groups = "drop") %>%
  group_by(seg_n, home_prefcode) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(home_prefcode) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(home_prefcode, prop)) +
  facet_wrap(.~ seg_n, scales = "free_y")





dailyid_seg_n <- seg %>%
  select(dailyid, seg_id) %>%
  distinct() %>%
  group_by(dailyid) %>%
  summarise(cluster_n = n(), .groups = "drop")

traj_simp_attr <- traj_simp %>%
  left_join(dailyid_seg_n, by = "dailyid")

View(traj_simp_attr)

traj_simp %>%
  filter(seg_n == 3) %>%
  pull(traj_3) %>%
  unique()

# 各种seg_n对应几种motif？
traj_simp %>%
  select(seg_n, traj_3) %>%
  distinct() %>%
  group_by(seg_n) %>%
  summarise(n = n(), .groups = "drop")








