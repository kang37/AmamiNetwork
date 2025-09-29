
vis_attr <-
  gis_agoop_coord %>%
  # Bug: Should ungroup earlier.
  ungroup() %>%
  st_drop_geometry() %>%
  select(
    dailyid, month, dayofweek, home_prefcode, home_citycode, gender
  ) %>%
  distinct()

traj_simp_attr <- traj_simp %>%
  left_join(vis_attr, by = "dailyid")

traj_simp_attr %>%
  group_by(traj_2, home_prefcode) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_point(aes(home_prefcode, n)) +
  facet_wrap(.~ traj_2, scales = "free")

vis_attr %>%
  group_by(month) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_col(aes(as.character(month), n))





ggplot() +
  geom_sf(data = amami) +
  geom_sf_label(
    data =
      filter(gis_agoop_coord_sample, cluster != 0) %>%
      group_by(cluster) %>%
      slice_head(n = 5),
    aes(col = as.character(cluster), label = cluster), alpha = 0.5
  )

