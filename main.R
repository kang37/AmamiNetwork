# Package ----
pacman::p_load(
  moveVis, move, lubridate, dplyr, dbscan, sf,
  ggplot2, tidyr, RColorBrewer, targets
)
tar_make()
tar_load(amami)
tar_load(gis_agoop_coord)
# Min start hour and max end hour of the dailyid.
gis_agoop_coord %>%
  group_by(dailyid) %>%
  summarise(min_hour = min(hour)) %>%
  ungroup() %>%
  ggplot() +
  geom_histogram(aes(min_hour))
gis_agoop_coord %>%
  group_by(dailyid) %>%
  summarise(max_hour = max(hour)) %>%
  ungroup() %>%
  ggplot() +
  geom_histogram(aes(max_hour))
# Conclusion: most dailyid start at 0 and end at 24. Can only keep the dailyid with min start time <= 22 and max end time >= 4.

gis_agoop_coord <- gis_agoop_coord %>%
  # Keep trajectory points between 4 and 22 everyday.
  filter(hour <= 22, hour >= 4) %>%
  # Make date time.
  mutate(time = as_datetime(
    paste(
      paste(year, month, day, sep = "-"),
      paste(hour, minute, "00", sep = "-"),
      sep = " "
    )
  )) %>%
  # If 2 points at the same time, keep only one with higher accuracy.
  arrange(dailyid, time, accuracy) %>%
  group_by(dailyid, time) %>%
  mutate(position_conflict_id = row_number()) %>%
  ungroup() %>%
  filter(position_conflict_id == 1) %>%
  select(-position_conflict_id)

# Trajectory animation ----
# Bug: Cluster based on visitor data, while trajectory anima based on whole data.
# Function to get trajectory_animation.gif by dailyid.
# get_anim <- function(dailyid_id) {
#   # Make move file with GIS data of a dailyid.
#   gis_agoop_head <- gis_agoop_ %>%
#     filter(dailyid == temp_id_num$dailyid[dailyid_id]) %>%
#     st_transform(4326)
#   gis_agoop_head_coord <- st_coordinates(gis_agoop_head) %>%
#     data.frame() %>%
#     tibble() %>%
#     rename_with(~c("x", "y"))
#   gis_agoop_head <-
#     gis_agoop_head %>%
#     cbind(gis_agoop_head_coord) %>%
#     mutate(time = as_datetime(
#       paste(
#         paste(year, month, day, sep = "-"),
#         paste(hour, minute, "00", sep = "-"),
#         sep = " "
#       )
#     )) %>%
#     arrange(time) %>%
#     # Keep only one log for a unique hour-minute.
#     # Bug: Should group by time (minute).
#     group_by(hour) %>%
#     mutate(rowid = row_number()) %>%
#     ungroup() %>%
#     filter(rowid == 1)
#   move_data <- move(
#     x = gis_agoop_head$x, y = gis_agoop_head$y,
#     time = gis_agoop_head$time,
#     proj = projection(gis_agoop_head)
#   )
#
#   # Align move_data to a uniform time scale.
#   m <- align_move(move_data, res = "mean")
#
#   frames <- frames_spatial(m, alpha = 0.5) %>%
#     add_labels(x = "Longitude", y = "Latitude") %>%
#     add_northarrow() %>%
#     add_scalebar() %>%
#     add_timestamps(type = "label") %>%
#     add_progress()
#
#   # Animate frames.
#   animate_frames(
#     frames, out_file = paste0("data_edited/", dailyid_id, ".gif"),
#     display = FALSE, fps = 1
#   )
# }
# Export 1-50 examples, some of them are not valid.
# get_anim(1)

# Cluster ----
# Clusters of logs.
# Bug: Need to determine minPts and eps first, manually. If k is larger, the calc is slower. The following plot takes 2 min.
# Bug: Take sample for clustering.
set.seed(1234)
gis_agoop_coord_sample_1 <-
  gis_agoop_coord[sample(nrow(gis_agoop_coord), nrow(gis_agoop_coord) / 10), ]
dbscan::kNNdistplot(gis_agoop_coord_sample_1[c("lon", "lat")], k = 300)
abline(h = 0.01, lty = 2, col = rainbow(1), main = "eps optimal value")
cluster_res <-
  dbscan(gis_agoop_coord_sample_1[c("lon", "lat")], eps = 0.01, minPts = 300)
cluster_res
gis_agoop_coord_sample_1 <- gis_agoop_coord_sample_1 %>%
  mutate(cluster = cluster_res$cluster)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = gis_agoop_coord_sample_1 %>%
      filter(cluster != 0) %>%
      group_by(cluster) %>%
      slice_sample(n = 100) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant"),
    aes(col = as.character(cluster)), alpha = 0.5
  ) +
  geom_sf_label(
    data = gis_agoop_coord_sample_1 %>%
      filter(cluster != 0) %>%
      group_by(cluster) %>%
      slice_sample(n = 1) %>%
      ungroup() %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant"),
    aes(label = cluster, col = as.character(cluster)), alpha = 0.9
  )

# Further cluster c1.
gis_agoop_coord_sample_2 <- gis_agoop_coord_sample_1 %>%
  # Need to check if the wanted cluster is picked.
  filter(cluster == 1)
# dbscan::kNNdistplot(gis_agoop_coord_sample_2[c("lon", "lat")], k = 300)
# abline(h = 0.01, lty = 2, col = rainbow(1), main = "eps optimal value")
cluster_res_c1 <-
  dbscan(gis_agoop_coord_sample_2[c("lon", "lat")], eps = 0.007, minPts = 300)
cluster_res_c1
gis_agoop_coord_sample_2 <- gis_agoop_coord_sample_2 %>%
  mutate(cluster = cluster_res_c1$cluster) %>%
  mutate(cluster = cluster + 10)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = gis_agoop_coord_sample_2 %>%
      filter(cluster != 10) %>%
      group_by(cluster) %>%
      slice_sample(n = 1000) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant"),
    aes(col = as.character(cluster)), alpha = 0.1
  )
# Bind results from 2 cluster.
gis_agoop_coord_sample <-
  rbind(
    gis_agoop_coord_sample_2,
    filter(gis_agoop_coord_sample_1, cluster != 1)
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

# Plot with ggplot.
# Bug: Need to identify if the clusters are overlap. This time, 3 and 9 are overlap.
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = gis_agoop_coord_sample %>%
      filter(cluster != 0, cluster != 10) %>%
      group_by(cluster) %>%
      slice_sample(n = 1000),
    aes(col = as.character(cluster)), alpha = 0.1
  ) +
  geom_sf_label(
    data =
      filter(gis_agoop_coord_sample, cluster != 0, cluster != 10) %>%
      group_by(cluster) %>%
      slice_head(n = 1),
    aes(col = as.character(cluster), label = cluster),
    alpha = 0.7, size = 2.5
  ) +
  theme_bw()

# Change cluster id: from north to south, and from east to west.
map_cluster_id <-
  c(
    0, 10, 11, 9, 6, 13, 12, 14, 2, 15, 16, 4, 8, 3, 5, 7,
    0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14
  ) %>%
  matrix(ncol = 2) %>%
  data.frame() %>%
  rename_with(~ c("cluster", "new_cluster"))

gis_agoop_coord_sample <- gis_agoop_coord_sample %>%
  left_join(map_cluster_id, by = "cluster") %>%
  select(-cluster) %>%
  rename(cluster = new_cluster) %>%
  mutate(cluster = factor(cluster, levels = as.character(0:15)))

# Plot with ggplot.
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = gis_agoop_coord_sample %>%
      filter(cluster != 0) %>%
      group_by(cluster) %>%
      slice_sample(n = 3000),
    aes(col = cluster), alpha = 0.1
  ) +
  geom_sf_label(
    data =
      filter(gis_agoop_coord_sample, cluster != 0) %>%
      group_by(cluster) %>%
      slice_head(n = 1),
    aes(col = cluster, label = cluster), alpha = 0.7, size = 2.5
  ) +
  scale_color_manual(values = rep(brewer.pal(9, "Dark2"), 3)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(legend.position = "none")

# General description ----
gis_agoop_coord_sample %>%
  left_join(vis_attr) %>%
  st_drop_geometry() %>%
  group_by(season) %>%
  summarise(dailyid_num = length(unique(dailyid)), .groups = "drop")

# Trajectory between clusters ----
# Further divide segments: a dailyid has more than one segment even for a cluster. For instance, the pathway c1-c2-c1-c3 has 2 c1 segments.
# Should further divid segments: a dailyid has more than one segment even for a cluster. For instance, the pathway c1-c2-c1-c3 has 2 c1 segments.
# Get segment ID for each dailyid.
seg_id <- gis_agoop_coord_sample %>%
  st_drop_geometry() %>%
  arrange(dailyid, time) %>%
  group_by(dailyid) %>%
  mutate(
    new_dailyid = c(dailyid != lag(dailyid)),
    new_cluster = c(cluster != lag(cluster)),
    new_dailyid = case_when(is.na(new_dailyid) ~ TRUE, TRUE ~ new_dailyid),
    new_cluster = case_when(is.na(new_cluster) ~ TRUE, TRUE ~ new_cluster)
  ) %>%
  select(dailyid, time, cluster, new_dailyid, new_cluster) %>%
  filter(new_dailyid + new_cluster >= 1) %>%
  mutate(seg_id = row_number())

# The duration of stay of each dailyid in each segment.
seg <- gis_agoop_coord_sample %>%
  # Add segment sequence.
  st_drop_geometry() %>%
  arrange(dailyid, hour) %>%
  left_join(seg_id, by = c("dailyid", "cluster", "time")) %>%
  ungroup() %>%
  tidyr::fill(seg_id) %>%
  # Calculate duration of stay.
  group_by(dailyid, cluster, seg_id) %>%
  summarise(duration_stay = max(time) - min(time), .groups = "drop") %>%
  # Remove noise points.
  filter(cluster != 0) %>%
  # Bug: Duration of stat = 0 also excluded, might introduce bias from signal lose, i.e., only one points is recorded in a cluster.
  filter(duration_stay > 600) %>%
  # Turn duration of stay from second to minute.
  mutate(duration_stay = as.numeric(duration_stay) / 60)

# Plot duration of stay of each cluster, based on segment duration stay data.
ggplot(data = seg, aes(cluster, duration_stay)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01) +
  labs(x = "Cluster", y = "Duration of stay")
# Table.
seg %>%
  group_by(cluster) %>%
  summarise(
    mean_dur_stay = mean(duration_stay, na.rm = TRUE),
    mid_dur_stay = median(duration_stay, na.rm = TRUE),
    sd_dur_stay = sd(duration_stay, na.rm = TRUE),
    n_dur_stay = n()
  ) %>%
  mutate(
    se_dur_stay = sd_dur_stay / sqrt(n_dur_stay),
    lower_ci =
      mean_dur_stay - qt(1 - (0.05 / 2), n_dur_stay - 1) * se_dur_stay,
    upper_ci =
      mean_dur_stay + qt(1 - (0.05 / 2), n_dur_stay - 1) * se_dur_stay
  ) %>%
  select(cluster, mid_dur_stay, mean_dur_stay, lower_ci, upper_ci, sd_dur_stay)

# Most visitors stay in a cluster; segment visited is similar to cluster visited number.
seg %>%
  group_by(dailyid) %>%
  summarise(seg_n = n(), .groups = "drop") %>%
  ggplot() +
  geom_histogram(aes(seg_n), binwidth = 1)
seg %>%
  select(dailyid, seg_id) %>%
  distinct() %>%
  group_by(dailyid) %>%
  summarise(cluster_n = n(), .groups = "drop") %>%
  ggplot() +
  geom_histogram(aes(cluster_n), binwidth = 1)

# In each mode, what is the structure?
# Bug: Take cluster 1 as an example.
seg_time %>%
  left_join(
    seg_time %>%
      group_by(dailyid) %>%
      summarise(cluster_n = n(), .groups = "drop") %>%
      filter(cluster_n == 1),
    by = "dailyid"
  ) %>%
  filter(!is.na(cluster_n)) %>%
  pull(seg_id) %>%
  table()

# Pairwise movement matrix.
# Has the data.frame been arranged in order?
# If a dailyid mostly stay in a cluster, but s/he goes out of the cluster usually? Then "c1-edge-c1-edge-c1" will become "c1".
# Bug: It is equal to that we remove the dailyid who stays in a cluster for the whole day.
seg_pair_od <- seg %>%
  arrange(dailyid, seg_id) %>%
  mutate(
    new_dailyid = c(dailyid != lag(dailyid)),
    new_cluster = c(cluster != lag(cluster)),
    new_dailyid = case_when(is.na(new_dailyid) ~ TRUE, TRUE ~ new_dailyid),
    new_cluster = case_when(is.na(new_cluster) ~ TRUE, TRUE ~ new_cluster),
    # If the answer is yes to either question, then the state is changed.
    state_chg = c(new_dailyid | new_cluster)
  ) %>%
  filter(state_chg) %>%
  group_by(dailyid) %>%
  mutate(
    destination = cluster, origin = lag(cluster)
  ) %>%
  filter(!is.na(origin))

# Most movements are from c2 to c2, followed by c1-c2, c1-c1, c2-c8, c10-c10. c2 is an important center.
seg_pair_od %>%
  group_by(origin, destination) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(origin, destination)) +
  geom_tile(aes(fill = log(n)), col = "black") +
  theme_bw() +
  scale_fill_gradient2(high = "red", mid = "white", low = "blue") +
  # scale_x_continuous(breaks = seq(1, 18, 2)) +
  # scale_y_continuous(breaks = seq(1, 18, 2)) +
  theme(axis.ticks.x = element_blank()) +
  geom_text(aes(label = n), col = "black", size = 3)

# Abstract trajectory.
# First simplification: a "vc1-r-vc1" or "vc1-rc2-vc1" will be simplified as "vc1-vc1", then further simplified as "vc1".

# Function to abstract a trajectory string. For example, a "vc1-vc2-vc1" is simplified as "0-1-0".
map_traj <- function(x) {
  x_split <- strsplit(x, "-") %>% unlist()
  x_non_duplicate <- duplicated(x_split)
  x_map <- data.frame(ori = x_split[!x_non_duplicate]) %>%
    mutate(new = row_number() - 1)
  x_replace <- data.frame(ori = x_split) %>%
    left_join(x_map, by = "ori")
  res <- paste0(x_replace$new, collapse = "-")
  return(res)
}

# seg_time is logs of stay time larger than 15 min.
# Bug: Should remove the noise (cluster = 0)?
merge_traj <- function(x) {
  # Split the character vector into individual digits.
  neighborhood_digits <- strsplit(x, "-")[[1]]
  # Remove consecutive duplicates.
  unique_digits <- c(
    neighborhood_digits[1],
    neighborhood_digits[-1][
      neighborhood_digits[-1] !=
        neighborhood_digits[-length(neighborhood_digits)]
    ]
  )
  # Combine the unique digits back into a single number
  merged_number <- paste(unique_digits, collapse = "-")
  return(merged_number)
}

traj_simp <- seg_time %>%
  arrange(dailyid, seg_id) %>%
  mutate(
    # If it is a new dailyid, or if it enters a new cluster.
    new_dailyid = c(dailyid != lag(dailyid)),
    new_cluster_id = c(cluster_id != lag(cluster_id)),
    new_dailyid =
      case_when(is.na(new_dailyid) ~ TRUE, TRUE ~ new_dailyid),
    new_cluster_id =
      case_when(is.na(new_cluster_id) ~ TRUE, TRUE ~ new_cluster_id),
    # If the answer is yes to either question, then the state is changed.
    state_chg = c(new_dailyid | new_cluster_id)
  ) %>%
  # Every time the state changes, assign a new visit ID. So for the non-changed rows, assgin NA then fill them with the changed visit ID.
  group_by(dailyid, state_chg) %>%
  mutate(visit_id = row_number()) %>%
  ungroup() %>%
  mutate(visit_id = case_when(
    state_chg ~ visit_id, !state_chg ~ NA
  )) %>%
  fill(visit_id) %>%
  # Make trajectory string.
  select(dailyid, visit_id, cluster_id) %>%
  distinct() %>%
  mutate(cluster_id = as.character(cluster_id)) %>%
  group_by(dailyid) %>%
  summarise(traj_1 = paste0(cluster_id, collapse = "-")) %>%
  ungroup() %>%
  # Further abstract trajectory. For example, a "c1-c2-c1" will be "0-1-0".
  mutate(traj_2 = lapply(traj_1, map_traj) %>% unlist()) %>%
  # Further merge traj_2.
  mutate(traj_3 = lapply(traj_2, merge_traj) %>% unlist())

table(traj_simp$traj_1) %>%
  data.frame() %>%
  tibble() %>%
  arrange(-Freq)

table(traj_simp$traj_2) %>%
  data.frame() %>%
  tibble() %>%
  arrange(-Freq)

top_traj_3 <-
  table(traj_simp$traj_3) %>%
  data.frame() %>%
  tibble() %>%
  arrange(-Freq) %>%
  head(15) %>%
  rename_with(~ c("traj", "freq")) %>%
  mutate(traj = factor(traj, levels = .$traj))
ggplot(top_traj_3) +
  geom_col(aes(traj, log(freq))) +
  labs(x = "Trajectory mode", y = "log(Frequency)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# Further explore some mode.
for (i in top_traj_3$traj) {
  print(i)
  traj_simp %>%
    filter(traj_3 == i) %>%
    group_by(traj_1) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(-n) %>%
    head(10) %>%
    print()
}
# Most visitors go circle, including 1 or 2 or 3 points circles.

# OD pair diff ----
vis_attr <-
  gis_agoop_coord %>%
  # Bug: Should ungroup earlier.
  ungroup() %>%
  st_drop_geometry() %>%
  select(
    dailyid, month, dayofweek, home_prefcode, home_citycode, gender
  ) %>%
  distinct() %>%
  mutate(
    season = case_when(
      month >= 10 ~ "q4",
      month >= 7 ~ "q3",
      month >= 4  ~ "q2",
      month >= 1 ~ "q1"
    ),
    home_pref_grp = case_when(
      home_prefcode == "46" ~ "local", home_prefcode != "46" ~ "visitor"
    )
    # Bug: Why no Amamia residents in the Amami data??
    # home_pref_grp = case_when(
    #   home_citycode %in% c(46222, "46523", "46524", "46525", "46527") ~ "local",
    #   TRUE ~ "visitor"
    # )
  )


seg_pair_od_local_prop <-
  seg_pair_od %>%
  left_join(vis_attr) %>%
  filter(!is.na(home_pref_grp)) %>%
  group_by(home_pref_grp, origin, destination) %>%
  summarise(n = n(), .groups = "drop") %>%
  # 计算每对出行占所有出行配对的百分比。
  group_by(home_pref_grp) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()
# 可视化。
seg_pair_od_local_prop %>%
  ggplot(aes(origin, destination)) +
  geom_tile(aes(fill = prop), col = "black") +
  theme_bw() +
  scale_fill_gradient2(high = "red", mid = "white", low = "blue") +
  theme(axis.ticks.x = element_blank()) +
  geom_text(aes(label = round(prop, 2)*100), col = "black", size = 3) +
  facet_wrap(.~ home_pref_grp, scales = "free")

# 计算基尼指数和SD。
seg_pair_od_local_prop %>%
  group_by(home_pref_grp) %>%
  summarise(
    prop_gini = Gini(prop), prop_sd = sd(prop), .groups = "drop"
  )

# Pairs of OD by local/visitor and season.
seg_pair_od_local_quarter_prop <-
  seg_pair_od %>%
  # filter(!c(destination == "4" & origin == "5")) %>%
  # filter(!c(destination == "5" & origin == "4")) %>%
  left_join(vis_attr) %>%
  filter(!is.na(home_pref_grp)) %>%
  group_by(home_pref_grp, season, origin, destination) %>%
  summarise(n = n(), .groups = "drop") %>%
  # 计算每对出行占所有出行配对的百分比。
  group_by(home_pref_grp, season) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# 可视化。
seg_pair_od_local_quarter_prop %>%
  ggplot(aes(origin, destination)) +
  geom_tile(aes(fill = prop), col = "black") +
  theme_bw() +
  scale_fill_gradient2(high = "red", mid = "white", low = "blue") +
  theme(axis.ticks.x = element_blank()) +
  geom_text(aes(label = round(prop, 2)*100), col = "black", size = 3) +
  facet_grid(season ~ home_pref_grp)

# 计算基尼指数。
library(DescTools)
seg_pair_od_prop %>%
  group_by(home_pref_grp, season) %>%
  summarise(prop_gini = Gini(prop), .groups = "drop") %>%
  ggplot() +
  geom_line(aes(season, prop_gini, col = home_pref_grp, group = home_pref_grp))
# 计算标准差。
seg_pair_od_prop %>%
  group_by(home_pref_grp, season) %>%
  summarise(prop_sd = sd(prop), .groups = "drop") %>%
  ggplot() +
  geom_line(aes(season, prop_sd, col = home_pref_grp, group = home_pref_grp))

# OD network ----
# Export csv file for Gephi.
seg_pair_od %>%
  ungroup() %>%
  select(Source = origin, Target = destination) %>%
  write.csv("od_edge.csv", row.names = FALSE)
st_coordinates(gis_agoop_coord_sample) %>%
  data.frame() %>%
  rename_with(~ c("longitude", "latitude")) %>%
  mutate(cluster = gis_agoop_coord_sample$cluster) %>%
  group_by(cluster) %>%
  summarise(longitude = median(longitude), latitude = median(latitude)) %>%
  rename(Id = cluster, Latitude = latitude, Longitude = longitude) %>%
  write.csv("od_node.csv", row.names = FALSE)
# Manually make network plots in Gephi.

# Trajectory by visitor attr ----
traj_simp_attr <- traj_simp %>%
  left_join(vis_attr, by = "dailyid")

traj_simp_attr %>%
  group_by(traj_3, season) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_col(aes(traj_3, log(n), fill = as.character(season))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

traj_simp_attr %>%
  group_by(traj_3, gender) %>%
  summarise(n = n()) %>%
  filter(n > 10) %>%
  ggplot() +
  geom_point(aes(gender, n)) +
  facet_wrap(.~ traj_3, scales = "free")

traj_simp_attr %>%
  group_by(traj_3, gender) %>%
  summarise(n = n()) %>%
  filter(n > 10) %>%
  ggplot() +
  geom_point(aes(gender, n)) +
  facet_wrap(.~ traj_3, scales = "free")

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
