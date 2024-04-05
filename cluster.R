# Package ----
pacman::p_load(
  moveVis, move, lubridate, targets, dplyr, dbscan, sf, ggplot2, tidyr
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
  group_by(dailyid) %>%
  mutate(min_hour = min(hour), max_hour = max(hour)) %>%
  ungroup() %>%
  filter(min_hour <= 22, max_hour >= 4) %>%
  select(-min_hour, -max_hour)

# Trajectory anima ----
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
table(cluster_res$cluster)
gis_agoop_coord_sample_1 <- gis_agoop_coord_sample_1 %>%
  mutate(cluster = cluster_res$cluster) %>%
  filter(cluster != 0)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = gis_agoop_coord_sample_1 %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant"),
    aes(col = as.character(cluster)), alpha = 0.5
  )

# Further cluster c1.
gis_agoop_coord_sample_2 <- gis_agoop_coord_sample %>%
  # Need to check if the wanted cluster is picked.
  filter(cluster == 1)
# ggplot() +
#   geom_sf(data = amami) +
#   geom_sf(
#     data = gis_agoop_coord_sample_2 %>%
#       st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant"),
#     aes(col = as.character(cluster)), alpha = 0.5
#   )
# dbscan::kNNdistplot(gis_agoop_coord_sample_2[c("lon", "lat")], k = 300)
# abline(h = 0.01, lty = 2, col = rainbow(1), main = "eps optimal value")
cluster_res_c1 <-
  dbscan(gis_agoop_coord_sample_2[c("lon", "lat")], eps = 0.0053, minPts = 300)
cluster_res_c1
gis_agoop_coord_sample_2 <- gis_agoop_coord_sample_2 %>%
  mutate(cluster = cluster_res_c1$cluster) %>%
  filter(cluster != 0)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = gis_agoop_coord_sample_2 %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant"),
    aes(col = as.character(cluster)), alpha = 0.5
  )
# Bind results from 2 cluster.
gis_agoop_coord_sample <-
  rbind(
    gis_agoop_coord_sample_2,
    filter(gis_agoop_coord_sample_1, cluster != 1) %>%
      mutate(cluster = cluster + length(table(gis_agoop_coord_sample_2$cluster)) - 1)
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

# Plot with ggplot.
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = gis_agoop_coord_sample,
    aes(col = as.character(cluster)), alpha = 0.5
  ) +
  geom_sf_label(
    data =
      filter(gis_agoop_coord_sample, cluster != 0) %>%
      group_by(cluster) %>%
      slice_head(n = 1),
    aes(col = as.character(cluster), label = cluster), alpha = 0.5
  )
ggplot() +
  # geom_sf(data = amami) +
  geom_sf(
    data = gis_agoop_coord_sample %>%
      group_by(cluster) %>%
      slice_head(n = 50),
    aes(col = as.character(cluster)),
    alpha = 0.1
  ) +
  geom_sf_label(
    data =
      filter(gis_agoop_coord_sample, cluster != 0) %>%
      group_by(cluster) %>%
      slice_head(n = 1),
    aes(col = as.character(cluster), label = cluster), alpha = 0.5
  )
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = gis_agoop_coord_sample %>%
      group_by(cluster) %>%
      slice_sample(n = 5),
    aes(col = as.character(cluster)),
    alpha = 0.1
  ) +
  geom_sf_label(
    data =
      filter(gis_agoop_coord_sample, cluster != 0) %>%
      group_by(cluster) %>%
      slice_sample(n = 1),
    aes(col = as.character(cluster), label = cluster), alpha = 0.5
  )

# How to apply to the whole data?
# gis_agoop_coord <- gis_agoop_coord %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
#   mutate(cluster = cluster_res$cluster)
# ggplot() +
#   geom_sf(data = amami) +
#   geom_sf(
#     data = filter(gis_agoop_coord, cluster != 0),
#     aes(col = as.character(cluster)), alpha = 0.5
#   )

# Trajectory between clusters ----
# Should further divide segments: a dailyid has more than one segment even for a cluster. For instance, the pathway c1-c2-c1-c3 has 2 c1 segments.
# Should further divid segments: a dailyid has more than one segment even for a cluster. For instance, the pathway c1-c2-c1-c3 has 2 c1 segments.
# Bug: Should make a time column for the raw data before.
gis_agoop_coord_sample <- gis_agoop_coord_sample %>%
  mutate(time = as_datetime(
    paste(
      paste(year, month, day, sep = "-"),
      paste(hour, minute, "00", sep = "-"),
      sep = " "
    )
  )) %>%
  # Bug: Remove one-dailyid-in-two
  arrange(dailyid, time, accuracy) %>%
  group_by(dailyid, time) %>%
  mutate(time_conflict_id = row_number()) %>%
  filter(time_conflict_id == 1) %>%
  select(-time_conflict_id)

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

seg <- gis_agoop_coord_sample %>%
  st_drop_geometry() %>%
  arrange(dailyid, hour) %>%
  left_join(seg_id, by = c("dailyid", "cluster", "time")) %>%
  ungroup() %>%
  tidyr::fill(seg_id)

# Distribution of log numbers of each segment for the clusters.
# lapply(
#   0:10,
#   function(cluster_id) {
#     seg %>%
#       filter(cluster == cluster_id) %>%
#       group_by(dailyid, seg_id) %>%
#       summarise(n = n(), .groups = "drop") %>%
#       mutate(cluster_id = cluster_id)
#   }
# ) %>%
#   bind_rows() %>%
#   ggplot() +
#   geom_density(aes(n)) +
#   facet_wrap(.~ cluster_id, scales = "free")

# Distribution of stay time of each segment for the clusters.
lapply(
  0:17,
  function(cluster_id) {
    seg %>%
      filter(cluster == cluster_id) %>%
      group_by(dailyid, seg_id) %>%
      summarise(
        stay_time = max(time, na.rm = TRUE) - min(time, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(cluster_id = cluster_id)
  }
) %>%
  bind_rows() %>%
  ggplot(aes(as.character(cluster_id), stay_time)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.05)

# Bug: Take 15 minutes as a "visit", less than 15 minutes is "pass".
seg_time <- lapply(
  0:17,
  function(cluster_id) {
    seg %>%
      filter(cluster == cluster_id) %>%
      group_by(dailyid, seg_id) %>%
      summarise(stay_time = max(time) - min(time), .groups = "drop") %>%
      mutate(cluster_id = cluster_id)
  }
) %>%
  bind_rows() %>%
  # Only keep the "visit".
  # Bug: How to convert unit? If a dailyid only has 2 logs in a cluster, the stay time might be very low.
  filter(stay_time > 900)
# Bug: An assumption - visitors' next destination depends on last destination.

# Most visitors stay in a cluster.
seg_time %>%
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

# Movement matrix.
# Has the data.frame been arranged in order?
# Bug: What if a dailyid mostly stay in a cluster, but s/he goes to the edge usually? "c1-edge-c1-edge-c1". Should merge it as "c1-c1-c1" or "c1"?
seg_time_od <- seg_time %>%
  arrange(dailyid, seg_id) %>%
  group_by(dailyid) %>%
  mutate(
    destination = cluster_id, origin = lag(cluster_id)
  ) %>%
  # Bug: It is equal to that we remove the dailyid who stays in a cluster for the whole day.
  filter(!is.na(origin))
# Most movements are from c2 to c2, followed by c1-c2, c1-c1, c2-c8, c10-c10. c2 is an important center.
seg_time_od %>%
  group_by(origin, destination) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(origin, destination)) +
  geom_tile(aes(fill = log(n))) +
  scale_fill_gradient2(high = "red", mid = "white", low = "blue") +
  geom_text(aes(label = n), col = "grey", size = 3)
seg_time_od %>%
  group_by(origin, destination) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(-n) %>%
  mutate(row_id = row_number())

# Abstract trajectory.
# First simplification: a "vc1-r-vc1" or "vc1-rc2-vc1" will be simplified as "vc1-vc1", then further simplified as "vc1".

# Function to abstract a trajectory string. For example, a "vc1-vc2-vc1" is simplified as "0-1-0".
map_traj <- function(x) {
  x_split <- strsplit(x, "") %>% unlist()
  x_non_duplicate <- duplicated(x_split)
  x_map <- data.frame(ori = x_split[!x_non_duplicate]) %>%
    mutate(new = row_number() - 1)
  x_replace <- data.frame(ori = x_split) %>%
    left_join(x_map, by = "ori")
  res <- paste0(x_replace$new, collapse = "-")
  return(res)
}

# seg_time is logs of stay time larger than 15 min. Bug: Should remove the noise (cluster = 0)?
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
  mutate(traj_2 = lapply(traj_1, map_traj) %>% unlist())

table(traj_simp$traj_1) %>%
  data.frame() %>%
  tibble() %>%
  arrange(-Freq)

table(traj_simp$traj_2) %>%
  data.frame() %>%
  tibble() %>%
  arrange(-Freq)
# Most visitors go circle, including 1 or 2 or 3 points circles.

