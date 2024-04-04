# Package ----
library(moveVis)
library(move)
library(lubridate)

# Trajectory anima ----
# Bug: Cluster based on visitor data, while trajectory anima based on whole data.
# Function to get trajectory_animation.gif by dailyid.
get_anim <- function(dailyid_id) {
  # Make move file with GIS data of a dailyid.
  gis_agoop_smp_head <- gis_agoop_smp %>%
    filter(dailyid == temp_id_num$dailyid[dailyid_id]) %>%
    st_transform(4326)
  gis_agoop_smp_head_coord <- st_coordinates(gis_agoop_smp_head) %>%
    data.frame() %>%
    tibble() %>%
    rename_with(~c("x", "y"))
  gis_agoop_smp_head <-
    gis_agoop_smp_head %>%
    cbind(gis_agoop_smp_head_coord) %>%
    mutate(time = as_datetime(
      paste(
        paste(year, month, day, sep = "-"),
        paste(hour, minute, "00", sep = "-"),
        sep = " "
      )
    )) %>%
    arrange(time) %>%
    # Keep only one log for a unique hour-minute.
    # Bug: Should group by time (minute).
    group_by(hour) %>%
    mutate(rowid = row_number()) %>%
    ungroup() %>%
    filter(rowid == 1)
  move_data <- move(
    x = gis_agoop_smp_head$x, y = gis_agoop_smp_head$y,
    time = gis_agoop_smp_head$time,
    proj = projection(gis_agoop_smp_head)
  )

  # Align move_data to a uniform time scale.
  m <- align_move(move_data, res = "mean")

  frames <- frames_spatial(m, alpha = 0.5) %>%
    add_labels(x = "Longitude", y = "Latitude") %>%
    add_northarrow() %>%
    add_scalebar() %>%
    add_timestamps(type = "label") %>%
    add_progress()

  # Animate frames.
  animate_frames(
    frames, out_file = paste0("data_edited/", dailyid_id, ".gif"),
    display = FALSE, fps = 1
  )
}
# Export 1-50 examples, some of them are not valid.
# get_anim(1)

# Cluster ----
# Clusters of logs.
library(dbscan)
library(targets)
tar_load(pref_city_code)

city_code_in_amami <-
  data.frame(
    city_in_amami = c(
      "奄美市", "大和村", "宇検村", "瀬戸内町", "龍郷町", "喜界町",
      "徳之島町", "天城町", "伊仙町", "和泊町", "知名町", "与論町"
    )
  ) %>%
  left_join(pref_city_code, by = c("city_in_amami" = "cityname")) %>%
  pull(citycode)

gis_agoop_smp_coord <- gis_agoop_smp %>%
  # Bug: Need to eliminate local residents for the raw data?
  filter(home_citycode %in% city_code_in_amami) %>%
  st_transform(4326)
gis_agoop_smp_coord <-
  cbind(
    st_drop_geometry(gis_agoop_smp_coord),
    st_coordinates(gis_agoop_smp_coord) %>%
      matrix(ncol = 2) %>%
      data.frame() %>%
      tibble() %>%
      rename_with(~ c("lon", "lat"))
  ) %>%
  tibble() %>%
  # Bug: Eliminate logs out of the island. Should have done that for the raw data.
  filter(lat > 27.06, lat < 28.66)
# Bug: Need to determine minPts and eps first, manually. If k is larger, the calc is slower.
dbscan::kNNdistplot(gis_agoop_smp_coord[c("lon", "lat")],k = 300)
abline(h = 0.01, lty = 2, col = rainbow(1), main = "eps optimal value")
cluster_res <- dbscan(gis_agoop_smp_coord[c("lon", "lat")], eps = 0.01, minPts = 300)
cluster_res
plot(
  gis_agoop_smp_coord$lon, gis_agoop_smp_coord$lat,
  col = cluster_res$cluster + 1L
)
# Eliminate the noise.
plot(
  gis_agoop_smp_coord$lon, gis_agoop_smp_coord$lat,
  col = cluster_res$cluster
)
table(cluster_res$cluster)

gis_agoop_smp_coord <- gis_agoop_smp_coord %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
  mutate(cluster = cluster_res$cluster)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = filter(gis_agoop_smp_coord, cluster != 0),
    aes(col = as.character(cluster)), alpha = 0.5
  )

# Trajectory between clusters ----
# About 500 dailyid in the data. What are their average stay time?
# Bug: How to id stay or pass? Method 1: by personal proportion. For example, if one stay in a place much of a day, then s/he stays there; if the proportion is only 1%, then it is pass. But problem: you only need to spend 10 min visiting a visitor center, isn't that "stay"?
gis_agoop_smp_coord %>%
  st_drop_geometry() %>%
  filter(dailyid == temp_id_num$dailyid[2]) %>%
  group_by(cluster) %>%
  summarise(prop = n(), .groups = "drop") %>%
  reframe(prop = prop / sum(prop))

# Method 2: time distribution of all visitors in a cluster.
# Bug: Aproximate stay time with the number of logs. Density of log numbers of each dailyid in a cluster.
lapply(
  0:10,
  function(cluster_id) {
    gis_agoop_smp_coord %>%
      st_drop_geometry() %>%
      filter(cluster == cluster_id) %>%
      group_by(dailyid) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(cluster_id = cluster_id)
  }
) %>%
  bind_rows() %>%
  ggplot() +
  geom_density(aes(n)) +
  facet_wrap(.~ cluster_id, scales = "free")
# Seems there is no obvious diff between "pass" and "stay."

# Should further divide segments: a dailyid has more than one segment even for a cluster. For instance, the pathway c1-c2-c1-c3 has 2 c1 segments.
# Should further divid segments: a dailyid has more than one segment even for a cluster. For instance, the pathway c1-c2-c1-c3 has 2 c1 segments.
# Bug: Should make a time column for the raw data before.
gis_agoop_smp_coord <- gis_agoop_smp_coord %>%
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
seg_id <- gis_agoop_smp_coord %>%
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

seg <- gis_agoop_smp_coord %>%
  st_drop_geometry() %>%
  arrange(dailyid, hour) %>%
  left_join(seg_id, by = c("dailyid", "cluster", "time")) %>%
  ungroup() %>%
  fill(seg_id)

library(tidyr)
# Distribution of log numbers of each segment for the clusters.
lapply(
  0:10,
  function(cluster_id) {
    seg %>%
      filter(cluster == cluster_id) %>%
      group_by(dailyid, seg_id) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(cluster_id = cluster_id)
  }
) %>%
  bind_rows() %>%
  ggplot() +
  geom_density(aes(n)) +
  facet_wrap(.~ cluster_id, scales = "free")

# Distribution of stay time of each segment for the clusters.
lapply(
  0:10,
  function(cluster_id) {
    seg %>%
      filter(cluster == cluster_id) %>%
      group_by(dailyid, seg_id) %>%
      summarise(stay_time = max(time) - min(time), .groups = "drop") %>%
      mutate(cluster_id = cluster_id)
  }
) %>%
  bind_rows() %>%
  ggplot(aes(as.character(cluster_id), stay_time)) +
  geom_boxplot() +
  geom_jitter()

# Bug: Take 15 minutes as a "visit", less than 15 minutes is "pass".
seg_time <- lapply(
  0:10,
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
  geom_histogram(aes(cluster_n))

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
  geom_text(aes(label = n), col = "white")

# Abstract trajectory.
# Function to id segments according to if a statement is "changed" or "not-changed".
id_seg <- function(x) {
  # Order x by id, time, etc.
  x_ordered <- arrange(x, dailyid, seg_id)
  # Id if the target statement is changed or not.
  x_chg <- x_ordered %>%
    mutate(
      new_dailyid = c(dailyid != lag(dailyid)),
      new_cluster_id = c(cluster_id != lag(cluster_id)),
      new_dailyid = case_when(is.na(new_dailyid) ~ TRUE, TRUE ~ new_dailyid),
      new_cluster_id = case_when(is.na(new_cluster_id) ~ TRUE, TRUE ~ new_cluster_id)
    ) %>%
    filter(new_dailyid + new_cluster_id >= 1) %>%
    group_by(dailyid) %>%
    mutate(segment_id = row_number()) %>%
    ungroup()
  # Add segment information to original data.
  res <- x_ordered %>%
    left_join(x_chg, by = c("dailyid", "seg_id", "cluster_id")) %>%
    fill(segment_id)
  return(res)
}

seg_time_simp1 <- id_seg(seg_time) %>%
  select(dailyid, segment_id, cluster_id) %>%
  distinct() %>%
  mutate(cluster_id = as.character(cluster_id)) %>%
  group_by(dailyid) %>%
  summarise(cluster_id = paste0(cluster_id, collapse = ""))
head(seg_time_simp1$cluster_id)
table(seg_time_simp1$cluster_id) %>%
  data.frame() %>%
  tibble() %>%
  arrange(-Freq)

# Further abstract trajectory. For example, "c2-c1-c2" is simplified to "0-1-0".
seg_id2 <- id_seg(seg_time) %>%
  select(dailyid, cluster_id) %>%
  group_by(dailyid) %>%
  mutate(cluster_duplicate = duplicated(cluster_id)) %>%
  filter(!cluster_duplicate) %>%
  mutate(cluster_id_simp2 = row_number() - 1)
View(seg_id2)
seg_time_simp2 <- id_seg(seg_time) %>%
  select(dailyid, segment_id, cluster_id) %>%
  left_join(seg_id2) %>%
  mutate(cluster_id_simp2 = as.character(cluster_id_simp2)) %>%
  group_by(dailyid) %>%
  summarise(cluster_id_simp2 = paste0(cluster_id_simp2, collapse = ""))
head(seg_time_simp2$cluster_id_simp2)
table(seg_time_simp2$cluster_id_simp2) %>%
  data.frame() %>%
  tibble() %>%
  arrange(-Freq)
# Most visitors goes circle, including 1 or 2 or 3 points circles.

