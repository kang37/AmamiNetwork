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
