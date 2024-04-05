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
gis_agoop_coord_sample <-
  gis_agoop_coord[sample(nrow(gis_agoop_coord), nrow(gis_agoop_coord) / 10), ]
dbscan::kNNdistplot(gis_agoop_coord_sample[c("lon", "lat")], k = 300)
abline(h = 0.01, lty = 2, col = rainbow(1), main = "eps optimal value")
cluster_res <-
  dbscan(gis_agoop_coord_sample[c("lon", "lat")], eps = 0.01, minPts = 300)
cluster_res
plot(
  gis_agoop_coord_sample$lon, gis_agoop_coord_sample$lat,
  col = cluster_res$cluster + 1L
)
# Eliminate the noise.
plot(
  gis_agoop_coord$lon, gis_agoop_coord$lat,
  col = cluster_res$cluster
)
table(cluster_res$cluster)
# Plot with ggplot.
gis_agoop_coord_sample <- gis_agoop_coord_sample %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
  mutate(cluster = cluster_res$cluster)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = filter(gis_agoop_coord_sample, cluster != 0),
    aes(col = as.character(cluster)), alpha = 0.5
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



