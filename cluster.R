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
