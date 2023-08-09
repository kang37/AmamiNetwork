# Package ----
pacman::p_load(
  tibble, dplyr, lubridate, purrr, mapview, sf, tmap, tmaptools, units, showtext
)
showtext_auto()

# Analysis ----
# The basic idea is to make road lines based on the GPS point data. But as we know, for a road, there might be multiple GPS point data files since there are usually more than one experiment trial for one road. So I get the GPS file with most points first, then make the road lines.

# Read the data of the GPS points of all roads. The raw data comes from the GPS data of the rabbit experiment.
gpx_dt_sub <- readRDS("data_raw/road_raw/gpx_dt_sub.rds")

# Make road map data.
# Firstly, we make a function that can make road line based on track points.
# The arguments are:
# prop: the proportion to keep the track points;
# from: keep the track points from top ("top") or from end ("end").
make_road_line <- function(road_name, track_file_name, prop, from) {
  tar_gpx_dt_sub <- gpx_dt_sub %>%
    filter(track_file == track_file_name, road == road_name)
  if (from == "top") {
    tar_gpx_dt_sub <- tar_gpx_dt_sub[1:round(prop * nrow(tar_gpx_dt_sub)), ]
  } else {
    tar_gpx_dt_sub <- tar_gpx_dt_sub[
      nrow(tar_gpx_dt_sub): round(prop * nrow(tar_gpx_dt_sub)), ]
  }
  points = tar_gpx_dt_sub$geometry

  # Number of total linestrings to be created
  n <- length(points) - 1
  # Build linestrings
  linestrings <- lapply(X = 1:n, FUN = function(x) {
    pair <- st_combine(c(points[x], points[x + 1]))
    line <- st_cast(pair, "LINESTRING")
    return(line)
  })

  # One MULTILINESTRING object with all the LINESTRINGS
  multilinetring <- st_multilinestring(do.call("rbind", linestrings)) %>%
    st_sfc() %>%
    st_sf() %>%
    st_set_crs(4326) %>%
    st_transform(6676)
  return(multilinetring)
}
# Then we firstly make road lines for the 4 roads.
# Target file names of 4 roads (without 三太郎東 and 三太郎西 since they are special that we need to deal with them later).
tar_file_road <- readRDS("data_raw/road_raw/tar_file_road.rds")

road_map <- map2(
  tar_file_road$road,
  tar_file_road$track_file,
  function(road_x, file_x) {
    make_road_line(road_x, file_x, 1, "top") %>%
      mutate(road = road_x)
  }
)
road_map <- do.call(rbind, road_map)

# Then add 三太郎東 and 三太郎西 roads.
road_map <- rbind(
  road_map,
  make_road_line("三太郎東", "Current_19&21_TK.gpx", 0.15, "top") %>%
    mutate(road = "三太郎東"),
  make_road_line("三太郎西", "Current_19&21_TK.gpx", 0.09, "top")  %>%
    mutate(road = "三太郎西")
) %>%
  left_join(
    c(
      "スタルマタ", "City road Sutarumata line",
      "マテリア線", "Village road Materia line",
      "三太郎東", "City road Santaro-Higashi line",
      "三太郎西", "City road Santaro-Nishi line",
      "大名線", "Forest road Daimyo line",
      "大棚名音線-大金久線", "Village road OhdanaNaon-Ohganeku lines"
    ) %>%
      matrix(nrow = 6, byrow = TRUE) %>%
      data.frame() %>%
      rename_with(~ c("road", "road_en"))
  )
mapview(road_map)

# The outline Amami boundary.
amami_box <-
  st_read(dsn = "data_raw/road_raw/KagoshimaAdmin/N03-180101_46_GML",
          layer = "N03-18_46_180101") %>%
  rename(citycode = N03_007) %>%
  filter(citycode %in% c(46222, 46527, 46523, 46524, 46525)) %>%
  st_union() %>%
  st_sf() %>%
  st_crop(xmin = 129.28, xmax = 129.52, ymin = 28.18, ymax = 28.43)

# Plot road map.
png("data_edited/amami_rabbit_road.png", width = 1000, height = 1000, res = 300)
tm_shape(amami_box) +
  tm_polygons() +
  tm_shape(road_map) +
  tm_lines(col = "road_en", lwd = 2, palette = "Set2", title.col = "Road") +
  tm_layout(
    legend.position = c("left", "bottom"),
    legend.bg.color = "white", legend.frame = "grey", legend.width = 0.8,
    legend.text.size = 0.5
  ) +
  tm_scale_bar(position = c("left", "top"))
dev.off()
