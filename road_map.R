# Preparation ----
library(showtext)
showtext_auto()
source("main.R")
source("other_road.R")

# Analysis ----
# Make road lines for 4 roads.
road_map <- map2(
  tar_file_road$road,
  tar_file_road$track_file,
  function(road_x, file_x) {
    make_road_line(road_x, file_x, 1, "top") %>%
      mutate(road = road_x)
  }
)
road_map <- do.call(rbind, road_map)
# Add 三太郎東 and 三太郎西 roads.
road_map <- rbind(
  road_map,
  make_road_line("三太郎東", "Current_19&21_TK.gpx", 0.15, "top") %>%
    mutate(road = "三太郎東"),
  make_road_line("三太郎西", "Current_19&21_TK.gpx", 0.09, "top")  %>%
    mutate(road = "三太郎西")
) %>%
  left_join(
    data.frame(
      road = c("スタルマタ", "マテリア線", "大名線", "大棚名音線-大金久線",
               "三太郎東", "三太郎西"),
      # Bug: The English names of the roads?
      road_en = c(1:6)
    )
  )
mapview(road_map)

# The outline Amami boundary.
amami_box <- st_crop(
  amami, xmin = 129.28, xmax = 129.52, ymin = 28.18, ymax = 28.43
)

# Plot road map.
tm_shape(amami_box) +
  tm_polygons() +
  tm_shape(road_map) +
  tm_lines(col = "road", lwd = 2, palette = "Set2", title.col = "Road") +
  tm_layout(
    legend.position = c("left", "bottom"),
    legend.bg.color = "white", legend.frame = "grey"
  ) +
  tm_scale_bar(bg.color = "white")
