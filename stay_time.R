# Preparation.
pacman::p_load(
  jmastats, lubridate, openxlsx, stringr, dplyr, tidyr, ggplot2, geojsonsf, sf,
  tmap, parallel, showtext, patchwork, jpmesh, mapview, data.table, targets,
  units
)
showtext_auto()
tar_make()
tar_load(get_inter_id)
tar_load(amami)
tar_load(pref_city_code)

# Analysis begins.
# Firstly, keep the dailyid from Agoop data with logs in target areas.
# Function to identify if a multi-polygon is interacted with another polygon. The result is a logical vector, the length of which is the same with the multi-polygon.
get_inter <- function(polygon_x, polygon_y) {
  res <- st_intersects(polygon_x, polygon_y)
  res[unlist(lapply(res, function(x) length(x) == 0))] <- 0
  res <- unlist(res)
  return(res)
}
# Bug: Take a sample for test.
set.seed(123)
# Get the Amami national park areas inside Amami Oshima island.
np_amami_pre <- st_read("data_raw/nps_amami.shp") %>%
  st_transform(6668) %>%
  st_make_valid %>%
  mutate(inter = get_inter(., amami)) %>%
  filter(inter == 1) %>%
  select(-inter) %>%
  mutate(id = row_number(), .before = 1)
mapview(np_amami_pre, zcol = "地域区")
dim(np_amami_pre)

# The stay time of tourists of the areas.
# Bug: There are too many polygons (n = 576) of the national park areas. Would that be too much?
# Find the boundary rectangular of each polygon in the national park area multi-polygon.
get_boundary_rec <- function(id_x) {
  np_amami_pre %>%
    filter(id == id_x) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    tibble() %>%
    select(X, Y) %>%
    rename_with(~ c("long", "lat")) %>%
    pivot_longer(cols = c("long", "lat"), names_to = "long_lat") %>%
    group_by(long_lat) %>%
    arrange(value) %>%
    summarise(min = first(value), max = last(value)) %>%
    ungroup() %>%
    pivot_longer(cols = c("min", "max"), names_to = "min_max") %>%
    mutate(boundary = paste(long_lat, min_max, sep = "_"), .before = 1) %>%
    select(boundary, value) %>%
    pivot_wider(names_from = boundary, values_from = value) %>%
    return()
}

np_amami_pre_rec <-
  lapply(st_drop_geometry(np_amami_pre)[["id"]], get_boundary_rec) %>%
  do.call(rbind, .) %>%
  mutate(id = np_amami_pre$id, .before = 1)

# Get Agoop data that has at least one point in target area.
get_agoop_target <- function(id_x) {
  # Remove logs out of the rectangular of the target national park area.
  gis_agoop_screen <-
    cbind(
      gis_agoop,
      st_coordinates(gis_agoop) %>%
        as.data.frame() %>%
        select(X, Y) %>%
        rename_with(~ c("long", "lat"))
    ) %>%
    filter(
      long >= np_amami_pre_rec[which(np_amami_pre_rec$id == id_x), ]$long_min,
      long <= np_amami_pre_rec[which(np_amami_pre_rec$id == id_x), ]$long_max,
      lat >= np_amami_pre_rec[which(np_amami_pre_rec$id == id_x), ]$lat_min,
      lat <= np_amami_pre_rec[which(np_amami_pre_rec$id == id_x), ]$lat_max
    )
  if(nrow(gis_agoop_screen) == 0) {
    return(NULL)
  } else {
    # Get target national park area.
    np_amami_pre_tar_area <- np_amami_pre %>%
      filter(id == id_x)
    # The interaction of remaining logs and target area.
    gis_agoop_inter <-
      gis_agoop_screen %>%
      mutate(inter = get_inter_id(gis_agoop_screen, np_amami_pre_tar_area))
    # The target dailyid.
    gis_agoop_inter_dailyid <-
      gis_agoop_inter %>%
      st_drop_geometry() %>%
      group_by(dailyid) %>%
      summarise(inter = sum(inter) > 0) %>%
      ungroup() %>%
      filter(inter) %>%
      pull(dailyid)
    gis_agoop_tar_area <-
      gis_agoop_inter %>%
      filter(dailyid %in% gis_agoop_inter_dailyid) %>%
      mutate(time = hour * 60 + minute) %>%
      arrange(month, day, dailyid, time)
    return(gis_agoop_tar_area)
  }
}

# Bug: Takes about 1.5 min.
gis_agoop_pre <- lapply(st_drop_geometry(np_amami_pre)[["id"]], get_agoop_target)
# Need to further remove the area with no logs.

np_amami <- np_amami_pre %>%
  mutate(have_log = !do.call("c", lapply(gis_agoop_pre, is.null))) %>%
  filter(have_log) %>%
  mutate(area = st_area(.), .after = "id")

names(gis_agoop_pre) <- np_amami_pre$id
gis_agoop_tar <- gis_agoop_pre[!do.call("c", lapply(gis_agoop_pre, is.null))]
gis_agoop_tar <-
  lapply(
    as.character(st_drop_geometry(np_amami)[["id"]]),
    function(x) {
      mutate(gis_agoop_tar[[x]], id = x)
    }
  ) %>%
  do.call(rbind, .) %>%
  # Bug: Should rename the column at the very begining.
  rename(area_id = id)

# So how long do people stay in the areas? And what is the difference between different areas?
get_stay_time <- function(area_id_x) {
  # Group by in-out and time order, keep the first and the last log of each period group and calculate the time difference.
  gis_agoop_tar %>%
    st_drop_geometry() %>%
    mutate(date = as_date(paste(year, month, day, sep = "-"))) %>%
    filter(area_id == area_id_x) %>%
    arrange(month, day, date, dailyid) %>%
    select(month, day, dailyid, date, hour, minute, inter) %>%
    mutate(time = hour * 60 + minute) %>%
    group_by(month, day, date, dailyid) %>%
    mutate(inter_grp = rleid(inter)) %>%
    ungroup() %>%
    group_by(month, day, date, dailyid, inter_grp) %>%
    mutate(
      row_num = row_number(),
      row_num_max = last(row_num)
    ) %>%
    ungroup() %>%
    filter(row_num == 1 | row_num == row_num_max) %>%
    group_by(month, day, date, dailyid, inter_grp) %>%
    mutate(time_pre = lag(time)) %>%
    ungroup() %>%
    mutate(time_diff = time - time_pre) %>%
    # Only keep the data in the target area and keep the data with stay time. .
    # Bug: If a person leave the target area for a little while, the "little while" shoule be counted into the stay time (considering that it might be due to some temporal leave, or error of the signal)?
    filter(inter == 1, !is.na(time_diff)) %>%
    # Add city name info.
    left_join(
      gis_agoop_tar %>%
        st_drop_geometry() %>%
        select(dailyid, home_citycode) %>%
        distinct() %>%
        left_join(pref_city_code, by = c("home_citycode" = "citycode")),
      by = "dailyid"
    ) %>%
    # Remove stay time <= 15 min.
    # Bug: That should be adjusted with size or road length of the target area.
    filter(time_diff > 15) %>%
    mutate(area_id = area_id_x, .before = 1) %>%
    return()
}

stay_time <- lapply(np_amami$id, get_stay_time) %>%
  do.call(rbind, .) %>%
  select(-time_pre) %>%
  left_join(st_drop_geometry(np_amami), by = c("area_id" = "id")) %>%
  rename(area_cls = 地域区)
ggplot(stay_time) +
  geom_histogram(aes(time_diff, fill = area_cls)) +
  facet_wrap(.~ area_id, scales = "free")
ggplot(stay_time) +
  geom_point(aes(month, time_diff), alpha = 0.5) +
  facet_wrap(.~ area_id, scales = "free")
mapview(np_amami, zcol = "地域区")

# What if we add area to the stay time?
stay_time_area <- stay_time %>%
  left_join(select(np_amami, id, area), by = c("area_id" = "id")) %>%
  mutate(stay_time_pa = time_diff / area)
library(units)
stay_time_area %>%
  arrange(area_cls) %>%
  mutate(area_id = factor(area_id, levels = unique(area_id))) %>%
  ggplot() +
  geom_histogram(aes(stay_time_pa, fill = area_cls)) +
  facet_wrap(.~ area_id, scales = "free")
ggplot(stay_time_area) +
  geom_boxplot(aes(area_cls, log(stay_time_pa)))

stay_time_area %>%
  group_by(area_id, month, day, area_cls) %>%
  summarise(n = length(unique(dailyid))) %>%
  ungroup() %>%
  ggplot() +
  geom_boxplot(aes(area_cls, log(n)))

# Why the 特別保護地域 (area_id == 23) has so many visitors?
mapview(np_amami %>% filter(id == 23)) +
  mapview(gis_agoop_tar %>% filter(area_id == 23), zcol = "month")

