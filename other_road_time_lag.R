# Introduction ----
# To extract the logs of date with more than one dailyid during night tour time, and compare the time lag of the pairs of the dailyid.

# Package ----
library(tmaptools)
library(purrr)
library(units)

# Preparation ----
source("main.R")
source("other_road.R")

# Analysis ----
# get GIS data first
gis_agoop <-
  # turn raw data into simple feature for GIS analysis
  st_as_sf(raw_agoop, coords = c("longitude", "latitude")) %>%
  # add projection
  st_set_crs(my_crs)

# apply original code to the other roads
gis_agoop_sub <- gis_agoop %>%
  filter(accuracy <= 100, hour >= 19 | hour <= 4) %>%
  select(dailyid, year, month, day, hour, minute)

# add longitude and latitude data to the data.frame
gis_agoop_sub_coord <-
  st_coordinates(gis_agoop_sub) %>%
  data.frame() %>%
  tibble() %>%
  rename_with(~ c("long", "lat"))
gis_agoop_sub <- gis_agoop_sub %>%
  cbind(gis_agoop_sub_coord)

# number of logs and dailyid of each road in each day
# opt the algorithm: if we can filter the points from gis_agoop_sub firstly by latitude and longitude, then it would be faster
res_log_in_road <- list()
for (i in 1:4) {
  road_buff_coord <- st_coordinates(other_road_buff[[i]]) %>%
    data.frame() %>%
    tibble() %>%
    select(X, Y) %>%
    rename_with(~ c("long", "lat"))
  res_log_in_road[[i]] <- gis_agoop_sub %>%
    filter(long < max(road_buff_coord$long), long > min(road_buff_coord$long)) %>%
    filter(lat < max(road_buff_coord$lat), lat > min(road_buff_coord$lat)) %>%
    st_join(other_road_buff[[i]], left = FALSE) %>%
    # only keep the logs of the date with >= 2 dailyid
    group_by(month, day) %>%
    mutate(n_dailyid = length(unique(dailyid))) %>%
    ungroup() %>%
    filter(n_dailyid != 1) %>%
    arrange(month, day, hour, minute) %>%
    mutate(
      date = as_date(paste(year, "-", month, "-", day)),
      date_time = as_datetime(
        paste(year, "-", month, "-", day, " ", hour, ":", minute, ":", "00")
      )
    )
}
# for Santaro-line road
road_buff_coord <- st_coordinates(road_buff[[i]]) %>%
  data.frame() %>%
  tibble() %>%
  select(X, Y) %>%
  rename_with(~ c("long", "lat"))
res_log_in_road[[5]] <- gis_agoop_sub %>%
  filter(long < max(road_buff_coord$long), long > min(road_buff_coord$long)) %>%
  filter(lat < max(road_buff_coord$lat), lat > min(road_buff_coord$lat)) %>%
  st_join(road_buff, left = FALSE) %>%
  # only keep the logs of the date with >= 2 dailyid
  group_by(month, day) %>%
  mutate(n_dailyid = length(unique(dailyid))) %>%
  ungroup() %>%
  filter(n_dailyid != 1) %>%
  arrange(month, day, hour, minute) %>%
  mutate(
    date = as_date(paste(year, "-", month, "-", day)),
    date_time = as_datetime(
      paste(year, "-", month, "-", day, " ", hour, ":", minute, ":", "00")
    )
  )
res_log_in_road

# track the logs of the same date
# first road: 3 pairs
for (tar_date in unique(res_log_in_road[[1]]$date)) {
  res_log_in_road[[1]] %>%
    filter(date == tar_date) %>%
    mapview(zcol = "dailyid", label = "date_time",
            col.region = c("blue", "red"), alpha.region = 0.7, cex = 3) %>%
    print()
}
# 2nd road: 0 pair
# 3rd road: 0 log
# 4th road: 0 pair
# 5th road
for (tar_date in unique(res_log_in_road[[5]]$date)) {
  res_log_in_road[[5]] %>%
    filter(date == tar_date) %>%
    mapview(zcol = "dailyid", label = "date_time",
            col.region = c("blue", "red"), alpha.region = 0.7, cex = 3) %>%
    print()
}
