library(tmaptools)
library(purrr)
library(units)

# since the *.gpx data contain some rebundant records, need to eliminate them by the time range of the experiment
expt <- readxl::read_excel("data_raw/amamirabbit.vehicle190621.xlsx") %>%
  tibble() %>%
  # one road was excluded from the analysis
  filter(roadname != "津名久林道")

# only focus on the common *.gpx showing both in the experiment data and the folder of *.gpx files
tar_gpx <- intersect(unique(expt$trackfile), list.files("data_raw/track"))

# further process the data
expt <- expt %>%
  # bug: why some track files only in the Excel, and some only in the folder?
  filter(trackfile %in% tar_gpx) %>%
  # enddate column: the raw data only records enddate when the experiment time cross 24:00:00 of the day
  mutate(enddate = case_when(is.na(enddate) ~ startdate, TRUE ~ enddate)) %>%
  # make a new time column for hour: minute: second
  mutate(
    start_date_time =
      paste(hour(starttime), minute(starttime), second(starttime), sep = ":"),
    end_date_time =
      paste(hour(endtime), minute(endtime), second(endtime), sep = ":")
  ) %>%
  mutate(
    start_date_time = paste(startdate, start_date_time),
    end_date_time = paste(enddate, end_date_time)
  ) %>%
  mutate(
    start_date_time = as_datetime(start_date_time, tz = "Asia/Tokyo"),
    end_date_time = as_datetime(end_date_time, tz = "Asia/Tokyo")
  ) %>%
  # get start time and end time of each track file
  group_by(trackfile, roadname) %>%
  summarise(
    start_time = min(start_date_time),
    end_time = max(end_date_time)
  ) %>%
  ungroup()

# *.gpx files containing the road and track points information
gpx_file <- paste0("data_raw/track/", tar_gpx)

# bug: take the first three *.gpx data as an example
gpx_dt <- lapply(
  gpx_file, function(x) read_GPX(x, layers = "track_points")$track_points
)
names(gpx_dt) <- tar_gpx

# for each track point data, only keep the points between the experiment time range, and further exclude the points that are not in Amami-oshima island
filter_gpx_dt <- function(tar_road, tar_track_file) {
  tar_time_record <- expt %>%
    filter(trackfile == tar_track_file, roadname == tar_road)
  # get longitude and latitude of the points
  # coord_info <- data.frame(st_coordinates(gpx_dt[[tar_track_file]])) %>%
  #   tibble() %>%
  #   rename_with(~ c("long", "lat"))
  res <- gpx_dt[[tar_track_file]] %>%
    # add longitude and latitude of the points
    # cbind(coord_info) %>%
    # the north point of the island is around (28.536134753004653, 129.686629208269), while the south point is around c(27.987566279271462, 129.20295933208985)
    # filter(lat < 28.536134753004653, lat > 27.987566279271462) %>%
    filter(time > tar_time_record$start_time, time < tar_time_record$end_time) %>%
    mutate(track_file = tar_track_file, road = tar_road) %>%
    select(road, track_file)
  return(res)
}

gpx_dt_sub <- map2(expt$roadname, expt$trackfile, filter_gpx_dt) %>%
  do.call(rbind, .)

# for all the road
mapview(gpx_dt_sub, zcol = "road", lwd = 0.1, alpha = 0.5, cex = 0.2)

# for each road
gpx_dt_sub %>%
  filter(road == "マテリア線") %>%
  mapview(zcol = "track_file", alpha.region = 0.1, lwd = 0.1, alpha = 0.2, cex = 3)
gpx_dt_sub %>%
  filter(road == "大名線") %>%
  mapview(zcol = "track_file", alpha.region = 0.1, lwd = 0.1, alpha = 0.2, cex = 3)
gpx_dt_sub %>%
  filter(road == "大棚名音線-大金久線") %>%
  mapview(zcol = "track_file", alpha.region = 0.1, lwd = 0.1, alpha = 0.2, cex = 3)
gpx_dt_sub %>%
  filter(road == "スタルマタ") %>%
  mapview(zcol = "track_file", alpha.region = 0.1, lwd = 0.1, alpha = 0.2, cex = 3)
gpx_dt_sub %>%
  filter(road == "三太郎東") %>%
  mapview(zcol = "track_file", alpha.region = 0.1, lwd = 0.1, alpha = 0.2, cex = 3)
gpx_dt_sub %>%
  filter(road == "三太郎西") %>%
  mapview(zcol = "track_file", alpha.region = 0.1, lwd = 0.1, alpha = 0.2, cex = 3)

# find the track file with most points for each road
tar_file_road <- gpx_dt_sub %>%
  st_drop_geometry() %>%
  group_by(road, track_file) %>%
  summarise(num_point = n()) %>%
  mutate(max_num = max(num_point)) %>%
  ungroup() %>%
  filter(num_point == max_num)

# then make road lines based on the track points
# function: make road line based on track points
# prop: the proportion to keep the track points
# from: keep the track points from top ("top") or from end ("end")
make_road_line <- function(road_name, track_file_name, prop, from) {
  tar_gpx_dt_sub = gpx_dt_sub %>%
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
# need to find the best proportion for keeping track points manually
# since 三太郎 road has been analyzed, here we exclude them
gpx_dt_sub %>%
  filter(road == "マテリア線") %>%
  mapview(alpha.region = 0.01, lwd = 0.1, alpha = 0.5, cex = 5) +
  mapview(
    make_road_line(
      "マテリア線", "track_18-01-23 221556_GPSmap62SJ.gpx", 1, "top"
    ),
    color = "red"
  )
gpx_dt_sub %>%
  filter(road == "大名線") %>%
  mapview(alpha.region = 0.01, lwd = 0.1, alpha = 0.2, cex = 5) +
  mapview(
    make_road_line(
      "大名線", "track_18-02-12 205150_GPSmap62SJ.gpx", 1, "top"
    ),
    color = "red"
  )
gpx_dt_sub %>%
  filter(road == "大棚名音線-大金久線") %>%
  mapview(alpha.region = 0.01, lwd = 0.1, alpha = 0.2, cex = 5) +
  mapview(
    make_road_line(
      "大棚名音線-大金久線","track_18-01-10 091522 PM_GPSmap62SJ.gpx", 1, "top"
    ),
    color = "red"
  )
gpx_dt_sub %>%
  filter(road == "スタルマタ") %>%
  mapview(alpha.region = 0.01, lwd = 0.1, alpha = 0.2, cex = 5) +
  mapview(
    make_road_line(
      "スタルマタ", "171120_track_HA.GPX", 0.5, "top"
    ),
    color = "red"
  )
# add the best proportion and direction of keeping track points to the target road-file data.frame
tar_file_road <- tar_file_road %>%
  # as mentioned above, exclue 三太郎
  filter(!grepl("三太郎", road)) %>%
  mutate(prop = c(0.5, 1, 1, 1), from = "top")

# make the buffer zones based on the road lines
sf_use_s2(FALSE)
other_road_buff <- pmap(
  list(tar_file_road$road,
       tar_file_road$track_file,
       tar_file_road$prop,
       tar_file_road$from),
  make_road_line
) %>%
  lapply(., function(x) st_buffer(x, dist = units::set_units(100, "m")))
sf_use_s2(TRUE)
other_road_buff <- other_road_buff %>%
  lapply(., function(x) st_transform(x, 6668))
# the buffer zones
do.call(rbind, other_road_buff) %>%
  mapview()

# apply original code to the other roads
gis_agoop_sub <- gis_agoop %>%
  filter(accuracy <= 100, hour >= 19 | hour <= 4) %>%
  select(dailyid, year, month, day)

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
res_nlog <- list()
for (i in 1:4) {
  road_buff_coord <- st_coordinates(other_road_buff[[i]]) %>%
    data.frame() %>%
    tibble() %>%
    select(X, Y) %>%
    rename_with(~ c("long", "lat"))
  res_nlog[[i]] <- gis_agoop_sub %>%
    filter(long < max(road_buff_coord$long), long > min(road_buff_coord$long)) %>%
    filter(lat < max(road_buff_coord$lat), lat > min(road_buff_coord$lat)) %>%
    st_join(other_road_buff[[i]], left = FALSE) %>%
    group_by(year, month, day, dailyid) %>%
    summarise(n_log = n()) %>%
    ungroup()
  if (nrow(res_nlog[[i]] != 0)) {
    res_nlog[[i]] <- res_nlog[[i]] %>%
      ggplot() +
      geom_col(aes(day, n_log, fill = dailyid)) +
      facet_wrap(.~ month) +
      theme(legend.position = "none")
  } else {
    res_nlog[[i]] <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
      geom_point(alpha = 0)
  }
}
res_nlog

res_ndailyid <- list()
for (i in 1:4) {
  road_buff_coord <- st_coordinates(other_road_buff[[i]]) %>%
    data.frame() %>%
    tibble() %>%
    select(X, Y) %>%
    rename_with(~ c("long", "lat"))
  res_ndailyid[[i]] <- gis_agoop_sub %>%
    filter(long < max(road_buff_coord$long), long > min(road_buff_coord$long)) %>%
    filter(lat < max(road_buff_coord$lat), lat > min(road_buff_coord$lat)) %>%
    st_join(other_road_buff[[i]], left = FALSE) %>%
    group_by(year, month, day) %>%
    summarise(n_dailyid = length(unique(dailyid))) %>%
    ungroup()
  if (nrow(res_ndailyid[[i]] != 0)) {
    res_ndailyid[[i]] <- ggplot(res_ndailyid[[i]]) +
      geom_col(aes(day, n_dailyid)) +
      facet_wrap(.~ month)
  } else {
    res_ndailyid[[i]] <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
      geom_point(alpha = 0)
  }
}
res_ndailyid

# what if we expand the buffer zone to 200 m?
# make the buffer zones based on the road lines
sf_use_s2(FALSE)
other_road_buff_200 <- pmap(
  list(tar_file_road$road,
       tar_file_road$track_file,
       tar_file_road$prop,
       tar_file_road$from),
  make_road_line
) %>%
  lapply(., function(x) st_buffer(x, dist = units::set_units(200, "m")))
sf_use_s2(TRUE)
other_road_buff_200 <- other_road_buff_200 %>%
  lapply(., function(x) st_transform(x, 6668))

# number of logs and dailyid of each road in each day under 200-m buffer zone
res_ndailyid_200 <- list()
for (i in 1:4) {
  road_buff_coord <- st_coordinates(other_road_buff_200[[i]]) %>%
    data.frame() %>%
    tibble() %>%
    select(X, Y) %>%
    rename_with(~ c("long", "lat"))
  res_ndailyid_200[[i]] <- gis_agoop_sub %>%
    filter(long < max(road_buff_coord$long), long > min(road_buff_coord$long)) %>%
    filter(lat < max(road_buff_coord$lat), lat > min(road_buff_coord$lat)) %>%
    st_join(other_road_buff_200[[i]], left = FALSE) %>%
    group_by(year, month, day) %>%
    summarise(n_dailyid = length(unique(dailyid))) %>%
    ungroup()
  if (nrow(res_ndailyid_200[[i]] != 0)) {
    res_ndailyid_200[[i]] <- ggplot(res_ndailyid_200[[i]]) +
      geom_col(aes(day, n_dailyid)) +
      facet_wrap(.~ month)
  } else {
    res_ndailyid_200[[i]] <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y)) +
      geom_point(alpha = 0)
  }
}
res_ndailyid_200
