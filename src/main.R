# Statement ----
# The code is used for Amami rabbit project.

# Package ----
library(jmastats)
library(lubridate)
library(openxlsx)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(geojsonsf)
library(sf)
library(tmap)
library(parallel)
library(showtext)
library(patchwork)
library(jpmesh)
library(mapview)

# Setting ----
showtext_auto()

# Function ----

# Read data ----
## Constant ----
# default CRS for the project: JGD2011
my_crs <- 6668
# note: EPSG for JGD2000 is 4612

## Pref and cities ----
# read prefcode and city code
pref_city_code <-
  read.csv("data_raw/prefcode_citycode_master_UTF-8.csv") %>%
  tibble() %>%
  select(prefcode, prefname, citycode, cityname) %>%
  distinct()

## Agoop ----
# Agoop folder
agoop_folder <- "data_raw/23_Agoop_amami_data"
# get all subfolder
agoop_subfolder <-
  list.files(agoop_folder) %>%
  grep("PDP", x = ., value = TRUE) %>%
  .[!grepl("zip", x = .)]
# get all Agoop *.csv file directory
agoop_file <- vector("list", length = length(agoop_subfolder))
for (i in 1:length(agoop_subfolder)) {
  agoop_file[[i]] <- list.files(paste0(agoop_folder, "/", agoop_subfolder[i]))
  agoop_file[[i]] <- paste0(
    agoop_folder, "/", agoop_subfolder[i], "/", agoop_file[[i]]
  )
}
agoop_file <- do.call(c, agoop_file)

# read all the data
# create a empty list to store the raw data
raw_agoop <- mclapply(agoop_file, read.csv, mc.cores = 4)
# combine the raw data list into a data.frame
raw_agoop <- bind_rows(raw_agoop) %>%
  left_join(pref_city_code)

# turn into GIS data
gis_agoop <-
  # turn raw data into simple feature for GIS analysis
  st_as_sf(raw_agoop, coords = c("longitude", "latitude")) %>%
  # add projection
  st_set_crs(my_crs)

## Holiday ----
# holidays in Japan
holiday <- read.csv("data_raw/Japan_holiday_2018.csv") %>%
  mutate(
    year = 2018, month = substr(.$月日, 1, 2), day  = substr(.$月日, 4, 5)
  ) %>%
  rename(holiday_name = 名称) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  select(date, holiday_name)

## Weather ----
# block: 名瀬 in Amami island
weather <- vector("list", 12)
for (i in 1:12) {
  weather[[i]] <-
    jma_collect(item = "daily", block_no = 47909, year = 2018, month = i)
}
weather <- do.call(rbind, weather)

## GIS layer ----
# Amami boundary
amami <- st_read(dsn = "data_raw/KagoshimaAdmin/N03-180101_46_GML",
                 layer = "N03-18_46_180101") %>%
  rename(citycode = N03_007) %>%
  filter(citycode == 46222) %>%
  st_union() %>%
  st_sf()

# national parks within Amami
# nps_amami <-
#   st_read(dsn = "data_raw/NationalPark/nps", layer = "nps_all") %>%
#   subset(名称 == "奄美群島") %>%
#   st_transform(my_crs) %>%
#   st_make_valid() %>%
#   st_union() %>%
#   st_sf()

# road for night trip
road <- st_read(dsn = "data_raw", layer = "Night_tour_road") %>%
  st_transform(my_crs)

# meshes cover the tour road
# bug: this layer also covers residential area and high way
tour_mesh <- meshcode_sf(
  data.frame(
    meshcode = meshcode(c(42293312, 42293313, 42293314, 42293324,
                          42293334, 42293344, 42293345))
  ),
  meshcode
) %>%
  st_transform(my_crs)

# make a buffer zone around the road
# What buffer distance should we use? According to quantile of accuracy?
road_buff <- st_buffer(road, 100)

# keep the target Agoop GIS data
# test: how many data left for analysis?
# test_agoop <- gis_agoop %>%
#   filter(accuracy <= 100)
# dim(test_agoop)
# test_agoop <- test_agoop %>%
#   filter(hour >= 19 | hour <= 4)
# dim(test_agoop)
# tm_shape(road_buff) +
#   tm_polygons() +
#   tm_shape(test_agoop) +
#   tm_dots()
# keep the target data
agoop_buff <- gis_agoop %>%
  filter(accuracy <= 100, hour >= 19 | hour <= 4) %>%
  st_join(road_buff) %>%
  filter(!is.na(id))
mapView(road_buff) +
  mapview(agoop_buff, cex = 0.5)

# add variables to the target data
agoop_buff <- agoop_buff %>%
  mutate(date = as_date(paste(year, month, day, sep = "-"))) %>%
  left_join(holiday)
agoop_buff <- agoop_buff %>%
  left_join(weather)

# Analysis ----
## General description ----
cat(
  "Number of logs: ", nrow(agoop_buff), "\n",
  "Number of dailyid: ", length(unique(agoop_buff$dailyid)), "\n",
  "Number of log per dailyid: ",
  nrow(raw_agoop) / length(unique(raw_agoop$dailyid))
)

# Peak days are neither holiday nor weekend?
agoop_buff %>%
  st_drop_geometry() %>%
  group_by(year, month, day, dayofweek, holiday_name) %>%
  summarise(n_dailyid = length(unique(dailyid))) %>%
  ungroup() %>%
  mutate(holi_wkn = case_when(
    !is.na(holiday_name) ~ "holiday",
    dayofweek > 5 ~ "weekend",
    TRUE ~ "other"
  )) %>%
  ggplot() +
  geom_col(aes(day, n_dailyid, fill = holi_wkn)) +
  facet_wrap(.~ month)

# time of logs of each dailyid
# take August 30 as an example
agoop_buff %>%
  st_drop_geometry() %>%
  filter(month == 8, day == 30) %>%
  group_by(dailyid, hour, minute) %>%
  summarise(n_log = n()) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(dailyid, minute, fill = n_log)) +
  theme(axis.text.x = element_blank()) +
  facet_wrap(.~ hour)
# there are 3 dailyid that day, and 2 of them have almost the same time of logs - maybe one person with 2 Agoop apps?

# some note:
# if there are too many people, what we should care is the time lag of two dailyid; or, time period when the target area is empty (say, it need 20 min to recover)
