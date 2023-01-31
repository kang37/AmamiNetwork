# Statement ----
# The code is used for Fujisan valuation project.

# Package ----
library(jmastats)
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

# Setting ----
showtext_auto()

# Function ----

# Read data ----
## Constant ----
# default CRS for the project: JGD2011
kCRS <- 6668
# note: EPSG for JGD2000 is 4612

## Pref and cities ----
# read prefcode and city code
pref_city_code <-
  read.csv("RawData/prefcode_citycode_master_UTF-8.csv") %>%
  tibble() %>%
  select(prefcode, prefname, citycode, cityname) %>%
  distinct()

## Agoop ----
# Agoop folder
agoop_folder <- "RawData/23_Agoop_amami_data"
# get all subfolder
agoop_subfolder <-
  list.files(agoop_folder) %>%
  grep("PDP", x = ., value = TRUE) %>%
  .[!grepl("zip", x = .)]
# get all Agoop *.csv file directory
agoop_file <- vector("list", length = length(agoop_subfolder))
for (i in 1:length(agoop_subfolder)) {
  agoop_file[[i]] <- list.files(paste0(kDirAgoop, "/", agoop_subfolder[i]))
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
  st_set_crs(kCRS)

## Holiday ----
# holidays in Japan
holiday <- read.csv("RawData/Japan_holiday_2018.csv") %>%
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
amami <- st_read(dsn = "RawData/KagoshimaAdmin/N03-180101_46_GML",
                 layer = "N03-18_46_180101") %>%
  rename(citycode = N03_007) %>%
  filter(citycode == 46222) %>%
  st_union() %>%
  st_sf()

# national parks within Amami
nps_amami <-
  st_read(dsn = "RawData/NationalPark/nps", layer = "nps_all") %>%
  subset(名称 == "奄美群島") %>%
  st_transform(kCRS) %>%
  st_make_valid() %>%
  st_union() %>%
  st_sf()

# Analysis -----
## General description -----
# reply to questions on GitHub
cat(
  "Number of count: ", nrow(raw_agoop), "\n",
  "Number of unique ID: ", length(unique(raw_agoop$dailyid)), "\n",
  "Number of log per dailyid: ",
  nrow(raw_agoop) / length(unique(raw_agoop$dailyid))
)

# temporal change
# daily change
raw_agoop %>%
  select(month, day, dailyid) %>%
  distinct() %>%
  group_by(month, day) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_col(aes(day, n)) +
  facet_wrap(.~ month)

# monthly change
raw_agoop %>%
  mutate(local = case_when(
    cityname == "奄美市" ~ TRUE,
    cityname != "奄美市" ~FALSE
  )) %>%
  select(month, local, dailyid) %>%
  distinct() %>%
  group_by(local, month) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_col(aes(month, n, fill = local))

# proportion of local people montly
local_prop_mth <- raw_agoop %>%
  mutate(local = case_when(
    cityname == "奄美市" ~ TRUE,
    cityname != "奄美市" ~FALSE
  )) %>%
  select(month, local, dailyid) %>%
  distinct() %>%
  group_by(month) %>%
  summarise(
    n = n(),
    n_local = sum(local == TRUE, na.rm = TRUE),
    prop_local = n_local / n
  )

# only keep tourists
raw_agoop %>%
  filter(cityname != "奄美市") %>%
  select(month, dailyid) %>%
  distinct() %>%
  group_by(month) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_col(aes(month, n), fill = "darkred")

# the distribution of the attributes
# table(raw_agoop$gender) %>% plot(main = "gender")
# table(raw_agoop$os) %>% plot(main = "os")

# where do people go?
gis_agoop_smp <- gis_agoop %>%
  group_by(month) %>%
  slice_sample(n = 10000)

tm_shape(amami) +
  tm_polygons(alpha = 0) +
  tm_shape(gis_agoop_smp) +
  tm_dots(alpha = 0.1)
