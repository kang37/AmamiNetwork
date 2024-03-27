pacman::p_load(dplyr, sf, SpatialKDE, targets, tmap, mapview)

tar_make()
tar_load(gis_agoop)

# Bug: Sample: 3 days in July and August respectively.
gis_agoop_smp <-
  filter(gis_agoop, month == 7, day == 10 | day == 20 | day == 30)
# Bug: What CRS?
gis_agoop_smp <- st_transform(gis_agoop_smp, 6684)

# Grid of Amami.
# Amami boundary
amami <- st_read(dsn = "data_raw/KagoshimaAdmin/N03-180101_46_GML",
                 layer = "N03-18_46_180101") %>%
  rename(citycode = N03_007) %>%
  # cities (villiges) in Amamioshima island
  filter(citycode %in% c(46222, 46527, 46523, 46524, 46525)) %>%
  st_union() %>%
  st_sf() %>%
  st_transform(6684)
amami_grid <- create_grid_rectangular(amami, cell_size = 500) %>%
  # Remove no-intersected grids.
  st_join(mutate(amami, keep = 1)) %>%
  filter(!is.na(keep)) %>%
  select(-keep) %>%
  mutate(grid_id = row_number())

tm_shape(amami) +
  tm_polygons() +
  tm_shape(amami_grid) +
  tm_polygons()

# The number of unique ID of each grid.
hot_grid <-
  gis_agoop_smp %>%
  st_join(amami_grid) %>%
  st_drop_geometry() %>%
  # Remove points outside the grid.
  filter(!is.na(grid_id)) %>%
  # How many unique dailyid in each grid?
  group_by(grid_id) %>%
  summarise(dailyid_num = length(unique(dailyid))) %>%
  arrange(-dailyid_num) %>%
  # Add geometry of the grid.
  left_join(amami_grid, by = "grid_id") %>%
  st_as_sf()


mapview(hot_grid, zcol = "dailyid_num")

hot_grid %>%
  mutate(dailyid_num = log(dailyid_num)) %>%
  mapview(., zcol = "dailyid_num")



