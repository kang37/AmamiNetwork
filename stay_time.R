pacman::p_load(
  jmastats, lubridate, openxlsx, stringr, dplyr, tidyr, ggplot2, geojsonsf, sf,
  tmap, parallel, showtext, patchwork, jpmesh, mapview, data.table, targets
)
tar_make()

# Stay time of the sample data.
tar_load(gis_agoop_kinsakubaru)
tar_load(pref_city_code)
# Group by in-out and time order, keep the first and the last log of each period group and calculate the time difference.
gis_agoop_kinsakubaru_time <- gis_agoop_kinsakubaru %>%
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
    gis_agoop_kinsakubaru %>%
      st_drop_geometry() %>%
      select(dailyid, home_citycode) %>%
      distinct() %>%
      left_join(pref_city_code, by = c("home_citycode" = "citycode")),
    by = "dailyid"
  ) %>%
  # Remove stay time <= 15 min.
  filter(time_diff > 15)
# How many people in sample? Need the data of original sample.
# How many people have stay time in target area?
length(unique(gis_agoop_kinsakubaru_time$dailyid))
# Distribution of the stay time.
ggplot(gis_agoop_kinsakubaru_time) +
  geom_histogram(aes(time_diff))
# Distribution of home location.
table(gis_agoop_kinsakubaru_time$cityname)
# Conclusion: most from other cities (tourists) rather than Amami city.

# Monthly change of visitor number in Amami.
tar_load(plt_agoop_raw)
plt_agoop_raw

# Monthly change of visitor number.
gis_agoop_kinsakubaru_time %>%
  st_drop_geometry() %>%
  select(dailyid, month) %>%
  distinct() %>%
  ggplot() + geom_bar(aes(month))
# Conclusion: the season variety of the visitors to target area and that of the island (not exclude the local peopl) are different.

# Monthly change of stay time.
gis_agoop_kinsakubaru_time %>%
  st_drop_geometry() %>%
  select(dailyid, month, time_diff) %>%
  group_by(month, dailyid) %>%
  summarise(time_diff = mean(time_diff)) %>%
  ungroup() %>%
  ggplot() + geom_col(aes(month, time_diff))

# Impact of holiday and weather.
tar_load(weather)
tar_load(holiday)

# On visitor number.
# Holiday ~ visitor number.
gis_agoop_kinsakubaru_num <-
  gis_agoop_kinsakubaru_time %>%
  group_by(date) %>%
  summarise(num = length(unique(dailyid))) %>%
  ungroup() %>%
  # Add holiday information.
  left_join(holiday, by = "date") %>%
  # Add weather column.
  left_join(weather, by = "date")
gis_agoop_kinsakubaru_num %>%
  mutate(holiday = case_when(
    is.na(holiday_name) ~ FALSE,
    TRUE ~ TRUE
  )) %>%
  ggplot() +
  geom_point(aes(date, num, col = holiday), alpha = 0.6)
gis_agoop_kinsakubaru_num %>%
  mutate(holiday = case_when(
    is.na(holiday_name) ~ FALSE,
    TRUE ~ TRUE
  )) %>%
  ggplot() +
  geom_point(aes(sum_mm, num, col = holiday), alpha = 0.6)

# On stay time.
gis_agoop_kinsakubaru_time <- gis_agoop_kinsakubaru_time %>%
  st_drop_geometry() %>%
  # Bug: take average stay time.
  group_by(date, dailyid, time_diff) %>%
  summarise(time_diff = mean(time_diff)) %>%
  ungroup() %>%
  # Add holiday information.
  left_join(holiday, by = "date") %>%
  # Add weather column.
  left_join(weather, by = "date")
# Holiday ~ stay time.
gis_agoop_kinsakubaru_time %>%
  mutate(holiday = case_when(
    is.na(holiday_name) ~ FALSE,
    TRUE ~ TRUE
  )) %>%
  ggplot() +
  geom_point(aes(date, time_diff, col = holiday), alpha = 0.5)
# Weather ~ stay time.
gis_agoop_kinsakubaru_time %>%
  mutate(holiday = case_when(
    is.na(holiday_name) ~ FALSE,
    TRUE ~ TRUE
  )) %>%
  ggplot(aes(sum_mm, time_diff)) +
  geom_point(aes(col = holiday), alpha = 0.5) +
  geom_smooth(method = "lm")
