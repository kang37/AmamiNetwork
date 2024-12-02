# Preparation ----
pacman::p_load(
  lubridate, dplyr, dbscan, sf, tmap, mapview, stringi, showtext, tmap,
  ggplot2, tidyr, RColorBrewer, targets
)
showtext_auto()

# 重新运行tar_make()需要花大约40分钟。
tar_make()
tar_load(agoop_amami)
tar_load(amami)
tar_load(loc)

# 导出数据并且在QGIS中进行合并操作。
agoop_amami %>%
  select(res_id) %>%
  st_write(
    paste0("data_proc/agoop_amami_", Sys.Date(), ".shp"), append = FALSE
  )
loc %>%
  st_write(
    paste0("data_proc/loc_", Sys.Date(), ".shp"), append = FALSE
  )

# 读取增添地点信息的轨迹点数据。
agoop_amami <- agoop_amami %>%
  left_join(read.csv("data_proc/agoop_amami_loc.csv"), by = "res_id") %>%
  mutate(loc_id = case_when(is.na(loc_id) ~ "r", TRUE ~ as.character(loc_id)))

# Trajectory between loc_ids ----
# 量化轨迹：例如，对于某个dailyid而言，其轨迹可能是1-2-1-r-3，其中数字代表地点编号，“r”代表非定义地点。
traj_1 <- agoop_amami %>%
  st_drop_geometry() %>%
  arrange(dailyid, time) %>%
  # 排序数据中，dailyid变化表示是新的一位客户，loc_id变化表示进入新地点，仅保留这些记录，中间的都是处于同一个定义地点中的数据。
  group_by(dailyid) %>%
  mutate(
    new_dailyid = c(dailyid != lag(dailyid)),
    new_loc_id = c(loc_id != lag(loc_id)),
    new_dailyid = case_when(is.na(new_dailyid) ~ TRUE, TRUE ~ new_dailyid),
    new_loc_id = case_when(is.na(new_loc_id) ~ TRUE, TRUE ~ new_loc_id)
  ) %>%
  select(dailyid, source, time, loc_id, area, new_dailyid, new_loc_id) %>%
  filter(new_dailyid + new_loc_id >= 1) %>%
  select(-new_dailyid, -new_loc_id) %>%
  mutate(traj_id = row_number()) %>%
  ungroup()

# The duration of stay of each dailyid in each segment.
traj_2 <- agoop_amami %>%
  left_join(st_drop_geometry(loc), by = "loc_id") %>%
  # Add segment sequence.
  st_drop_geometry() %>%
  arrange(dailyid, time) %>%
  left_join(
    traj_1 %>% select(dailyid, loc_id, area, time, traj_id),
    by = c("dailyid", "loc_id", "time")
  ) %>%
  tidyr::fill(traj_id) %>%
  # 滞留时间为最大时间减最小时间，加上额外10分钟。加上额外10分钟原因：取样阶段每10分钟取一个点；换言之，否则如果只有一行数据，则滞留时间将为0。
  group_by(qua, dailyid, loc_id, area, traj_id) %>%
  summarise(
    duration_stay =
      as.numeric(difftime(max(time), min(time), units = "mins"))  + 10,
    .groups = "drop"
  ) %>%
  mutate(dur_stay_per_area = duration_stay / area)

# 合并两个结果。
traj <- left_join(traj_1, traj_2, by = c("dailyid", "loc_id", "traj_id"))

# General description ----
# 作图展示原始数据点的分布。
tm_shape(agoop_amami) +
  tm_dots(size = 0.01, col = "red", alpha = 0.3, palette="div") +
  tm_facets(by = "qua", along = "source")

# 不同客源的轨迹点数。
table(agoop_amami$source)

# 每个人每天有几个记录点？
agoop_amami %>%
  st_drop_geometry() %>%
  group_by(dailyid) %>%
  summarise(n_log = n(), .groups = "drop") %>%
  ggplot() +
  geom_histogram(aes(n_log), col = "white") +
  theme_bw()

# 每个人每天滞留地点数量？
agoop_amami %>%
  st_drop_geometry() %>%
  group_by(dailyid) %>%
  summarise(loc_id_num = length(unique(loc_id)), .groups = "drop") %>%
  ggplot() +
  geom_histogram(aes(loc_id_num), col = "white", binwidth = 1) +
  theme_bw()

# 每个月有多少人，本地和外地人分别多少？
agoop_amami %>%
  st_drop_geometry() %>%
  group_by(month, source) %>%
  summarise(dailyid_num = length(unique(dailyid)), .groups = "drop") %>%
  ggplot() +
  geom_col(aes(month, dailyid_num, fill = source)) +
  facet_wrap(.~ source, scales = "free")
# 每个季度有多少人？
agoop_amami %>%
  st_drop_geometry() %>%
  group_by(qua, source) %>%
  summarise(dailyid_num = length(unique(dailyid)), .groups = "drop") %>%
  ggplot() +
  geom_col(aes(qua, dailyid_num, fill = source)) +
  facet_wrap(.~ source, scales = "free")

# Maps of visitor number and duration ----
## Population ----
# 各个地点总共有多少人滞留？
# 漏洞：按照这样的方法统计，得到的是每个季度在各个地点停留的、按照客源区分的总人数。是否需要除以纳入计算的天数，以得到平均每天的滞留人数呢？从目的考虑，未必，因为目的是要看哪些地方滞留人数和滞留时间有差异。虽然不比考虑除以时间，但是可以考虑除以面积。
loc_pop <-
  traj %>%
  group_by(source, qua, loc_id) %>%
  summarise(dailyid_num = length(unique(dailyid)), .groups = "drop") %>%
  left_join(loc, by = "loc_id") %>%
  # 删除非定义地点的数据。
  filter(loc_id != "r") %>%
  # 计算单位面积滞留人口。
  mutate(dailyid_num_per_area = dailyid_num / area) %>%
  st_as_sf(sf_column_name = "geometry")

# 分季节分客源各个地点总滞留人数。
tm_shape(amami) +
  tm_polygons(col = "white") +
  tm_shape(loc_pop) +
  tm_dots(size = "dailyid_num", alpha = 0.5) +
  tm_facets(by = "qua", along = "source")

# 分季节分客源单位面积滞留人数。
# 漏洞：可能受极端值影响。
quantile(loc_pop$dailyid_num)
tm_shape(amami) +
  tm_polygons(col = "white") +
  # 漏洞：为了消除极端值的影响，删除只包50%分位数或以下人数的地点。
  tm_shape(loc_pop %>% filter(dailyid_num > 5)) +
  tm_dots(size = "dailyid_num_per_area", alpha = 0.5) +
  tm_facets(by = "qua", along = "source")

## Duration ----
# 各个地点的每人滞留时间中位数是多少？
# 漏洞：为什么有些行的dur_stay_per_area是空值？
loc_dur <-
  traj %>%
  group_by(source, qua, loc_id) %>%
  summarise(
    dailyid_num = length(unique(dailyid)),
    mid_dur_stay = median(duration_stay),
    mid_dur_stay_per_area = median(dur_stay_per_area),
    .groups = "drop"
  ) %>%
  left_join(loc, by = "loc_id") %>%
  # 删除非定义地点的数据。
  filter(loc_id != "r") %>%
  st_as_sf(sf_column_name = "geometry")

# 可视化。
# 滞留时间中位数。
tm_shape(amami) +
  tm_polygons(col = "white") +
  tm_shape(loc_dur) +
  tm_dots(size = "mid_dur_stay", alpha = 0.5) +
  tm_facets(by = "qua", along = "source")

# 单位面积滞留时间。
# 漏洞：可能受极端值影响。
quantile(loc_dur$dailyid_num)
tm_shape(amami) +
  tm_polygons(col = "white") +
  # 漏洞：为了消除极端值的影响，删除只包50%分位数或以下人数的地点。
  tm_shape(loc_dur %>% filter(dailyid_num > 5)) +
  tm_dots(size = "mid_dur_stay_per_area", alpha = 0.5) +
  tm_facets(by = "qua", along = "source")

## Pop and duration ----
# 总滞留时间和总滞留人数关系。
st_drop_geometry(loc_pop) %>%
  left_join(
    st_drop_geometry(loc_dur),
    by = c("source", "qua", "loc_id", "dailyid_num", "area")
  ) %>%
  ggplot() +
  geom_point(aes(log(dailyid_num), log(mid_dur_stay)))

# 单位面积滞留时间和单位面积滞留人数关系。
st_drop_geometry(loc_pop) %>%
  left_join(
    st_drop_geometry(loc_dur),
    by = c("source", "qua", "loc_id", "dailyid_num", "area")
  ) %>%
  ggplot() +
  geom_point(
    aes(log(dailyid_num_per_area), log(mid_dur_stay_per_area)), alpha = 0.3
  )
# 漏洞：单位面积停留时间最长、人数最多的236号是酒店的一部分。
# 漏洞：计算单位道路停留时间？

## Total duration ----
# 每个人各地点总停留时长？
loc_dur_tot <-
  traj %>%
  group_by(source, qua, loc_id) %>%
  summarise(
    dailyid_num = length(unique(dailyid)),
    dur_stay = sum(duration_stay),
    .groups = "drop"
  ) %>%
  left_join(loc, by = "loc_id") %>%
  # 删除非定义地点的数据。
  filter(loc_id != "r") %>%
  st_as_sf(sf_column_name = "geometry")
tm_shape(amami) +
  tm_polygons(col = "white") +
  tm_shape(loc_dur_tot) +
  tm_dots(size = "dur_stay", alpha = 0.5) +
  tm_facets(by = "qua", along = "source")

# In each mode, what is the structure?
# Bug: Take loc_id 76 as an example.
seg_time %>%
  left_join(
    seg_time %>%
      group_by(dailyid) %>%
      summarise(loc_id_n = n(), .groups = "drop") %>%
      filter(loc_id_n == 76),
    by = "dailyid"
  ) %>%
  filter(!is.na(loc_id_n)) %>%
  pull(seg_id) %>%
  table()

# Pairwise movement matrix.
# Has the data.frame been arranged in order?
# Bug: If a dailyid mostly stay in a loc_id, but s/he goes out of the loc_id usually? Then "c1-edge-c1-edge-c1" will become "c1".
# Bug: It is equal to that we remove the dailyid who stays in a loc_id for the whole day.
seg_pair_od <- seg %>%
  arrange(dailyid, seg_id) %>%
  mutate(
    new_dailyid = c(dailyid != lag(dailyid)),
    new_loc_id = c(loc_id != lag(loc_id)),
    new_dailyid = case_when(is.na(new_dailyid) ~ TRUE, TRUE ~ new_dailyid),
    new_loc_id = case_when(is.na(new_loc_id) ~ TRUE, TRUE ~ new_loc_id),
    # If the answer is yes to either question, then the state is changed.
    state_chg = c(new_dailyid | new_loc_id)
  ) %>%
  filter(state_chg) %>%
  group_by(dailyid) %>%
  mutate(
    destination = loc_id, origin = lag(loc_id)
  ) %>%
  filter(!is.na(origin)) %>%
  ungroup()

# Most movements are from c2 to c2, followed by c1-c2, c1-c1, c2-c8, c10-c10. c2 is an important center.
seg_pair_od %>%
  group_by(origin, destination) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(origin, destination)) +
  geom_tile(aes(fill = log(n)), col = "black") +
  theme_bw() +
  scale_fill_gradient2(high = "red", mid = "white", low = "blue") +
  # scale_x_continuous(breaks = seq(1, 18, 2)) +
  # scale_y_continuous(breaks = seq(1, 18, 2)) +
  theme(axis.ticks.x = element_blank()) +
  geom_text(aes(label = n), col = "black", size = 3)

# Abstract trajectory.
# First simplification: a "vc1-r-vc1" or "vc1-rc2-vc1" will be simplified as "vc1-vc1", then further simplified as "vc1".

# Function to abstract a trajectory string. For example, a "vc1-vc2-vc1" is simplified as "0-1-0".
map_traj <- function(x) {
  x_split <- strsplit(x, "-") %>% unlist()
  x_non_duplicate <- duplicated(x_split)
  x_map <- data.frame(ori = x_split[!x_non_duplicate]) %>%
    mutate(new = row_number() - 1)
  x_replace <- data.frame(ori = x_split) %>%
    left_join(x_map, by = "ori")
  res <- paste0(x_replace$new, collapse = "-")
  return(res)
}

# seg_time is logs of stay time larger than 15 min.
# Bug: Should remove the noise (loc_id = 0)?
merge_traj <- function(x) {
  # Split the character vector into individual digits.
  neighborhood_digits <- strsplit(x, "-")[[1]]
  # Remove consecutive duplicates.
  unique_digits <- c(
    neighborhood_digits[1],
    neighborhood_digits[-1][
      neighborhood_digits[-1] !=
        neighborhood_digits[-length(neighborhood_digits)]
    ]
  )
  # Combine the unique digits back into a single number
  merged_number <- paste(unique_digits, collapse = "-")
  return(merged_number)
}

traj_simp <- seg_time %>%
  arrange(dailyid, seg_id) %>%
  mutate(
    # If it is a new dailyid, or if it enters a new loc_id.
    new_dailyid = c(dailyid != lag(dailyid)),
    new_loc_id_id = c(loc_id_id != lag(loc_id_id)),
    new_dailyid =
      case_when(is.na(new_dailyid) ~ TRUE, TRUE ~ new_dailyid),
    new_loc_id_id =
      case_when(is.na(new_loc_id_id) ~ TRUE, TRUE ~ new_loc_id_id),
    # If the answer is yes to either question, then the state is changed.
    state_chg = c(new_dailyid | new_loc_id_id)
  ) %>%
  # Every time the state changes, assign a new visit ID. So for the non-changed rows, assgin NA then fill them with the changed visit ID.
  group_by(dailyid, state_chg) %>%
  mutate(visit_id = row_number()) %>%
  ungroup() %>%
  mutate(visit_id = case_when(
    state_chg ~ visit_id, !state_chg ~ NA
  )) %>%
  fill(visit_id) %>%
  # Make trajectory string.
  select(dailyid, visit_id, loc_id_id) %>%
  distinct() %>%
  mutate(loc_id_id = as.character(loc_id_id)) %>%
  group_by(dailyid) %>%
  summarise(traj_1 = paste0(loc_id_id, collapse = "-")) %>%
  ungroup() %>%
  # Further abstract trajectory. For example, a "c1-c2-c1" will be "0-1-0".
  mutate(traj_2 = lapply(traj_1, map_traj) %>% unlist()) %>%
  # Further merge traj_2.
  mutate(traj_3 = lapply(traj_2, merge_traj) %>% unlist())
traj_simp$seg_n <- strsplit(traj_simp$traj_3, "-") %>%
  lapply(., max) %>%
  unlist()
traj_simp$seg_n <- as.numeric(traj_simp$seg_n) + 1

table(traj_simp$traj_1) %>%
  data.frame() %>%
  tibble() %>%
  arrange(-Freq)

table(traj_simp$traj_2) %>%
  data.frame() %>%
  tibble() %>%
  arrange(-Freq)

top_traj_3 <-
  table(traj_simp$traj_3) %>%
  data.frame() %>%
  tibble() %>%
  arrange(-Freq) %>%
  head(15) %>%
  rename_with(~ c("traj", "freq")) %>%
  mutate(traj = factor(traj, levels = .$traj))
ggplot(top_traj_3) +
  geom_col(aes(traj, log(freq))) +
  labs(x = "Trajectory mode", y = "log(Frequency)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# Further explore some mode.
for (i in top_traj_3$traj) {
  print(i)
  traj_simp %>%
    filter(traj_3 == i) %>%
    group_by(traj_1) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(-n) %>%
    head(10) %>%
    print()
}
# Most visitors go circle, including 1 or 2 or 3 points circles.

# OD pair diff ----
vis_attr <-
  gis_agoop_coord_filt %>%
  # Bug: Should ungroup earlier.
  ungroup() %>%
  st_drop_geometry() %>%
  select(
    dailyid, month, dayofweek, home_prefcode, home_citycode, gender
  ) %>%
  distinct() %>%
  mutate(
    season = case_when(
      month >= 10 ~ "q4",
      month >= 7 ~ "q3",
      month >= 4  ~ "q2",
      month >= 1 ~ "q1"
    ),
    home_pref_grp = case_when(
      home_prefcode == "46" ~ "local", home_prefcode != "46" ~ "visitor"
    )
    # Bug: Why no Amamia residents in the Amami data??
    # home_pref_grp = case_when(
    #   home_citycode %in% c(46222, "46523", "46524", "46525", "46527") ~ "local",
    #   TRUE ~ "visitor"
    # )
  )

seg_pair_od_local_prop <-
  seg_pair_od %>%
  left_join(vis_attr) %>%
  filter(!is.na(home_pref_grp)) %>%
  group_by(home_pref_grp, origin, destination) %>%
  summarise(n = n(), .groups = "drop") %>%
  # 计算每对出行占所有出行配对的百分比。
  group_by(home_pref_grp) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()
# 可视化。
seg_pair_od_local_prop %>%
  ggplot(aes(origin, destination)) +
  geom_tile(aes(fill = prop), col = "black") +
  theme_bw() +
  scale_fill_gradient2(high = "red", mid = "white", low = "blue") +
  theme(axis.ticks.x = element_blank()) +
  geom_text(aes(label = round(prop, 2)*100), col = "black", size = 3) +
  facet_wrap(.~ home_pref_grp, scales = "free", ncol = 1) +
  labs(x = "Origin", y = "Destination", fill = "Proportion") +
  theme(legend.position = "bottom")

# 计算基尼指数和SD。
seg_pair_od_local_prop %>%
  group_by(home_pref_grp) %>%
  summarise(
    prop_gini = Gini(prop), prop_sd = sd(prop), .groups = "drop"
  )

# Pairs of OD by local/visitor and season.
seg_pair_od_local_quarter_prop <-
  seg_pair_od %>%
  # filter(!c(destination == "4" & origin == "5")) %>%
  # filter(!c(destination == "5" & origin == "4")) %>%
  left_join(vis_attr) %>%
  filter(!is.na(home_pref_grp)) %>%
  group_by(home_pref_grp, season, origin, destination) %>%
  summarise(n = n(), .groups = "drop") %>%
  # 计算每对出行占所有出行配对的百分比。
  group_by(home_pref_grp, season) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# 可视化。
seg_pair_od_local_quarter_prop %>%
  ggplot(aes(origin, destination)) +
  geom_tile(aes(fill = prop), col = "black") +
  theme_bw() +
  scale_fill_gradient2(high = "red", mid = "white", low = "blue") +
  theme(axis.ticks.x = element_blank()) +
  geom_text(aes(label = round(prop, 2)*100), col = "black", size = 3) +
  facet_grid(home_pref_grp ~ season)

# 计算基尼指数。
library(DescTools)
seg_pair_od_prop %>%
  group_by(home_pref_grp, season) %>%
  summarise(prop_gini = Gini(prop), .groups = "drop") %>%
  ggplot() +
  geom_line(aes(season, prop_gini, col = home_pref_grp, group = home_pref_grp))
# 计算标准差。
seg_pair_od_prop %>%
  group_by(home_pref_grp, season) %>%
  summarise(prop_sd = sd(prop), .groups = "drop") %>%
  ggplot() +
  geom_line(aes(season, prop_sd, col = home_pref_grp, group = home_pref_grp))

# OD network ----
# By source prefecture.
od_node <- st_coordinates(gis_agoop_coord_filt_sample) %>%
  data.frame() %>%
  rename_with(~ c("longitude", "latitude")) %>%
  mutate(loc_id = gis_agoop_coord_filt_sample$loc_id) %>%
  group_by(loc_id) %>%
  summarise(lon = median(longitude), lat = median(latitude))
od_edge <-
  seg_pair_od %>%
  ungroup() %>%
  left_join(vis_attr) %>%
  select(season, gender, home_pref_grp, origin, destination) %>%
  group_by(season, gender, home_pref_grp, origin, destination) %>%
  summarise(n = n(), .groups = "drop")

# Get seasonal OD network by a variable.
ggplot() +
  # geom_sf(data = amami) +
  geom_curve(
    data = od_edge %>%
      filter(
        !c(origin == 4 & destination == 5),
        !c(origin == 5 & destination == 4),
        !is.na(home_pref_grp),
        home_pref_grp != ""
      ) %>%
      group_by(home_pref_grp, season, origin, destination) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      group_by(home_pref_grp, season) %>%
      mutate(n = n / max(n)) %>%
      left_join(
        od_node %>% rename(origin = loc_id, ori_lon = lon, ori_lat = lat),
        by = "origin"
      ) %>%
      left_join(
        od_node %>% rename(destination = loc_id, dest_lon = lon, dest_lat = lat),
        by = "destination"
      ) ,
    aes(x = ori_lon, y = ori_lat, xend = dest_lon, yend = dest_lat, linewidth = n,
        alpha = n, col = n)
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_linewidth(range = c(0.1, 1)) +
  geom_point(
    data = od_node, aes(x = lon, y = lat), col = "grey", size = 3.5
  ) +
  geom_text(
    data = od_node, aes(x = lon, y = lat, label = loc_id), size = 3
  ) +
  labs(x = "Lon", y = "Lat") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_grid(home_pref_grp ~ season)

# Add gender.
# Get seasonal OD network by a variable.
ggplot() +
  # geom_sf(data = amami) +
  geom_curve(
    data = od_edge %>%
      filter(
        !c(origin == 4 & destination == 5),
        !c(origin == 5 & destination == 4),
        !is.na(home_pref_grp),
        home_pref_grp != "",
        !is.na(gender),
        gender != ""
      ) %>%
      mutate(home_gender = paste0(home_pref_grp, "-", gender)) %>%
      group_by(home_gender, season, origin, destination) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      group_by(home_gender, season) %>%
      mutate(n = n / sum(n)) %>%
      left_join(
        od_node %>% rename(origin = loc_id, ori_lon = lon, ori_lat = lat),
        by = "origin"
      ) %>%
      left_join(
        od_node %>% rename(destination = loc_id, dest_lon = lon, dest_lat = lat),
        by = "destination"
      ) ,
    aes(x = ori_lon, y = ori_lat, xend = dest_lon, yend = dest_lat, size = n,
        alpha = n, col = n)
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_size(range = c(0.2, 1)) +
  geom_point(
    data = od_node, aes(x = lon, y = lat), col = "grey", size = 3.5
  ) +
  geom_text(
    data = od_node, aes(x = lon, y = lat, label = loc_id), size = 3
  ) +
  labs(x = "Lon", y = "Lat") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_grid(home_gender ~ season)

# 各个格子的多样性系数。
od_edge %>%
  filter(
    !c(origin == 4 & destination == 5),
    !c(origin == 5 & destination == 4),
    !is.na(home_pref_grp),
    home_pref_grp != "",
    !is.na(gender),
    gender != ""
  ) %>%
  mutate(home_gender = paste0(home_pref_grp, "-", gender)) %>%
  group_by(home_pref_grp, gender, season, origin, destination) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  group_by(home_pref_grp, gender, season) %>%
  mutate(n = n / sum(n)) %>%
  # Calculate Gini and SD of each cell.
  group_by(home_pref_grp, gender, season) %>%
  summarise(diversity = vegan::diversity(n, "shannon"), .groups = "drop") %>%
  ggplot() +
  geom_col(aes(season, diversity, fill = gender), position = "dodge") +
  facet_wrap(home_pref_grp~., ncol = 1) +
  labs(x = NULL, y = "Diversity", fill = "Gender") +
  theme_bw()

# Trajectory by visitor attr ----
traj_simp_attr <- traj_simp %>%
  left_join(vis_attr, by = "dailyid")

## Motif ----
traj_simp_attr %>%
  group_by(traj_3, season) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1) %>%
  ggplot() +
  geom_col(aes(traj_3, n, fill = as.character(season))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

traj_simp_attr %>%
  group_by(traj_3, gender) %>%
  summarise(n = n()) %>%
  filter(n > 10) %>%
  ggplot() +
  geom_point(aes(gender, n)) +
  facet_wrap(.~ traj_3, scales = "free")

traj_simp_attr %>%
  group_by(traj_3, gender) %>%
  summarise(n = n()) %>%
  filter(n > 10) %>%
  ggplot() +
  geom_point(aes(gender, n)) +
  facet_wrap(.~ traj_3, scales = "free")

# 各种简化轨迹的数量分布。
traj_simp %>%
  group_by(seg_n, traj_3) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(seg_n, -n) %>%
  mutate(traj_3 = factor(traj_3, levels = traj_3)) %>%
  # Remove n_loc more than 4.
  filter(seg_n <= 4) %>%
  group_by(seg_n) %>%
  slice_head(n = 15) %>%
  ggplot() +
  geom_col(aes(traj_3, n, fill = as.character(seg_n))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Simplified trajectory", y = "Number of Daily ID",
    fill = "Location\nNumber"
  )

# 各种seg_n对应的主要motif。
motif_smry <- traj_simp_attr %>%
  group_by(seg_n, traj_3, gender, home_pref_grp) %>%
  summarise(dailyid_n = n(), .groups = "drop") %>%
  # 各种motif占seg_n组内的比例。
  group_by(seg_n, gender, home_pref_grp) %>%
  mutate(n_prop_in_grp = dailyid_n / sum(dailyid_n)) %>%
  ungroup() %>%
  arrange(seg_n, -n_prop_in_grp) %>%
  # 各种motif占总体的比例。
  mutate(n_prop_tot = dailyid_n / sum(dailyid_n)) %>%
  # 可以归入哪种motif。
  mutate(motif = case_when(
    seg_n == 1 ~ "1loc",
    seg_n == 2 &
      stri_detect_fixed(traj_3, "0-1") & !stri_detect_fixed(traj_3, "1-0") ~
      "2loc-chain",
    seg_n == 2 &
      stri_detect_fixed(traj_3, "0-1") & stri_detect_fixed(traj_3, "1-0") ~
      "2loc-full",
    seg_n == 3 &
      stri_detect_fixed(traj_3, "0-1") & !stri_detect_fixed(traj_3, "1-0") &
      stri_detect_fixed(traj_3, "1-2") & !stri_detect_fixed(traj_3, "2-1") &
      !stri_detect_fixed(traj_3, "0-2") & !stri_detect_fixed(traj_3, "2-0") ~
      "3loc-chain",
    seg_n == 3 &
      stri_detect_fixed(traj_3, "0-1") & stri_detect_fixed(traj_3, "1-0") &
      !stri_detect_fixed(traj_3, "1-2") & !stri_detect_fixed(traj_3, "2-1") &
      stri_detect_fixed(traj_3, "0-2") & !stri_detect_fixed(traj_3, "2-0") ~
      "3loc-center",
    seg_n == 3 &
      stri_detect_fixed(traj_3, "0-1") & !stri_detect_fixed(traj_3, "1-0") &
      stri_detect_fixed(traj_3, "1-2") & stri_detect_fixed(traj_3, "2-1") &
      !stri_detect_fixed(traj_3, "0-2") & !stri_detect_fixed(traj_3, "2-0") ~
      "3loc-chain-1late-back",
    seg_n == 3 &
      stri_detect_fixed(traj_3, "0-1") & !stri_detect_fixed(traj_3, "1-0") &
      stri_detect_fixed(traj_3, "1-2") & !stri_detect_fixed(traj_3, "2-1") &
      !stri_detect_fixed(traj_3, "0-2") & stri_detect_fixed(traj_3, "2-0") ~
      "3loc-loop",
    seg_n == 3 &
      stri_detect_fixed(traj_3, "0-1") & stri_detect_fixed(traj_3, "1-0") &
      stri_detect_fixed(traj_3, "1-2") & !stri_detect_fixed(traj_3, "2-1") &
      !stri_detect_fixed(traj_3, "0-2") & !stri_detect_fixed(traj_3, "2-0") ~
      "3loc-chain-1early-back",
    seg_n == 4 &
      stri_detect_fixed(traj_3, "0-1") & !stri_detect_fixed(traj_3, "1-0") &
      stri_detect_fixed(traj_3, "1-2") & !stri_detect_fixed(traj_3, "2-1") &
      stri_detect_fixed(traj_3, "2-3") & !stri_detect_fixed(traj_3, "3-2") &
      !stri_detect_fixed(traj_3, "0-3") & !stri_detect_fixed(traj_3, "3-0") &
      !stri_detect_fixed(traj_3, "0-2") & !stri_detect_fixed(traj_3, "2-0") &
      !stri_detect_fixed(traj_3, "1-3") & !stri_detect_fixed(traj_3, "3-1") ~
      "4loc-chain",
    seg_n == 4 &
      stri_detect_fixed(traj_3, "0-1") & stri_detect_fixed(traj_3, "1-0") &
      !stri_detect_fixed(traj_3, "1-2") & !stri_detect_fixed(traj_3, "2-1") &
      stri_detect_fixed(traj_3, "2-3") & !stri_detect_fixed(traj_3, "3-2") &
      !stri_detect_fixed(traj_3, "0-3") & !stri_detect_fixed(traj_3, "3-0") &
      stri_detect_fixed(traj_3, "0-2") & !stri_detect_fixed(traj_3, "2-0") &
      !stri_detect_fixed(traj_3, "1-3") & !stri_detect_fixed(traj_3, "3-1") ~
      "4loc-chain-1early-back",
    seg_n == 4 &
      stri_detect_fixed(traj_3, "0-1") & !stri_detect_fixed(traj_3, "1-0") &
      stri_detect_fixed(traj_3, "1-2") & !stri_detect_fixed(traj_3, "2-1") &
      stri_detect_fixed(traj_3, "2-3") & !stri_detect_fixed(traj_3, "3-2") &
      !stri_detect_fixed(traj_3, "0-3") & stri_detect_fixed(traj_3, "3-0") &
      !stri_detect_fixed(traj_3, "0-2") & !stri_detect_fixed(traj_3, "2-0") &
      !stri_detect_fixed(traj_3, "1-3") & !stri_detect_fixed(traj_3, "3-1") ~
      "4loc-loop"
  ))

# 占比：前5个取比例值，其他的归入“其他”中，计算各种motif在seg_n组内的比例，以及占所有motif的比例。
motif_smry %>%
  group_by(seg_n) %>%
  mutate(motif = case_when(
    is.na(motif) ~ paste0(seg_n, "loc_other"), TRUE ~ motif
  )) %>%
  group_by(seg_n, motif) %>%
  summarise(
    dailyid_n = sum(dailyid_n),
    n_prop_in_grp = sum(n_prop_in_grp),
    n_prop_tot = sum(n_prop_tot),
    .groups = "drop"
  )

# Factor levels of motif by proportion.
lvl_motif <- motif_smry %>%
  group_by(seg_n) %>%
  mutate(motif = case_when(
    is.na(motif) ~ paste0(seg_n, "loc_other"), TRUE ~ motif
  )) %>%
  group_by(seg_n, motif) %>%
  summarise(n_prop_tot = sum(n_prop_tot), .groups = "drop") %>%
  arrange(-n_prop_tot) %>%
  pull(motif)

# 计算各motif中性别占比、客源占比。
motif_smry %>%
  group_by(seg_n) %>%
  mutate(motif = case_when(
    is.na(motif) ~ paste0(seg_n, "loc_other"), TRUE ~ motif
  )) %>%
  filter(gender != "") %>%
  group_by(seg_n, motif, gender) %>%
  summarise(dailyid_n = sum(dailyid_n), .groups = "drop") %>%
  group_by(seg_n, motif) %>%
  mutate(prop = dailyid_n / sum(dailyid_n)) %>%
  filter(seg_n <= 4) %>%
  mutate(motif = factor(motif, levels = lvl_motif)) %>%
  ggplot() +
  geom_col(aes(motif, prop, fill = gender)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

motif_smry %>%
  group_by(seg_n) %>%
  mutate(motif = case_when(
    is.na(motif) ~ paste0(seg_n, "loc_other"), TRUE ~ motif
  )) %>%
  filter(!is.na(home_pref_grp)) %>%
  group_by(seg_n, motif, home_pref_grp) %>%
  summarise(dailyid_n = sum(dailyid_n), .groups = "drop") %>%
  group_by(seg_n, motif) %>%
  mutate(prop = dailyid_n / sum(dailyid_n)) %>%
  filter(seg_n <= 4) %>%
  mutate(motif = factor(motif, levels = lvl_motif)) %>%
  ggplot() +
  geom_col(aes(motif, prop, fill = home_pref_grp)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

rbind(
  motif_smry %>%
    group_by(seg_n) %>%
    mutate(motif = case_when(
      is.na(motif) ~ paste0(seg_n, "loc_other"), TRUE ~ motif
    )) %>%
    filter(gender != "") %>%
    group_by(seg_n, motif, gender) %>%
    summarise(dailyid_n = sum(dailyid_n), .groups = "drop") %>%
    group_by(seg_n, motif) %>%
    mutate(prop = dailyid_n / sum(dailyid_n)) %>%
    ungroup() %>%
    filter(seg_n <= 4) %>%
    mutate(motif = factor(motif, levels = lvl_motif)) %>%
    mutate(facet = "gender") %>%
    select(motif, facet, facet_val = gender, prop),
  motif_smry %>%
    group_by(seg_n) %>%
    mutate(motif = case_when(
      is.na(motif) ~ paste0(seg_n, "loc_other"), TRUE ~ motif
    )) %>%
    filter(!is.na(home_pref_grp)) %>%
    group_by(seg_n, motif, home_pref_grp) %>%
    summarise(dailyid_n = sum(dailyid_n), .groups = "drop") %>%
    group_by(seg_n, motif) %>%
    mutate(prop = dailyid_n / sum(dailyid_n)) %>%
    ungroup() %>%
    filter(seg_n <= 4) %>%
    mutate(motif = factor(motif, levels = lvl_motif)) %>%
    mutate(facet = "home") %>%
    select(motif, facet, facet_val = home_pref_grp, prop)
) %>%
  filter(!grepl("other", motif, .)) %>%
  ggplot() +
  geom_col(aes(motif, prop, fill = facet_val)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(.~ facet, ncol = 1) +
  labs(x = NULL, y = "Proportion")

## Number of destination ----
# 本地人和游客四季旅途中经过的地点个数有何不同？
seg %>%
  left_join(vis_attr, by = "dailyid") %>%
  filter(!is.na(home_pref_grp)) %>%
  # 每个dailyid经过多少个地点。
  group_by(dailyid, home_pref_grp, season) %>%
  summarise(seg_n = n(), .groups = "drop") %>%
  # 不同客源地的四季每日途径地点数。
  group_by(home_pref_grp, season, seg_n) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(home_pref_grp, season) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(season, prop, fill = as.character(seg_n))) +
  facet_wrap(.~ home_pref_grp, scales = "free_y")

# 如果加入性别维度？
seg %>%
  left_join(vis_attr, by = "dailyid") %>%
  filter(!is.na(home_pref_grp), gender != "") %>%
  # 每个dailyid经过多少个地点。
  group_by(dailyid, home_pref_grp, gender, season) %>%
  summarise(seg_n = n(), .groups = "drop") %>%
  # 不同客源地的四季每日途径地点数。
  group_by(home_pref_grp, gender, season, seg_n) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(home_pref_grp, gender, season) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(season, prop, fill = as.character(seg_n))) +
  facet_grid(gender ~ home_pref_grp, scales = "free_y")

# File for Gephi ----
node <- st_centroid(loc)
node <- data.frame(Id = node$loc_id) %>%
  cbind(
    st_coordinates(node) %>%
      data.frame() %>%
      rename_with(~ c("Longitude", "Latitude"))
  ) %>%
  tibble()
write.csv(
  node, paste0("data_proc/new_od_node_", Sys.Date(), ".csv"), row.names = FALSE
)

library(purrr)
edge_export <- split.data.frame(seg_pair_od, seg_pair_od$qua)
map2(
  edge_export,
  1:4,
  function(x, y) {
    select(x, origin, destination) %>%
      rename_with(~ c("Source", "Target")) %>%
      write.csv(
        ., paste0("data_proc/new_od_edge_", y, "_", Sys.Date(), ".csv"),
        row.names = FALSE
      )
  }
)

# 导出节点。
map2(
  edge_export,
  1:4,
  function(x, y) {
    node %>%
      filter(Id %in% unique(c(x$origin, x$destination))) %>%
      write.csv(
        ., paste0("data_proc/od_node_", y, "_", Sys.Date(), ".csv"),
        row.names = FALSE
      )
  }
)

# From center node ----
# 对于重要节点，计算其出链接的目的地。
# 如果算上所有经过的人。
lapply(
  1:4,
  function(x) {
    # 选取前10个节点作为重要节点。
    center_node <- table(c(edge_export[[x]]$destination, edge_export[[1]]$origin)) %>%
      sort(decreasing = T) %>%
      names() %>%
      head(10)
    # 前10个节点的去向。
    to_top <- edge_export[[x]] %>%
      filter(origin %in% center_node) %>%
      mutate(destination = case_when(
        destination %in% center_node ~ "center node",
        TRUE ~ "other"
      )) %>%
      group_by(origin, destination) %>%
      summarise(n = n(), .groups = "drop")
    return(to_top)
  }
) %>%
  lapply(
    function(x) {
      x %>%
        ggplot() +
        geom_col(aes(origin, n, fill = destination))
    }
  )
# 中心节点出发的，大部分都是去其他中心节点，但是市中心的地点76除外（所有季节），而95和89也不一定去中心节点。

# 如果不算经过的人，只算当天从中心节点出发的人。
# 每个人一天中的出发点只有一个。符合条件的分析对象：从中心节点出发。
# 漏洞：从某个地点出发后，下一步马上去往哪里？还是几次经停都算呢？如果不考虑其后经停，只考虑出发之后的下一步的话，只取第一个节点和第二个节点即可。
tar_dailyid <- seg_id %>%
  filter(seg_id == 1, loc_id %in% center_node) %>%
  pull(dailyid) %>%
  unique()
seg_id %>%
  filter(dailyid %in% tar_dailyid, seg_id == 1 | seg_id == 2) %>%
  mutate(month = month(time), qua = quarter(month)) %>%
  select(-time, -new_loc_id, -new_dailyid) %>%
  pivot_wider(names_from = seg_id, values_from = loc_id) %>%
  rename("origin" = "1", "destination" = "2") %>%
  group_by(qua, origin, destination) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(!is.na(destination))
