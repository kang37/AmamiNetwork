# Preparation ----
pacman::p_load(
  lubridate, dplyr, dbscan, sf, tmap, mapview, stringi,
  ggplot2, tidyr, RColorBrewer, targets
)
tar_make()
tar_load(amami)
tar_load(gis_agoop_coord_cluster)
tar_load(loc)
tar_load(pref_city_code)

# 漏洞：增加本地/外地区分；增加季度信息。
gis_agoop_coord_cluster <- gis_agoop_coord_cluster %>%
  mutate(
    source = case_when(
      home_citycode %in% c(
        "46222", "46505", "46502", "46504", "46501", "46506", "46521",
        "46522", "46523"
      ) ~ "local",
      TRUE ~ "tourist"
    ),
    qua = quarter(month)
  )

# General description ----
# 选择三个轨迹点最多的dailyid展示轨迹点。
example_dailyid <- gis_agoop_coord_cluster %>%
  st_drop_geometry() %>%
  # 每个人各个小时的记录数量。
  group_by(dailyid) %>%
  summarise(log = n(), .groups = "drop") %>%
  arrange(-log) %>%
  pull(dailyid) %>%
  head(10)
# 可视化轨迹。
gis_agoop_coord_cluster %>%
  filter(dailyid == example_dailyid[7]) %>%
  mutate(dailyid_short = substr(dailyid, 1, 5)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
  mapview(zcol = "hour", col.region = colorRampPalette(c("red", "yellow", "blue")))

# 每个人每天有几个记录点？
gis_agoop_coord_cluster %>%
  st_drop_geometry() %>%
  group_by(dailyid) %>%
  summarise(n_log = n(), .groups = "drop") %>%
  ggplot() +
  geom_histogram(aes(n_log), col = "white") +
  theme_bw()

# 每个人每天滞留地点数量？
gis_agoop_coord_cluster %>%
  st_drop_geometry() %>%
  group_by(dailyid) %>%
  summarise(cluster_num = length(unique(cluster)), .groups = "drop") %>%
  ggplot() +
  geom_histogram(aes(cluster_num), col = "white") +
  theme_bw()

# 每个月有多少人，本地和外地人分别多少？
gis_agoop_coord_cluster %>%
  st_drop_geometry() %>%
  group_by(month, source) %>%
  summarise(dailyid_num = length(unique(dailyid)), .groups = "drop") %>%
  ggplot() +
  geom_col(aes(month, dailyid_num, fill = source))
# 每个季度有多少人？
gis_agoop_coord_cluster %>%
  st_drop_geometry() %>%
  group_by(qua, source) %>%
  summarise(dailyid_num = length(unique(dailyid)), .groups = "drop") %>%
  ggplot() +
  geom_col(aes(qua, dailyid_num, fill = source))

# 每个地点滞留多少人？只取人数较多的地点。
# 基本上都表现为第三季度人数最多。
gis_agoop_coord_cluster %>%
  st_drop_geometry() %>%
  group_by(qua, cluster) %>%
  summarise(dailyid_num = length(unique(dailyid)), .groups = "drop") %>%
  arrange(-dailyid_num) %>%
  head(100) %>%
  mutate(cluster = as.character(cluster)) %>%
  ggplot() +
  geom_line(aes(qua, dailyid_num)) +
  theme_bw() +
  facet_wrap(.~ cluster, scales = "free_y")

# Trajectory between clusters ----
# Further divide segments: a dailyid has more than one segment even for a cluster. For instance, the pathway c1-c2-c1-c3 has 2 c1 segments.
# Should further divid segments: a dailyid has more than one segment even for a cluster. For instance, the pathway c1-c2-c1-c3 has 2 c1 segments.
# Get segment ID for each dailyid.
seg_id <- gis_agoop_coord_cluster %>%
  st_drop_geometry() %>%
  arrange(dailyid, time) %>%
  group_by(dailyid) %>%
  mutate(
    new_dailyid = c(dailyid != lag(dailyid)),
    new_cluster = c(cluster != lag(cluster)),
    new_dailyid = case_when(is.na(new_dailyid) ~ TRUE, TRUE ~ new_dailyid),
    new_cluster = case_when(is.na(new_cluster) ~ TRUE, TRUE ~ new_cluster)
  ) %>%
  select(dailyid, time, cluster, new_dailyid, new_cluster) %>%
  filter(new_dailyid + new_cluster >= 1) %>%
  mutate(seg_id = row_number())

# The duration of stay of each dailyid in each segment.
seg <- gis_agoop_coord_cluster %>%
  # Add segment sequence.
  st_drop_geometry() %>%
  arrange(dailyid, hour) %>%
  left_join(seg_id, by = c("dailyid", "cluster", "time")) %>%
  ungroup() %>%
  tidyr::fill(seg_id) %>%
  # Calculate duration of stay.
  group_by(qua, dailyid, cluster, seg_id) %>%
  summarise(duration_stay = max(time) - min(time), .groups = "drop") %>%
  # Remove noise points.
  filter(cluster != 0) %>%
  # Bug: Duration of stat = 0 also excluded, might introduce bias from signal lose, i.e., only one points is recorded in a cluster.
  filter(duration_stay > 600) %>%
  # Turn duration of stay from second to minute.
  mutate(
    duration_stay = as.numeric(duration_stay) / 60,
    cluster = as.character(cluster)
  )

# Plot duration of stay of each cluster for each visitor, based on segment duration stay data.
# 漏洞：滞留时间和人数是否应该除以面积呢？
ggplot(data = seg, aes(cluster, duration_stay)) +
  geom_boxplot() +
  geom_jitter(aes(col = as.character(qua)), alpha = 0.3) +
  labs(x = "Cluster", y = "Duration of stay") +
  coord_flip()
# 漏洞：看看面积和滞留时间的关系？

# Table.
seg %>%
  group_by(cluster) %>%
  summarise(
    mean_dur_stay = mean(duration_stay, na.rm = TRUE),
    mid_dur_stay = median(duration_stay, na.rm = TRUE),
    sd_dur_stay = sd(duration_stay, na.rm = TRUE),
    vc_dur_stay = sd_dur_stay / mean_dur_stay,
    n_dur_stay = n()
  ) %>%
  select(cluster, mid_dur_stay, mean_dur_stay, sd_dur_stay, vc_dur_stay)

# 平均来看，各个地点滞留时间是多久？
# 漏洞：要计算每次滞留时间的平均，还是将每个人在同一个地点的滞留时间加起来呢？
seg %>%
  group_by(cluster) %>%
  summarise(duration_stay = mean(duration_stay), .groups = "drop") %>%
  arrange(-duration_stay)

# Most visitors stay in a cluster; segment visited is similar to cluster visited number.
# 每个人访问了多少个cluster。
seg %>%
  select(dailyid, cluster) %>%
  distinct() %>%
  group_by(dailyid) %>%
  summarise(seg_n = n(), .groups = "drop") %>%
  ggplot() +
  geom_histogram(aes(seg_n), binwidth = 1)

# In each mode, what is the structure?
# Bug: Take cluster 76 as an example.
seg_time %>%
  left_join(
    seg_time %>%
      group_by(dailyid) %>%
      summarise(cluster_n = n(), .groups = "drop") %>%
      filter(cluster_n == 76),
    by = "dailyid"
  ) %>%
  filter(!is.na(cluster_n)) %>%
  pull(seg_id) %>%
  table()

# Pairwise movement matrix.
# Has the data.frame been arranged in order?
# Bug: If a dailyid mostly stay in a cluster, but s/he goes out of the cluster usually? Then "c1-edge-c1-edge-c1" will become "c1".
# Bug: It is equal to that we remove the dailyid who stays in a cluster for the whole day.
seg_pair_od <- seg %>%
  arrange(dailyid, seg_id) %>%
  mutate(
    new_dailyid = c(dailyid != lag(dailyid)),
    new_cluster = c(cluster != lag(cluster)),
    new_dailyid = case_when(is.na(new_dailyid) ~ TRUE, TRUE ~ new_dailyid),
    new_cluster = case_when(is.na(new_cluster) ~ TRUE, TRUE ~ new_cluster),
    # If the answer is yes to either question, then the state is changed.
    state_chg = c(new_dailyid | new_cluster)
  ) %>%
  filter(state_chg) %>%
  group_by(dailyid) %>%
  mutate(
    destination = cluster, origin = lag(cluster)
  ) %>%
  filter(!is.na(origin))

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
# Bug: Should remove the noise (cluster = 0)?
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
    # If it is a new dailyid, or if it enters a new cluster.
    new_dailyid = c(dailyid != lag(dailyid)),
    new_cluster_id = c(cluster_id != lag(cluster_id)),
    new_dailyid =
      case_when(is.na(new_dailyid) ~ TRUE, TRUE ~ new_dailyid),
    new_cluster_id =
      case_when(is.na(new_cluster_id) ~ TRUE, TRUE ~ new_cluster_id),
    # If the answer is yes to either question, then the state is changed.
    state_chg = c(new_dailyid | new_cluster_id)
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
  select(dailyid, visit_id, cluster_id) %>%
  distinct() %>%
  mutate(cluster_id = as.character(cluster_id)) %>%
  group_by(dailyid) %>%
  summarise(traj_1 = paste0(cluster_id, collapse = "-")) %>%
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
  mutate(cluster = gis_agoop_coord_filt_sample$cluster) %>%
  group_by(cluster) %>%
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
        od_node %>% rename(origin = cluster, ori_lon = lon, ori_lat = lat),
        by = "origin"
      ) %>%
      left_join(
        od_node %>% rename(destination = cluster, dest_lon = lon, dest_lat = lat),
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
    data = od_node, aes(x = lon, y = lat, label = cluster), size = 3
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
        od_node %>% rename(origin = cluster, ori_lon = lon, ori_lat = lat),
        by = "origin"
      ) %>%
      left_join(
        od_node %>% rename(destination = cluster, dest_lon = lon, dest_lat = lat),
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
    data = od_node, aes(x = lon, y = lat, label = cluster), size = 3
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
write.csv(node, "data_proc/new_od_node.csv", row.names = FALSE)

seg_pair_od %>%
  # Bug: Should ungroup earlier.
  ungroup() %>%
  select(origin, destination) %>%
  rename_with(~ c("Source", "Target")) %>%
  write.csv(., "data_proc/new_od_edge.csv", row.names = FALSE)
