# Preparation ----
pacman::p_load(
  lubridate, dplyr, dbscan, sf, tmap, mapview, stringi, showtext, tmap,
  purrr, ggplot2, patchwork, tidyr, RColorBrewer, targets, ggsci, ggthemes,
  stringr
)
showtext_auto()

# tar_make()
tar_load(agoop_amami)
tar_load(amami)
# 自定义目标地点。
loc <- st_read("data_raw/loc/loc62.shp") %>%
  # 计算每个定义地点的面积，单位为平方米。
  st_make_valid() %>%
  mutate(loc_area = st_area(.) %>% as.numeric()) %>%
  st_transform(6668) %>%
  # 删除加計呂麻島（kakeromajima）：人口过少，POI可达性数据缺失，与主岛交通不连续。
  filter(spa_group != "kakeromajima")

# 从轨迹数据中同步删除加計呂麻島的轨迹点（agoop_amami仅含loc_id，无loc_area，
# ka*节点在loc中已被删除，若不同步过滤则后续join会产生NA，导致分析出错）。
agoop_amami <- agoop_amami %>%
  filter(is.na(loc_id) | !grepl("^ka", loc_id))

# 对每个地点，计算其包含的轨迹点个数、涉及的人数。
# Bug: 后面有同名变量。
loc_smry_1 <- agoop_amami %>%
  st_drop_geometry() %>%
  group_by(loc_id, source, qua) %>%
  summarise(
    tp_num = n(),
    dailyid_num = length(unique(dailyid)),
    .groups = "drop"
  )

# 对于每个人，计算其在每个路段的滞留时间和单位面积滞留时间。这里“路段”是指其按照时间顺序经过的时间-地段区间，例如，一个人的轨迹是1-3-2-r-2，则他所经过的路段包括4个定义地点和一个非定义地点（r）。
event <- agoop_amami %>%
  st_drop_geometry() %>%
  arrange(dailyid, time) %>%
  # 排序数据后，对于每个DailyID，当地点发生变化的时候，则地点编号增加1。
  group_by(dailyid) %>%
  # 漏洞：每次滞留称之为“事件”更加合适。换句话说，从原始数据的时空分辨率，到按照地点聚合统计的分辨率，有3级时空尺度：轨迹点，事件，地点。
  mutate(
    event_id = cumsum(loc_id != lag(loc_id, default = first(loc_id))) + 1
  ) %>%
  ungroup() %>%
  # 滞留时间为最大时间减最小时间，加上额外10分钟。加上额外10分钟原因：取样阶段每10分钟取一个点；换言之，否则如果只有一行数据，则滞留时间将为0。
  group_by(source, qua, dailyid, loc_id, event_id) %>%
  summarise(
    event_dur_stay =
      as.numeric(difftime(max(time), min(time), units = "mins"))  + 10,
    .groups = "drop"
  ) %>%
  left_join(loc, by = "loc_id") %>%
  mutate(event_dur_stay_per_area = event_dur_stay / loc_area)

# 按照路段或地点计算：滞留人数，单位面积滞留人数，滞留时间中位数，单位面积滞留时间中位数。
loc_smry <- left_join(
  # 单位DailyID地点滞留时间中位数计算。
  agoop_amami %>%
    st_drop_geometry() %>%
    # agoop_amami仅有loc_id（QGIS空间连接只加入loc_id），需从loc补充loc_area。
    left_join(
      loc %>% st_drop_geometry() %>% select(loc_id, loc_area),
      by = "loc_id"
    ) %>%
    # 先计算每个DailyID的地点滞留时间。
    group_by(source, qua, dailyid, loc_id, loc_area) %>%
    summarise(
      loc_dur_stay =
        as.numeric(difftime(max(time), min(time), units = "mins"))  + 10,
      .groups = "drop"
    ) %>%
    mutate(loc_dur_stay_per_area = loc_dur_stay / loc_area) %>%
    # 再计算中位数。
    group_by(source, qua, loc_id, loc_area) %>%
    summarise(
      mid_loc_dur_stay = median(loc_dur_stay),
      mid_loc_dur_stay_per_area = median(loc_dur_stay_per_area),
      .groups = "drop"
    ),
  # 其他统计指标。
  event %>%
    group_by(source, qua, loc_id, loc_area) %>%
    # 删除非定义地点的数据。
    filter(loc_id != "r") %>%
    summarise(
      # 每个地点的事件滞留时间中位数。
      mid_event_dur_stay = median(event_dur_stay),
      mid_event_dur_stay_per_area = median(event_dur_stay_per_area),
      # 每个地点的滞留人数。
      dailyid_num = length(unique(dailyid)),
      .groups = "drop"
    ) %>%
    mutate(dailyid_num_per_area = dailyid_num / loc_area),
  by = c("source", "qua", "loc_id", "loc_area")
) %>%
  filter(loc_id != "r") %>%
  left_join(loc %>% select(-loc_area), by = "loc_id") %>%
  st_as_sf(sf_column_name = "geometry")
# 漏洞：计算中的一些问题：按照这样的方法统计，得到的是每个季度在各个地点停留的、按照客源区分的总人数。是否需要除以纳入计算的天数，以得到平均每天的滞留人数呢？从目的考虑，未必，因为目的是要看哪些地方滞留人数和滞留时间有差异。虽然不比考虑除以时间，但是可以考虑除以面积。

# 提取起点-终点对。
# 漏洞：对于地点-r-地点，则处理为地点自己指向自己。
od <- event %>%
  # 漏洞：本分析不包含非定义地点。
  filter(loc_id != "r") %>%
  select(source, qua, dailyid, loc_id, event_id) %>%
  group_by(dailyid) %>%
  # 确保按顺序排列。
  arrange(event_id) %>%
  # 获取起点和下一地点。
  mutate(origin = loc_id, destination = lead(loc_id)) %>%
  # 去掉最后一行，因为没有对应终点。
  filter(!is.na(destination)) %>%
  ungroup() %>%
  # 重新排序。
  arrange(dailyid, event_id) %>%
  select(-loc_id, -event_id)

# General description ----
## 图2 ----
# 第1部分：区位图，包含地点。
png(
  paste0("data_proc/re_area_", Sys.Date(), ".png"),
  width = 1000, height = 1200, res = 300
)
ggplot() +
  geom_sf(data = amami, col = "lightgrey") +
  geom_sf(
    data = st_as_sf(st_centroid(loc)) %>%
      mutate(spa_group = factor(spa_group, levels = c(
        "north", "tatsugo", "airport", "city",
        "mangrove", "mid", "uken", "setouchi"
      ))),
    aes(col = spa_group)
  ) +
  labs(col = "Location cluster") +
  scale_color_npg(labels = function(x) str_to_title(x)) +
  scale_x_continuous(
    breaks = c(129.1, 129.3, 129.5, 129.7),
    labels = c("129.1E", "129.3E", "129.5E", "129.7E")
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.99),
    legend.key.height = unit(0.2, "lines"),
    panel.grid.minor = element_blank(),
    legend.justification = c("left", "top"),
    legend.background = element_rect(color = "black")
  )
dev.off()

# 第2部分：原始数据分布。
# Bug: 只取一部分数据作图。
png(
  paste0("data_proc/re_tp_raw_", Sys.Date(), ".png"),
  width = 1000, height = 1200, res = 300
)
set.seed(1234)
ggplot() +
  geom_sf(data = amami, col = "lightgrey") +
  geom_sf(
    data = st_jitter(sample_n(agoop_amami, size = 10000), 0.001),
    size = 0.1, col = "black", alpha = 0.8
  ) +
  scale_x_continuous(
    breaks = c(129.1, 129.3, 129.5, 129.7),
    labels = c("129.1E", "129.3E", "129.5E", "129.7E")
  ) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
dev.off()

# 第3部分：奄美道路图。
# 读取谢于松提取的奄美路线数据。
# Bug：需要写数据来源。
road <- st_read("data_raw/osm_amami_road/研究范围内的道路.shp") %>%
  mutate(
    road_class = case_when(
      fclass %in% c("unclassified", "path", "track", "trunk") ~ "others",
      TRUE ~ fclass
    ),
    road_class = factor(road_class, levels = c(
      "primary", "secondary", "tertiary", "others"
    ))
  )
png(
  paste0("data_proc/road_", Sys.Date(), ".png"),
  width = 1000, height = 1200, res = 300
)
ggplot() +
  geom_sf(data = amami, col = "lightgrey") +
  geom_sf(data = road, aes(col = road_class)) +
  labs(col = "Class") +
  scale_x_continuous(
    breaks = c(129.1, 129.3, 129.5, 129.7),
    labels = c("129.1E", "129.3E", "129.5E", "129.7E")
  ) +
  scale_color_manual(
    breaks = c("primary", "secondary", "tertiary", "others"),
    values = c("darkred", "orange", "darkgreen", "lightgreen")
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.99),
    legend.key.height = unit(0.2, "lines"),
    panel.grid.minor = element_blank(),
    legend.justification = c("left", "top"),
    legend.background = element_rect(color = "black")
  )
dev.off()

# 每个人每天有几个记录点？
agoop_amami %>%
  st_drop_geometry() %>%
  group_by(dailyid) %>%
  summarise(n_log = n(), .groups = "drop") %>%
  ggplot() +
  geom_histogram(aes(n_log), col = "white") +
  theme_bw()

# 第4部分：各类POI。
# 获取文件路径列表。
file_list <- list.files("data_raw/osm_poi", full.names = TRUE) %>%
  grep(".shp$", ., value = TRUE)

# 批量读取并合并。
# 使用 map_df 会将每个文件读取后的 sf 对象合并在一起
# 我们添加一个 .id 参数或手动添加一列来区分 POI 类型
all_poi <- file_list %>%
  map_df(~{
    # 读取 shp 文件
    temp_sf <- st_read(.x, quiet = TRUE)

    # 提取文件名（不带路径和后缀）作为类别名称
    type_name <- tools::file_path_sans_ext(basename(.x))

    # 添加类别列
    temp_sf <- temp_sf %>% mutate(poi_type = type_name)

    return(temp_sf)
  })

# 绘图。
png(
  paste0("data_proc/poi_", Sys.Date(), ".png"),
  width = 1000, height = 1200, res = 300
)
ggplot() +
  geom_sf(data = amami, col = "lightgrey") +
  # 绘制 POI 点，根据类别着色。
  geom_sf(data = all_poi, aes(color = poi_type), size = 0.5, alpha = 0.7) +
  scale_color_tableau(
    palette = "Tableau 10",
    labels = function(x) {
      x %>%
        # 1. 将下划线替换为空格
        str_replace_all("_", " ") %>%
        # 2. 删除指定的后缀（ignore_case = TRUE 确保大小写都能匹配）
        # 使用 | 连接多个词，并匹配前后的空格
        str_remove_all(
          regex(" services| facilities| and utilities", ignore_case = TRUE)
        ) %>%
        # 3. 修剪首尾多余空格并将首字母大写
        str_squish() %>%
        str_to_title()
    }
  ) +
  scale_x_continuous(
    breaks = c(129.1, 129.3, 129.5, 129.7),
    labels = c("129.1E", "129.3E", "129.5E", "129.7E")
  ) +
  labs(color = "POI Type") +
  # 保持坐标系比例一致
  coord_sf() +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.99),
    legend.key.height = unit(0.3, "lines"),
    panel.grid.minor = element_blank(),
    legend.justification = c("left", "top"),
    legend.background = element_rect(color = "black")
  )
dev.off()

## 图3 ----
# 函数：各地点轨迹点数或人数，并显示游客和本地人比例作图。
plt_loc_smry <- function(tar_var) {
  loc_smry_proc <- loc_smry_1 %>%
    pivot_wider(
      id_cols = c(loc_id, qua),
      names_from = source, values_from = all_of(tar_var), values_fill = 0
    ) %>%
    mutate(vis_2_loc = tourist / local, num = tourist + local) %>%
    mutate(
      vis_2_loc_quan = cut(
        vis_2_loc,
        breaks = quantile(vis_2_loc, probs = seq(0, 1, 0.25), na.rm = TRUE),
        labels = 1:4,
        include.lowest = TRUE
      )
    ) %>%
    left_join(loc, by = "loc_id") %>%
    st_as_sf()
  ggplot() +
    geom_sf(data = amami) +
    geom_sf(
      data = st_centroid(loc_smry_proc),
      aes(size = num, col = as.numeric(vis_2_loc_quan)), alpha = 0.5
    ) +
    scale_color_gradient(low = "darkgreen", high = "orange") +
    theme_bw() +
    facet_wrap(.~ qua, nrow = 1) +
    theme(legend.position = "bottom", legend.box = "vertical")
}
# 作图：各地点轨迹点数。
# plt_loc_smry("tp_num")

# 图3。
png(
  paste0("data_proc/fig_3_", Sys.Date(), ".png"),
  width = 2000, height = 3000, res = 300
)
(
  # 每个月有多少人，本地和外地人分别多少？
  # 分季节和客源人数。
  agoop_amami %>%
    st_drop_geometry() %>%
    group_by(source, qua, month) %>%
    summarise(dailyid_num = length(unique(dailyid)), .groups = "drop") %>%
    ggplot() +
    geom_col(aes(month, dailyid_num, fill = qua)) +
    scale_fill_manual(
      breaks = as.character(1:4),
      values = c("#D3A9C5", "#8CD3D6", "#F0B29D", "#7BABDD")
    ) +
    scale_x_continuous(breaks = 1:12, labels = 1:12) +
    facet_wrap(.~ source, labeller = labeller(source = c(
      "local" = "Local", "tourist" = "Tourist"
    )), ncol = 1) +
    labs(
      x = "Month", y = "Numbe of daily ID", fill = "Quarter", title = "(a)"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank()
    )
) / (
  # 分客源分季度下，每个人每天滞留地点数量。
  agoop_amami %>%
    st_drop_geometry() %>%
    group_by(source, qua, dailyid) %>%
    summarise(loc_id_num = length(unique(loc_id)), .groups = "drop") %>%
    ggplot() +
    geom_histogram(aes(loc_id_num), col = "white", binwidth = 1) +
    theme_bw() +
    facet_grid(
      source ~ qua,
      labeller = labeller(source = c("local" = "Local", "tourist" = "Tourist"))
    ) +
    labs(x = "Location number", y = "Count", title = "(b)")
) / (
  # 各季度
  plt_loc_smry("dailyid_num") +
    scale_x_continuous(
      breaks = c(129.1, 129.5), labels = c("129.1E", "129.5E")
    ) +
    labs(
      col = "Tourist/Local rate quartile",
      size = "Daily ID number",
      title = "(c)"
    )
)
dev.off()

# 每个柱子中占比较多的是哪些具体地点？
agoop_amami %>%
  st_drop_geometry() %>%
  # 删除非定义地点。
  filter(loc_id != "r") %>%
  # 删除重复地点。
  select(source, qua, dailyid, loc_id) %>%
  distinct() %>%
  # 合并地点，按地点编号大小排列地点。
  group_by(source, qua, dailyid) %>%
  arrange(as.numeric(loc_id)) %>%
  summarise(
    loc_id_comb = paste(loc_id, collapse = "-"),
    loc_id_num = length(unique(loc_id)),
    .groups = "drop"
  ) %>%
  # 统计每种地点组合数量。
  group_by(source, qua, loc_id_comb, loc_id_num) %>%
  summarise(dailyid_num = n(), .groups = "drop") %>%
  # 取出每组中排名靠前的地点组合。
  group_by(source, qua, loc_id_num) %>%
  arrange(-dailyid_num) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  # 排序。
  select(source, qua, loc_id_num, dailyid_num, loc_id_comb) %>%
  arrange(source, qua, loc_id_num, -dailyid_num)

# Maps of visitor number and duration ----
## Duration ----
# 事件滞留时间中位数。
tm_shape(amami) +
  tm_polygons(col = "white") +
  tm_shape(loc_smry) +
  tm_dots(size = "mid_event_dur_stay", alpha = 0.5) +
  tm_facets(by = "qua", along = "source")
tm_shape(amami) +
  tm_polygons(col = "white") +
  tm_shape(loc_smry) +
  tm_dots(size = "mid_event_dur_stay_per_area", alpha = 0.5) +
  tm_facets(by = "qua", along = "source")

# 地点滞留时间中位数。
# 漏洞：可能受极端值影响。
quantile(loc_smry$mid_loc_dur_stay)
tm_shape(amami) +
  tm_polygons(col = "white") +
  # 漏洞：为了消除极端值的影响，删除只包50%分位数或以下人数的地点。
  tm_shape(loc_smry %>% filter(dailyid_num > 5)) +
  tm_dots(size = "mid_loc_dur_stay", alpha = 0.5) +
  tm_facets(by = "qua", along = "source")
tm_shape(amami) +
  tm_polygons(col = "white") +
  # 漏洞：为了消除极端值的影响，删除只包50%分位数或以下人数的地点。
  tm_shape(loc_smry %>% filter(dailyid_num > 5)) +
  tm_dots(size = "mid_loc_dur_stay_per_area", alpha = 0.5) +
  tm_facets(by = "qua", along = "source")

## Population ----
# 分季节分客源各个地点总滞留人数。
tm_shape(amami) +
  tm_polygons(col = "white") +
  tm_shape(loc_smry) +
  tm_dots(size = "dailyid_num", alpha = 0.5) +
  tm_facets(by = "qua", along = "source")

# 分季节分客源单位面积滞留人数。
# 漏洞：可能受极端值影响。
quantile(loc_smry$dailyid_num)
tm_shape(amami) +
  tm_polygons(col = "white") +
  # 漏洞：为了消除极端值的影响，删除只包50%分位数或以下人数的地点。
  tm_shape(loc_smry %>% filter(dailyid_num > 5)) +
  tm_dots(size = "dailyid_num_per_area", alpha = 0.5) +
  tm_facets(by = "qua", along = "source")

## Pop and duration ----
# 滞留人数和事件滞留时间的关系。
st_drop_geometry(loc_smry) %>%
  ggplot() +
  geom_point(aes(log(dailyid_num), log(mid_event_dur_stay)))

# 单位面积滞留人数和单位面积事件滞留时间的关系。
st_drop_geometry(loc_smry) %>%
  ggplot() +
  geom_point(aes(log(dailyid_num_per_area), log(mid_event_dur_stay_per_area)))
# 漏洞：计算单位道路停留时间？

# 滞留人数和事件滞留时间的关系。
st_drop_geometry(loc_smry) %>%
  ggplot() +
  geom_point(aes(log(dailyid_num), log(mid_loc_dur_stay)))

# 单位面积滞留人数和单位面积事件滞留时间的关系。
st_drop_geometry(loc_smry) %>%
  ggplot() +
  geom_point(aes(log(dailyid_num_per_area), log(mid_loc_dur_stay_per_area)))

# OD analysis ----
# 分客源分季度统计排名靠前的OD对。
od %>%
  group_by(source, qua, origin, destination) %>%
  summarise(flow = n(), .groups = "drop") %>%
  group_by(source, qua) %>%
  arrange(-flow) %>%
  slice_head(n = 20) %>%
  ungroup()

# Export data ----
## Location summary ----
write.csv(loc_smry, "data_proc/loc_smry. csv")

## Gephi data ----
# 导出Gephi作图所需数据。
# 生成节点和边。
node <- st_centroid(loc)
node <- data.frame(Id = node$loc_id) %>%
  cbind(
    st_coordinates(node) %>%
      data.frame() %>%
      rename_with(~ c("Longitude", "Latitude"))
  ) %>%
  tibble()
gephi_data <- od %>%
  group_by(source, qua, origin, destination) %>%
  summarise(flow = n(), .groups = "drop") %>%
  split.data.frame(.$qua)

# 导出边：分季节和客源。
map2(
  rep(1:4, 2),
  rep(c("local", "tourist"), each = 4),
  function(x, y) {
    gephi_data[[x]] %>%
      filter(source == y) %>%
      select(origin, destination, flow) %>%
      rename_with(~ c("Source", "Target", "Weight")) %>%
      write.csv(
        ., paste0("data_proc/od_edge_", y, "_", x, ".csv"),
        row.names = FALSE
      )
  }
)

# 导出节点：分季节和客源。
map2(
  rep(1:4, 2),
  rep(c("local", "tourist"), each = 4),
  function(x, y) {
    node %>%
      filter(
        Id %in% unique(c(gephi_data[[x]]$origin, gephi_data[[x]]$destination))
      ) %>%
      write.csv(
        ., paste0("data_proc/od_node_", y, "_", x, ".csv"),
        row.names = FALSE
      )
  }
)

# 导出边：分季节不分客源。
lapply(
  c(1:4),
  function(x) {
    gephi_data[[x]] %>%
      group_by(qua, origin, destination) %>%
      summarise(flow = sum(flow), .groups = "drop") %>%
      select(origin, destination, flow) %>%
      rename_with(~ c("Source", "Target", "Weight")) %>%
      write.csv(
        ., paste0("data_proc/od_edge_allsrc_", x, ".csv"),
        row.names = FALSE
      )
  }
)

# 导出节点：分季节不分客源。
lapply(
  c(1:4),
  function(x) {
    node %>%
      filter(
        Id %in% unique(c(gephi_data[[x]]$origin, gephi_data[[x]]$destination))
      ) %>%
      write.csv(
        ., paste0("data_proc/od_node_allsrc_", x, ".csv"),
        row.names = FALSE
      )
  }
)

