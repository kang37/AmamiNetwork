# Preparation ----
pacman::p_load(
  lubridate, dplyr, dbscan, sf, tmap, mapview, stringi, showtext, tmap,
  purrr, ggplot2, patchwork, tidyr, RColorBrewer, targets, ggsci
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
  st_transform(6668)

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
# 地点图和原始数据分布，分成3部分：区位图，原始数据分布，各地点轨迹点数分布。
# 第1部分：区位图，包含地点。
png(
  paste0("data_proc/re_area_", Sys.Date(), ".png"),
  width = 1500, height = 1500, res = 300
)
ggplot() +
  geom_sf(data = amami, col = "lightgrey") +
  geom_sf(data = st_as_sf(st_centroid(loc)), aes(col = spa_group)) +
  labs(col = "Location group") +
  scale_color_npg() +
  theme_bw() +
  theme(
    legend.position = c(0.01, 0.99),
    panel.grid.minor = element_blank(),
    legend.justification = c("left", "top"),
    legend.background = element_rect(color = "black")
  )
dev.off()

# 第2部分：原始数据分布。
# Bug: 只取一部分数据作图。
png(
  paste0("data_proc/re_tp_raw_", Sys.Date(), ".png"),
  width = 1500, height = 1500, res = 300
)
set.seed(1234)
ggplot() +
  geom_sf(data = amami, col = "lightgrey") +
  geom_sf(
    data = st_jitter(sample_n(agoop_amami, size = 50000), 0.001),
    size = 0.1, col = "black", alpha = 0.8
  ) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
dev.off()

# 第3部分：各地点轨迹点数分布。
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
      aes(size = num / 1000, col = as.numeric(vis_2_loc_quan)), alpha = 0.5
    ) +
    scale_color_gradient(low = "darkgreen", high = "orange") +
    theme_bw() +
    labs(
      col = "Tourist/Local\nquartile", size = "Track point\nnumber (x 1000)"
    ) +
    facet_wrap(.~ qua) +
    theme(axis.text.x = element_text(angle = 90))
}
# 作图：各地点轨迹点数。
png(
  paste0("data_proc/re_tp_num_", Sys.Date(), ".png"),
  width = 1800, height = 1500, res = 300
)
plt_loc_smry("tp_num")
dev.off()
plt_loc_smry("tp_num") %>% saveRDS("temp_fig/re_tp_num.rds")
# 作图：各地点人数。
plt_loc_smry("dailyid_num")

# 每个人每天有几个记录点？
agoop_amami %>%
  st_drop_geometry() %>%
  group_by(dailyid) %>%
  summarise(n_log = n(), .groups = "drop") %>%
  ggplot() +
  geom_histogram(aes(n_log), col = "white") +
  theme_bw()

# 每个月有多少人，本地和外地人分别多少？
# 分季节和客源人数。
p_pop_season_group <- agoop_amami %>%
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
    "all" = "All", "local" = "Local", "tourist" = "Tourist"
  )), ncol = 1) +
  labs(x = "Month", y = "Numbe of daily ID", fill = "Quarter") +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank()
  )
print(p_pop_season_group)
saveRDS(p_pop_season_group, "temp_fig/p_pop_season_group.rds")

# 分客源分季度下，每个人每天滞留地点数量。
# 分图方案。
lapply(
  list("local", "tourist"),
  function(x) {
    agoop_amami %>%
      st_drop_geometry() %>%
      filter(source %in% x) %>%
      group_by(source, qua, dailyid) %>%
      summarise(loc_id_num = length(unique(loc_id)), .groups = "drop") %>%
      ggplot() +
      geom_histogram(aes(loc_id_num), col = "white", binwidth = 1) +
      theme_bw() +
      facet_wrap(.~ qua, scales = "free_y", nrow = 1) +
      labs(
        x = "Location number",
        y = paste(tools::toTitleCase(x), "daily ID count", collapse = " ")
      ) +
      lims(x = c(0, 15), y = c(0, 2000))
  }
) %>%
  Reduce("/", .) %>%
  saveRDS("temp_fig/visit_loc_hist.rds")

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

# Results ----
# 地点地图。
loc %>%
  st_centroid() %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf()


