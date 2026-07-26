# Node index ----
# 加载包。
library(stringr)
library(readxl)
library(scatterpie)
library(ggsci)

# 获取所有csv文件路径。
file_paths <- list.files(
  path = "data_raw/gephi_output",
  full.names = TRUE
)

# 定义解析函数
parse_file_info <- function(file_path) {
  # 提取文件名：去除路径和扩展名。
  file_name <- tools::file_path_sans_ext(basename(file_path))
  file_name <- gsub("gephi_node_export_", "", file_name)
  # 使用正则表达式提取季节和群体。
  matches <- str_match(file_name, "(\\w+)_(\\d+)")
  # 返回解析结果。
  tibble(
    season = as.integer(matches[1, 3]),
    vis_src = matches[1, 2],
    file_path = file_path
  )
}

# 解析所有文件信息。
file_info <- map_dfr(file_paths, parse_file_info)

# 读取并合并所有CSV文件。
combined_data <- pmap(
  list(file_info$file_path, file_info$vis_src, file_info$season),
  function(x, y, z) {
    read.csv(x) %>%
      tibble() %>%
      mutate(vis_src = y, season = z, .before = 1)
  }
) %>%
  bind_rows() %>%
  rename_with(~ tolower(gsub("\\.", "_", .x))) %>%
  # 更改列名：在小写字母和"centrality"之间加下划线。
  rename_with(
    ~ gsub("([a-z]centrality)", "", .x),
    matches("centrality$")
  ) %>%
  rename(
    "harmonic" = "harmonicclosnes",
    "betweeness" = "betweenes",
    "closeness" = "closnes"
  ) %>%
  # 删除加計呂麻島节点（ka1-ka8）。
  filter(!grepl("^ka", id))

# 标记每个 vis_src × season 中节点数最多的 modularity_class（top_mod == 1）。
# 暂时注释：gephi_output CSV 缺少 modularity_class 列，需重新从 Gephi 导出后启用。
# combined_data <- combined_data %>%
#   group_by(vis_src, season, modularity_class) %>%
#   mutate(mod_size = n()) %>%
#   group_by(vis_src, season) %>%
#   mutate(top_mod = as.integer(mod_size == max(mod_size))) %>%
#   ungroup() %>%
#   select(-mod_size)

# Demand ----
## 图6 ----
# 函数：画特定群体各季节各中心度地图。
plt_demand_map <- function(visitor_x) {
  ggplot() +
    geom_sf(data = amami, col = "lightgrey") +
    geom_sf(
      data = loc %>%
        st_centroid() %>%
        left_join(
          combined_data %>% filter(vis_src == visitor_x),
          by = c("loc_id" = "id")
        ) %>%
        select(
          "loc_id", "spa_group", "vis_src", "season",
          "degree", "closeness", "harmonic"
        ) %>%
        pivot_longer(
          cols = c(degree, closeness, harmonic),
          names_to = "centrality",
          values_to = "cen_val"
        ) %>%
        # 对各个中心度进行标准化。
        group_by(centrality) %>%
        mutate(
          # Min-Max 标准化公式：(x - min(x)) / (max(x) - min(x))
          cen_val_normalized = (cen_val - min(cen_val, na.rm = TRUE)) /
            (max(cen_val, na.rm = TRUE) - min(cen_val, na.rm = TRUE))
        ) %>%
        ungroup() %>%
        # 修改变量类型。
        mutate(spa_group = factor(spa_group, levels = c(
          "north", "tatsugo", "airport", "city",
          "mangrove", "mid", "uken", "setouchi"
        ))),
      aes(size = cen_val_normalized, col = spa_group), alpha = 0.6
    ) +
    scale_color_npg(labels = function(x) str_to_title(x)) +
    scale_x_continuous(
      breaks = c(129.1, 129.3, 129.5, 129.7),
      labels = c("129.1E", "129.3E", "129.5E", "129.7E")
    ) +
    labs(col = "Location cluster") +
    scale_size_continuous(
      name = "Normalized Centrality", range = c(0.1, 3)
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90),
      panel.grid = element_line(color = "white"),
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 14)
    ) +
    facet_grid(centrality ~ season)
}
# 居民各季节各项中心度。
png(
  "data_proc/loc_demand_map_local.png",
  width = 3000, height = 2000, res = 300
)
plt_demand_map("local")
dev.off()
# 游客各季节各项中心度。
png(
  "data_proc/loc_demand_map_tourist.png",
  width = 3000, height = 2000, res = 300
)
plt_demand_map("tourist")
dev.off()

# 第三部分：各用户群体不同地点组团中分季度中心度的中值对比。
png(
  paste0("data_proc/loc_cen_mid_", Sys.Date(), ".png"),
  width = 1400, height = 1000, res = 300
)
loc %>%
  st_drop_geometry() %>%
  left_join(
    combined_data %>% filter(vis_src %in% c("local", "tourist")),
    by = c("loc_id" = "id")
  ) %>%
  select(
    "loc_id", "spa_group", "vis_src", "season",
    "degree", "closeness", "harmonic"
  ) %>%
  group_by(vis_src, spa_group, season) %>%
  summarise(
    across(c(degree, closeness, harmonic), function(x) median(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(degree, closeness, harmonic),
    names_to = "centrality",
    values_to = "cen_val"
  ) %>%
  mutate(
    spa_group = str_to_title(spa_group),
    vis_src = str_to_title(vis_src),
    centrality = str_to_title(centrality)
  ) %>%
  ggplot() +
  geom_point(aes(spa_group, cen_val, col = as.factor(season)), alpha = 0.8) +
  facet_grid(centrality ~ vis_src, scale = "free") +
  labs(x = "Location cluster", y = "Centrality", col = "Quarter") +
  scale_color_manual(
    values = c("1" = "#FF9EBC", "2" = "#4DAF4A", "3" = "#E41A1C", "4" = "#377EB8"),
    labels = c("1" = "Spring", "2" = "Summer", "3" = "Autumn", "4" = "Winter")
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
dev.off()

# Supply ----
## 图7 ----
# 定义不同可达时间段的权重。
poi_access_weight <-
  setNames(sapply(seq(5, 30, 5), function(x) 1/x), seq(5, 30, 5))

# POI表格路径。
poi_file_path <- "data_raw/loc_poi_overlay.xlsx"

# 函数：处理单个POI表格。
proc_poi_sheet <- function(sheet_name) {
  # 读取表格第1行：包含POI类型和列名。
  row_first <- read_excel(poi_file_path, sheet = sheet_name, n_max = 1)
  poi_type <- names(row_first)[[2]]
  # 读取表格主要数据。
  df <- read_excel(poi_file_path, sheet = sheet_name, skip = 1)[, -2] %>%
    rename_with(~ c(
      "loc_id",
      paste(rep(c("walk", "drive"), each = 6), row_first[, 3:14], sep = "_")
    ))

  # 加权计算可达性指标：给行人赋予更高权重。
  walk_score <- as.matrix(select(df, contains("walk"))) %*% poi_access_weight
  drive_score <- as.matrix(select(df, contains("drive"))) %*% poi_access_weight
  tibble(
    loc_id = df$loc_id,
    access = as.numeric(0.6 * walk_score + 0.4 * drive_score)
  ) %>%
    rename_with(~ c("loc_id", poi_type))
}

# 处理所有POI可达性表格，并合并结果。
loc_poi_access <- lapply(excel_sheets(poi_file_path), proc_poi_sheet) %>%
  reduce(left_join, by = "loc_id") %>%
  rename_with(~ tolower(.x))
# 查看各地点可达性。
# 定义可达性字段
access_cols <- c(
  "education", "government", "health", "ac",
  "retail", "tourism"
)

png(
  "data_proc/loc_poi_access_map.png",
  width = 3000, height = 2000, res = 300
)
ggplot() +
  geom_sf(data = amami, col = "lightgrey") +
  geom_sf(
    data = loc %>%
      st_centroid() %>%
      left_join(loc_poi_access, by = "loc_id") %>%
      select(loc_id, spa_group, all_of(access_cols)) %>%
      pivot_longer(
        cols = all_of(access_cols),
        names_to = "poi",
        values_to = "Accessibility"
      ) %>%
      mutate(
        spa_group = factor(spa_group, levels = c(
          "north", "tatsugo", "airport", "city",
          "mangrove", "mid", "uken", "setouchi"
        )),
        poi = case_when(
          poi == "ac" ~ "Accommodation & Food",
          poi == "education" ~ "Education",
          poi == "government" ~ "Government",
          poi == "health" ~ "Health",
          poi == "retail" ~ "Commerce",
          poi == "tourism" ~ "Tourism & Recreation",
        )
      ),
    aes(size = Accessibility, col = spa_group), alpha = 0.6
  ) +
  labs(col = "Location Cluster") +
  scale_color_npg(labels = function(x) str_to_title(x)) +
  scale_x_continuous(
    breaks = c(129.1, 129.3, 129.5, 129.7),
    labels = c("129.1E", "129.3E", "129.5E", "129.7E")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid = element_line(color = "white")
  ) +
  facet_wrap(.~ poi, nrow = 2)
dev.off()

# 导出对应数据。
loc %>%
  st_drop_geometry() %>%
  left_join(loc_poi_access, by = "loc_id") %>%
  select(loc_id, all_of(access_cols)) %>%
  write.csv("data_proc/loc_poi_access.csv")

# Demand and supply ----
# 分客源-季节的各地点各类供需比率。
loc_dem_sup <-
  list(
    # 本地人各项需求。
    combined_data %>%
      left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
      filter(vis_src == "local") %>%
      mutate(
        ds_edu = education / degree,
        ds_gov = government/ degree,
        ds_health = health / closeness,
        ds_retail_close = retail / closeness,
        ds_retail_harmonic = retail / harmonic
      ) %>%
      # 将无限大的结果转化为0：对应供给非0而需求为0的地点-季节。
      mutate(across(contains("ds_"), ~ ifelse(is.infinite(.x), 1, .x))) %>%
      # 对每个地点的供需比率进行标准化。
      group_by(vis_src) %>%
      mutate(across(
        contains("ds_"),
        ~ (.x - min(.x, na.rm = T))/(max(.x, na.rm = T) - min(.x, na.rm = T))
      )) %>%
      ungroup() %>%
      # 对一对多的供需配对，计算供需比率加权平均值。
      mutate(
        # 更强调”平均可达性”，harmonic处理偏远点，用于微调。
        ds_retail_mix = ds_retail_close * 0.7 + ds_retail_harmonic * 0.3
      ) %>%
      # 转化为长数据。
      select(vis_src, id, season, contains("ds")) %>%
      select(
        -c(ds_retail_close, ds_retail_harmonic)
      ) %>%
      pivot_longer(
        cols = contains("ds_"), names_to = "ds_cat", values_to = "ds_val"
      ),
    # 游客各项需求。
    combined_data %>%
      left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
      filter(vis_src == "tourist") %>%
      mutate(
        ds_accomfood_degree = ac / degree,
        ds_accomfood_close = ac / closeness,
        ds_retail_degree = retail / degree,
        ds_retail_harmonic = retail / harmonic,
        ds_tour_degree = tourism / degree,
        ds_tour_close = tourism / closeness,
        ds_tour_harmonic = tourism / harmonic,
      ) %>%
      # 将无限大的结果转化为0：对应供给非0而需求为0的地点-季节。
      mutate(across(contains("ds_"), ~ ifelse(is.infinite(.x), 1, .x))) %>%
      # 对每个地点的供需比率进行标准化。
      group_by(vis_src) %>%
      mutate(across(
        contains("ds_"),
        ~ (.x - min(.x, na.rm = T))/(max(.x, na.rm = T) - min(.x, na.rm = T))
      )) %>%
      ungroup() %>%
      # 对一对多的供需配对，计算供需比率加权平均值。
      mutate(
        # 游客热度主导，closeness补充反映“是否方便到达”。
        ds_accomfood_mix = ds_accomfood_degree * 0.7 + ds_accomfood_close * 0.3,
        # 热度主导，harmonic保留广覆盖性。
        ds_retail_mix = ds_retail_degree * 0.7 + ds_retail_harmonic * 0.3,
        # 热度主导 + 中心性支持 + 修正远点。
        ds_tour_mix =
          ds_tour_degree * 0.5 + ds_tour_close * 0.3 + ds_tour_harmonic * 0.2
      ) %>%
      # 转化为长数据。
      select(vis_src, id, season, contains("ds")) %>%
      select(-c(
        ds_accomfood_degree, ds_accomfood_close,
        ds_retail_degree, ds_retail_harmonic,
        ds_tour_degree, ds_tour_close, ds_tour_harmonic
      )) %>%
      pivot_longer(
        cols = contains("ds_"), names_to = "ds_cat", values_to = "ds_val"
      )
  ) %>%
  bind_rows() %>%
  # 获得经纬度信息。
  left_join(st_centroid(loc), by = c("id" = "loc_id")) %>%
  st_as_sf() %>%
  mutate(long = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

# 挑选出各客源-季节-需求中，供需比率最低的地点。
loc_dem_sup_min <- loc_dem_sup %>%
  group_by(vis_src, ds_cat) %>%
  slice_min(order_by = ds_val, n = 30) %>%
  ungroup()

# 密度图：分服务类型和客源，不分季节，比较各组团供给比率。
lapply(
  c("local", "tourist"),
  function(x) {
    loc_dem_sup %>%
      filter(vis_src == x) %>%
      ggplot() +
      geom_density(aes(log(ds_val))) +
      facet_grid(ds_cat ~ spa_group) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90))
  }
)

## 图8 ----
# 供需指数变量名和对应标签。
ds_label <- c(
  "ds_accomfood_mix" = "Accommodation & Food",
  "ds_retail_mix" = "Commerce",
  "ds_edu" = "Education",
  "ds_gov" = "Government",
  "ds_health" = "Health",
  "ds_tour_mix" = "Tourism & Recreation"
)

# 函数：用于画带有供需饼图的地图。
plt_ds_map <- function(vis_src_x) {
  plt_data <- loc_dem_sup_min %>%
    filter(vis_src == vis_src_x) %>%
    mutate(ds_val_fill = 1) %>%
    pivot_wider(
      id_cols = c(id, season, long, lat),
      names_from = ds_cat, values_from = ds_val_fill, values_fill = 0
    ) %>%
    mutate(radius = 0.02)

  ggplot() +
    geom_sf(data = amami, fill = "white") +
    geom_sf(data = st_centroid(loc), size = 1, col = "darkgrey", alpha = 0.8) +
    geom_scatterpie(
      data = plt_data,
      aes(x = long, y = lat, r = radius),
      cols= grep("^ds_", names(plt_data), value = TRUE),
      linewidth = 0.1, color = "white", alpha=0.9
    ) +
    scale_fill_npg(labels = ds_label) +
    scale_x_continuous(
      breaks = c(129.1, 129.3, 129.5, 129.7),
      labels = c("129.1E", "129.3E", "129.5E", "129.7E")
    ) +
    labs(fill = "Service") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90),
      panel.grid = element_line(color = "white")
    ) +
    facet_wrap(
      .~ season, nrow = 1,
      labeller = labeller(season = c(
        "1" = "Quarter 1", "2" = "Quarter 2",
        "3" = "Quarter 3", "4" = "Quarter 4"
      ))
    )
}

# 作图。
# 本地人供需。
png(
  paste0("data_proc/ds_map_local_", Sys.Date(), ".png"),
  width = 3500, height = 1000, res = 300
)
plt_ds_map("local")
dev.off()

# 游客供需。
png(
  paste0("data_proc/ds_map_tourist_", Sys.Date(), ".png"),
  width = 3500, height = 1000, res = 300
)
plt_ds_map("tourist")
dev.off()

# 如果混合起来呢？
plt_data <- loc_dem_sup_min %>%
  mutate(ds_val_fill = 1) %>%
  pivot_wider(
    id_cols = c(vis_src, id, season, long, lat),
    names_from = ds_cat, values_from = ds_val_fill, values_fill = 0
  ) %>%
  mutate(radius = 0.02)

png(
  paste0("data_proc/ds_map_all_", Sys.Date(), ".png"),
  width = 3500, height = 2500, res = 300
)
ggplot() +
  geom_sf(data = amami) +
  geom_scatterpie(
    data = plt_data,
    aes(x = long, y = lat, r = radius),
    cols= grep("^ds_", names(plt_data), value = TRUE),
    linewidth = 0.1, color = "white", alpha=0.9
  ) +
  scale_fill_npg() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(vis_src ~ season)
dev.off()

# 分客源-季节下各供给率低地点对比。
ggplot(filter(loc_dem_sup_min, vis_src == "local")) +
  geom_col(aes(id, ds_val)) +
  facet_grid(ds_cat ~ season) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
ggplot(filter(loc_dem_sup_min, vis_src == "tourist")) +
  geom_col(aes(id, ds_val)) +
  facet_grid(ds_cat ~ season) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# 分客源-季节下各供给率低地点对比地图。
png(
  paste0("data_proc/ds_size_local_", Sys.Date(), ".png"),
  width = 3500, height = 2500, res = 300
)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "local"),
    # aes(size = ds_val),
    alpha = 0.5, col = "red"
  ) +
  facet_grid(ds_cat ~ season) +
  theme_bw()
dev.off()

png(
  paste0("data_proc/ds_size_tourist_", Sys.Date(), ".png"),
  width = 3500, height = 2500, res = 300
)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "tourist"),
    # aes(size = ds_val),
    alpha = 0.5, col = "red"
  ) +
  facet_grid(ds_cat ~ season) +
  theme_bw()
dev.off()

# 分客源-季节下各供给率低地点数量对比地图。
png(
  paste0("data_proc/ds_number_local_", Sys.Date(), ".png"),
  width = 3500, height = 2500, res = 300
)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "local") %>%
      group_by(vis_src, season, spa_group, ds_cat) %>%
      summarise(n = n(), geometry = first(geometry), .groups = "drop"),
    aes(size = n),
    alpha = 0.5, col = "red"
  ) +
  geom_sf_text(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "local") %>%
      group_by(vis_src, season, spa_group, ds_cat) %>%
      summarise(n = n(), geometry = first(geometry), .groups = "drop"),
    aes(label = n), size = 3
  ) +
  facet_grid(ds_cat ~ season) +
  theme_bw()
dev.off()

png(
  paste0("data_proc/ds_num_tourist_", Sys.Date(), ".png"),
  width = 3500, height = 2500, res = 300
)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "tourist") %>%
      group_by(vis_src, season, spa_group, ds_cat) %>%
      summarise(n = n(), geometry = first(geometry), .groups = "drop"),
    aes(size = n),
    alpha = 0.5, col = "red"
  ) +
  geom_sf_text(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "tourist") %>%
      group_by(vis_src, season, spa_group, ds_cat) %>%
      summarise(n = n(), geometry = first(geometry), .groups = "drop"),
    aes(label = n), size = 3
  ) +
  facet_grid(ds_cat ~ season) +
  theme_bw()
dev.off()

## 图9 ----
# 各地点组团中供给短缺地点数量的比例。
png(
  "data_proc/loc_supply_short_percent.png",
  width = 2000, height = 1000, res = 300
)
loc_dem_sup_min %>%
  group_by(vis_src, ds_cat, spa_group, season) %>%
  summarise(loc_n = n(), .groups = "drop") %>%
  left_join(
    loc %>%
      st_drop_geometry() %>%
      group_by(spa_group) %>%
      summarise(loc_n_tot = n(), .groups = "drop"),
    by = "spa_group"
  ) %>%
  mutate(loc_rate = loc_n / loc_n_tot) %>%
  # 补全所有分类组合，并去除不必要的行。
  complete(ds_cat, vis_src, season, spa_group) %>%
  filter(
    !(
      ds_cat %in% c(
        "ds_tour_mix", "ds_accomfood_mix"
      ) &
        vis_src == "local"
    ),
    !(
      ds_cat %in% c(
        "ds_health", "ds_gov", "ds_edu"
      ) &
        vis_src == "tourist"
    )
  ) %>%
  # 作图。
  ggplot(aes(spa_group, ds_cat)) +
  # 设置格子边框
  geom_tile(aes(fill = loc_rate), color = "black") +
  # 关键修改 1：消除坐标轴两侧的空白间隙，让边框贴齐
  scale_x_discrete(expand = c(0.08, 0.08), labels = str_to_title) +
  scale_y_discrete(
    expand = c(0.08, 0.08),
    labels = ds_label
  ) +
  # 设置颜色和NA值。
  scale_fill_gradient(
    low = "yellow", high = "darkred", na.value = "white",
    name = "Location\nPercentage"
  ) +
  labs(x = "Location cluster", y = "Service") +
  theme_bw() +
  # 关键修改 2：调整主题，移除多余的外框冲突
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid = element_blank(),
    # 移除 theme_bw 默认的面板边框，防止双重线
    panel.border = element_blank(),
    # 移除坐标轴线。
    axis.line = element_blank(),
    # 如果有分面标题，去掉其背景边框
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.ticks = element_blank()
  ) +
  facet_grid(
    vis_src ~ season, scale = "free_y",
    labeller = labeller(season = c(
      "1" = "Quater 1", "2" = "Quater 2", "3" = "Quater 3", "4" = "Quater 4"
    ))
  )
dev.off()

# Network index ----
net_index <- read.csv("data_raw/net_index.csv") %>%
  tibble()

# 只分析本地人和外地人的话。
net_index %>%
  pivot_longer(
    cols = -c(vis_src, season), names_to = "index_cat", values_to = "index_val"
  ) %>%
  ggplot() +
  geom_col(aes(season, index_val)) +
  facet_grid(
    index_cat ~ vis_src, scales = "free",
    labeller = labeller(
      vis_src = as_labeller(
        c("local" = "Local", "tourist" = "Tourist", "all_src" = "All visitor")
      ),
      index_cat = as_labeller(
        function(x) {
          x <- gsub("_", "\n", x)
          x <- stringr::str_to_title(x)
          gsub("Avg", "Average", x)
        }
      )
    )
  ) +
  theme_bw() +
  labs(x = "Quarter", y = "Index value")

# Node index ----
# 直方图。
lapply(
  c("indegree", "closeness", "betweeness"),
  function(index_x) {
    combined_data %>%
      filter(vis_src != "local_tourist") %>%
      ggplot() +
      geom_histogram(aes(get(index_x))) +
      facet_grid(vis_src ~ season) +
      theme_bw() +
      labs(x = index_x)
  }
)
# 密度图。
lapply(
  c("indegree", "closeness", "betweeness"),
  function(index_x) {
    combined_data %>%
      filter(vis_src != "local_tourist") %>%
      ggplot() +
      geom_density(aes(get(index_x))) +
      facet_grid(vis_src ~ season) +
      theme_bw() +
      labs(x = index_x)
  }
)

# Abstract subnet ----
# 暂时注释：依赖 modularity_class，需重新从 Gephi 导出 CSV 后启用。
# 根据特定指标，提取各客源各季节点最多的模块中最重要的节点并作图。
# plt_abs_subnet <- function(vis_src_x, index_x) {
  # 首先筛选每个分组特定指标值最大的4个点。
#   top_points <-
#     combined_data %>%
#     filter(vis_src == vis_src_x) %>%
    # 选出节点数最多的module。
    # Bug: 目前仅针对一种指标。
#     filter(top_mod == 1) %>%
#     select(
#       vis_src, season, modularity_class, id, longitude, latitude, all_of(index_x)
#     ) %>%
#     group_by(vis_src, season, modularity_class) %>%
    # Bug: 当出现并列排名的多个点时，强制返回其中一个。
#     slice_max(., order_by = get(index_x), n = 4, with_ties = FALSE) %>%
    # 将4个点按相对位置分配到正方形4个端点：先分左右，再分上下。
#     mutate(x_posi = ifelse(
#       rank(longitude, ties.method = "first") <= 2, "left", "right"
#     )) %>%
#     group_by(vis_src, season, modularity_class, x_posi) %>%
#     mutate(y_posi = ifelse(
#       rank(latitude, ties.method = "first") <= 1, "bottom", "top"
#     )) %>%
#     ungroup() %>%
#     mutate(node_pos = paste0(y_posi, "_", x_posi)) %>%
    # 添加正方形端点坐标。
#     left_join(
#       tibble(
#         node_pos = c("top_left", "top_right", "bottom_left", "bottom_right"),
#         square_x = c(0.1, 0.9, 0.1, 0.9),
#         square_y = c(0.9, 0.9, 0.1, 0.1)
#       ),
#       by = "node_pos"
#     ) %>%
    # 添加季节-组团编号。
#     mutate(season_mod = paste0(season, "-", modularity_class))

  # Bug: 需要还原loc_x_y数据。
#   loc_x_y <- top_points %>%
#     select(vis_src, season, modularity_class, id, square_x, square_y) %>%
#     mutate(id = as.character(id), season = as.character(season)) %>%
#     distinct()
  # 各个节点在不同客源和季节中所属的module。
#   belong_mod <- loc_x_y %>%
#     select(vis_src, season, modularity_class, id) %>%
#     distinct() %>%
#     mutate(season = as.character(season))

#   flow_sub <- od %>%
    # 汇总计算流量。
#     group_by(source, qua, origin, destination) %>%
#     summarise(flow = n(), .groups = "drop")
#   flow_all <- od %>%
    # 汇总计算流量。
#     group_by(qua, origin, destination) %>%
#     summarise(flow = n(), .groups = "drop") %>%
#     mutate(source = "local_tourist")
#   flow <- bind_rows(flow_sub, flow_all) %>%
    # 加入所属module信息。
    # 对起点。
#     left_join(belong_mod, by = c(
#       "source" = "vis_src", "qua" = "season", "origin" = "id"
#     )) %>%
#     rename("origin_mod" = "modularity_class") %>%
#     filter(!is.na(origin_mod)) %>%
    # 对终点。
#     left_join(belong_mod, by = c(
#       "source" = "vis_src", "qua" = "season", "destination" = "id"
#     )) %>%
#     rename("destination_mod" = "modularity_class") %>%
#     filter(!is.na(destination_mod)) %>%
    # 只保留起点和终点属于同一个module的数据。
#     filter(origin_mod == destination_mod) %>%
    # 加入坐标信息。
#     left_join(loc_x_y, by = c(
#       "origin" = "id", "source" = "vis_src", "qua" = "season",
#       "origin_mod" = "modularity_class"
#     )) %>%
#     rename(x_from = square_x, y_from = square_y) %>%
#     left_join(loc_x_y, by = c(
#       "destination" = "id", "source" = "vis_src", "qua" = "season",
#       "destination_mod" = "modularity_class"
#     )) %>%
#     rename(x_to = square_x, y_to = square_y) %>%
    # 去除自己流向自己的部分。
#     filter(!c(x_from == x_to & y_from == y_to))

  # 作图。
#   top_points_plt <- ggplot() +
    # 添加箭头。
#     geom_curve(
      # Bug: 只做本地人。
#       data = flow %>%
#         filter(source == vis_src_x) %>%
#         mutate(season_mod = paste0(qua, "-", origin_mod)),
#       aes(
#         x = x_from, y = y_from,
#         xend = x_to, yend = y_to,
#         linewidth = flow # 箭头粗细代表流量
#       ),
      # 箭头顺时针方向。
#       curvature = -0.16,
#       alpha = 0.5
      # arrow = arrow(
      #   type = "closed",
      #   length = unit(0.1, "cm"),
      #   angle = 20
      # )
#     ) +
    # 定义边界。
#     lims(x = c(-0.5, 1.5), y = c(-0.5, 1.5)) +
    # 绘制节点（大小反映指标1的值）
#     geom_point(data = top_points,
#                aes(x = square_x, y = square_y), fill = "pink",
#                shape = 21, color = "black", stroke = 1, size = 7) +
    # 添加节点标签
#     geom_text(
#       data = top_points, aes(x = square_x, y = square_y, label = id), size = 3
#     ) +
#     facet_wrap(.~ season_mod, nrow = 4) +
#     theme_minimal() +
#     theme(
#       panel.grid = element_blank(),
#       axis.text = element_blank()
#     ) +
#     scale_linewidth_continuous(range = c(0.5, 3)) +
#     labs(x = NULL, y = NULL)

  # 返回结果。
#   return(list(top_points, top_points_plt))
# }
# plt_abs_subnet("local_tourist", "weighted_indegree")

# 对所有客源和网络指标的组合进行作图。
# abs_subnet_comb <-
#   expand.grid(
#     c("local", "tourist", "local_tourist"),
#     c(
#       "indegree", "closness_centrality", "betweeness_centrality"
      # "outdegree", "degree",
      # "weighted_indegree", "weighted_outdegree", "weighted_degree",
      # "eccentricity",
      # "harmonicclosness_centrality"
#     )
#   ) %>%
#   rename_with(~ c("vis_src", "index"))

# map2(
#   abs_subnet_comb$vis_src %>% as.character(),
#   abs_subnet_comb$index %>% as.character(),
#   function(x, y) {
#     png(
#       paste0("data_proc/abs_subnet/", x, "_", y, ".png"),
#       width = 1500, height = 1500, res = 200
#     )
#     plt_abs_subnet(x, y)[[2]] %>% print()
#     dev.off()
#   }
# )

# Results ----
# 计算供需指数时的服务-中心度对应关系。
# 读取数据
library(readxl)
df <- read_excel("data_proc/table_demand_supply_calc.xlsx") %>%
  # 填充Visitor group列
  fill(`Visitor group`, .direction = "down") %>%
  # 转换为长格式
  pivot_longer(
    cols = c(Degree, Closeness, Harmonic),
    names_to = "centrality",
    values_to = "weight"
  ) %>%
  # 设置因子顺序
  mutate(
    centrality = factor(centrality, levels = c("Degree", "Closeness", "Harmonic"))
  )

# 图1: 热力图 (Heatmap)
ggplot(df, aes(x = centrality, y = Service, fill = weight)) +
  geom_tile(color = "white", linewidth = 1) +
  # 添加数值标签
  geom_text(
    aes(label = ifelse(!is.na(weight), sprintf("%.1f", weight), "")),
    col = "white"
  ) +
  facet_wrap(.~ `Visitor group`, ncol = 1, scale = "free_y") +
  # 颜色设置
  scale_fill_gradient(
    low = "#FFF5EB", high = "#8B0000", na.value = "white",
    limits = c(0, 1), name = "Weight"
  ) +
  labs(y = NULL) +
  theme_bw()
# 图2: 圆圈图 (Circle Plot)
ggplot(df %>% filter(!is.na(weight)), aes(x = centrality, y = Service)) +
  geom_point(size = 3) +
  facet_wrap(.~ `Visitor group`, ncol = 1, scale = "free_y") +
  labs(y = NULL) +
  theme_bw()

# Export ----
# 各群体各季节中心度原始数据。
loc %>%
  st_drop_geometry() %>%
  left_join(
    combined_data %>% filter(vis_src %in% c("local", "tourist")),
    by = c("loc_id" = "id")
  ) %>%
  select(
    "loc_id", "spa_group", "vis_src", "season",
    "degree", "closeness", "harmonic"
  ) %>%
  write.csv("data_proc/loc_centrality_raw.csv")

# 各群体各季节中心度按地区汇总数据。
loc %>%
  st_drop_geometry() %>%
  left_join(
    combined_data %>% filter(vis_src %in% c("local", "tourist")),
    by = c("loc_id" = "id")
  ) %>%
  select(
    "loc_id", "spa_group", "vis_src", "season",
    "degree", "closeness", "harmonic"
  ) %>%
  group_by(vis_src, spa_group, season) %>%
  summarise(
    across(
      .cols = c(degree, closeness, harmonic),
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        # 计算四分位数。
        q25 = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
        q50 = ~ quantile(.x, probs = 0.50, na.rm = TRUE), # 中位数
        q75 = ~ quantile(.x, probs = 0.75, na.rm = TRUE)
      ),
      # 定义新列名格式：原始列名_函数名
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  write.csv("data_proc/loc_centrality_summary.csv")

# 可达性原始数据。
loc %>%
  st_drop_geometry() %>%
  left_join(loc_poi_access, by = "loc_id") %>%
  select(loc_id, spa_group, all_of(access_cols)) %>%
  write.csv("data_proc/loc_accessibility_raw.csv")

# 可达性汇总数据。
loc %>%
  st_drop_geometry() %>%
  left_join(loc_poi_access, by = "loc_id") %>%
  select(loc_id, spa_group, all_of(access_cols)) %>%
  group_by(spa_group) %>%
  summarise(
    across(
      .cols = all_of(access_cols),
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        # 计算四分位数。
        q25 = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
        q50 = ~ quantile(.x, probs = 0.50, na.rm = TRUE), # 中位数
        q75 = ~ quantile(.x, probs = 0.75, na.rm = TRUE)
      ),
      # 定义新列名格式：原始列名_函数名
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  write.csv("data_proc/loc_accessibility_summary.csv")

# 供需指数原始数据。
loc_dem_sup %>%
  select(vis_src, id, , spa_group, season, ds_cat, ds_val) %>%
  write.csv("data_proc/loc_supplydemand_raw.csv")

# 供需指数汇总计算。
loc_dem_sup %>%
  select(vis_src, id, , spa_group, season, ds_cat, ds_val) %>%
  group_by(vis_src, spa_group, season, ds_cat) %>%
  summarise(
    across(
      .cols = ds_val,
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        # 计算四分位数。
        q25 = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
        q50 = ~ quantile(.x, probs = 0.50, na.rm = TRUE), # 中位数
        q75 = ~ quantile(.x, probs = 0.75, na.rm = TRUE)
      ),
      # 定义新列名格式：原始列名_函数名
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  write.csv("data_proc/loc_supplydemand_summary.csv")
