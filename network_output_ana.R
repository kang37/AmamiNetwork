# Node index ----
# 加载包。
library(stringr)
library(readxl)

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
  )

# Supply POI ----
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

# Demand and supply ----
loc_dem_sup <- combined_data %>%
  left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
  mutate(
    # 对于本地人。
    local_ds_gov = government / indegree,
    local_ds_edu = education / indegree,
    local_ds_amen = public_amenities / degree,
    local_ds_health = health / degree,
    local_ds_retail = retail / harmonic,
    local_ds_accomfood = ac / indegree,
    # 对于游客。
    tourist_ds_accomfood = ac / indegree,
    tourist_ds_tour = tourism / indegree,
    tourist_ds_amen = public_amenities / degree,
    # Bug: 游客和本地人交通是混合在一起的，因此应该计算总中心度。
    tourist_ds_mobility = mobility / betweeness
  ) %>%
  left_join(loc, by = c("id" = "loc_id")) %>%
  # Bug.
  mutate(across(
    c(local_ds_gov:tourist_ds_mobility),
    ~ ifelse(is.infinite(.x), 1, .x)
  )) %>%
  group_by(vis_src, season) %>%
  mutate(across(
    c(local_ds_gov:tourist_ds_mobility), ~ .x/max(.x, na.rm = TRUE)
  )) %>%
  ungroup() %>%
  st_as_sf()

# 本地人的各项需求。
loc_dem_sup %>%
  st_drop_geometry() %>%
  filter(vis_src == "local") %>%
  select(spa_group, season, contains("local_")) %>%
  pivot_longer(
    cols = c(contains("local_")), names_to = "ds_cat", values_to = "ds_val"
  ) %>%
  ggplot(aes(spa_group, ds_val)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(ds_cat ~ season)
# 对数尺度。
loc_dem_sup %>%
  st_drop_geometry() %>%
  filter(vis_src == "local") %>%
  select(spa_group, season, contains("local_")) %>%
  pivot_longer(
    cols = c(contains("local_")), names_to = "ds_cat", values_to = "ds_val"
  ) %>%
  ggplot(aes(spa_group, log(ds_val))) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(ds_cat ~ season)

ggplot() +
  geom_sf(data = amami, col = "white") +
  geom_sf(
    data = loc_dem_sup %>% filter(vis_src == "local") %>% st_centroid(),
    aes(size = local_ds_gov, col = spa_group), alpha = 0.5
  ) +
  theme_bw() +
  facet_wrap(.~ season, nrow = 1)

# 旅客的各项需求。
loc_dem_sup %>%
  st_drop_geometry() %>%
  filter(vis_src == "tourist") %>%
  select(spa_group, season, contains("tourist_")) %>%
  pivot_longer(
    cols = c(contains("tourist_")), names_to = "ds_cat", values_to = "ds_val"
  ) %>%
  ggplot(aes(spa_group, ds_val)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(ds_cat ~ season)
# 对数尺度。
loc_dem_sup %>%
  st_drop_geometry() %>%
  filter(vis_src == "tourist") %>%
  select(spa_group, season, contains("tourist_")) %>%
  pivot_longer(
    cols = c(contains("tourist_")), names_to = "ds_cat", values_to = "ds_val"
  ) %>%
  ggplot(aes(spa_group, log(ds_val))) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(ds_cat ~ season)

# Network index ----
net_index <- readxl::read_xlsx(
  "data_raw/gephi_output_net.xlsx", sheet = "Sheet3"
) %>%
  pivot_longer(
    cols = paste0("qua_", 1:4), names_to = "qua", values_to = "index_val"
  )

# 只分析本地人和外地人的话。
net_index %>%
  filter(vis_src != "local+tourist") %>%
  ggplot() +
  geom_col(aes(qua, index_val)) +
  facet_grid(
    net_index ~ vis_src, scales = "free",
    labeller = labeller(vis_src = as_labeller(
      c("local" = "Local", "tourist" = "Tourist")
    ),
    net_index = as_labeller(
      c("Average Clustering Coefficient" = "Average\nClustering Coefficient")
    ))
  ) +
  theme_bw() +
  scale_x_discrete(labels = c(
    "qua_1" = "1", "qua_2" = "2", "qua_3" = "3", "qua_4" = "4"
  )) +
  labs(x = "四半期", y = "Index value")

# Node index ----
# 直方图。
lapply(
  c("indegree", "closness_centrality", "betweeness_centrality"),
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
  c("indegree", "closness_centrality", "betweeness_centrality"),
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
# 根据特定指标，提取各客源各季节点最多的模块中最重要的节点并作图。
plt_abs_subnet <- function(vis_src_x, index_x) {
  # 首先筛选每个分组特定指标值最大的4个点。
  top_points <-
    combined_data %>%
    filter(vis_src == vis_src_x) %>%
    # 选出节点数最多的module。
    # Bug: 目前仅针对一种指标。
    filter(top_mod == 1) %>%
    select(
      vis_src, season, modularity_class, id, longitude, latitude, all_of(index_x)
    ) %>%
    group_by(vis_src, season, modularity_class) %>%
    # Bug: 当出现并列排名的多个点时，强制返回其中一个。
    slice_max(., order_by = get(index_x), n = 4, with_ties = FALSE) %>%
    # 将4个点按相对位置分配到正方形4个端点：先分左右，再分上下。
    mutate(x_posi = ifelse(
      rank(longitude, ties.method = "first") <= 2, "left", "right"
    )) %>%
    group_by(vis_src, season, modularity_class, x_posi) %>%
    mutate(y_posi = ifelse(
      rank(latitude, ties.method = "first") <= 1, "bottom", "top"
    )) %>%
    ungroup() %>%
    mutate(node_pos = paste0(y_posi, "_", x_posi)) %>%
    # 添加正方形端点坐标。
    left_join(
      tibble(
        node_pos = c("top_left", "top_right", "bottom_left", "bottom_right"),
        square_x = c(0.1, 0.9, 0.1, 0.9),
        square_y = c(0.9, 0.9, 0.1, 0.1)
      ),
      by = "node_pos"
    ) %>%
    # 添加季节-组团编号。
    mutate(season_mod = paste0(season, "-", modularity_class))

  # Bug: 需要还原loc_x_y数据。
  loc_x_y <- top_points %>%
    select(vis_src, season, modularity_class, id, square_x, square_y) %>%
    mutate(id = as.character(id), season = as.character(season)) %>%
    distinct()
  # 各个节点在不同客源和季节中所属的module。
  belong_mod <- loc_x_y %>%
    select(vis_src, season, modularity_class, id) %>%
    distinct() %>%
    mutate(season = as.character(season))

  flow_sub <- od %>%
    # 汇总计算流量。
    group_by(source, qua, origin, destination) %>%
    summarise(flow = n(), .groups = "drop")
  flow_all <- od %>%
    # 汇总计算流量。
    group_by(qua, origin, destination) %>%
    summarise(flow = n(), .groups = "drop") %>%
    mutate(source = "local_tourist")
  flow <- bind_rows(flow_sub, flow_all) %>%
    # 加入所属module信息。
    # 对起点。
    left_join(belong_mod, by = c(
      "source" = "vis_src", "qua" = "season", "origin" = "id"
    )) %>%
    rename("origin_mod" = "modularity_class") %>%
    filter(!is.na(origin_mod)) %>%
    # 对终点。
    left_join(belong_mod, by = c(
      "source" = "vis_src", "qua" = "season", "destination" = "id"
    )) %>%
    rename("destination_mod" = "modularity_class") %>%
    filter(!is.na(destination_mod)) %>%
    # 只保留起点和终点属于同一个module的数据。
    filter(origin_mod == destination_mod) %>%
    # 加入坐标信息。
    left_join(loc_x_y, by = c(
      "origin" = "id", "source" = "vis_src", "qua" = "season",
      "origin_mod" = "modularity_class"
    )) %>%
    rename(x_from = square_x, y_from = square_y) %>%
    left_join(loc_x_y, by = c(
      "destination" = "id", "source" = "vis_src", "qua" = "season",
      "destination_mod" = "modularity_class"
    )) %>%
    rename(x_to = square_x, y_to = square_y) %>%
    # 去除自己流向自己的部分。
    filter(!c(x_from == x_to & y_from == y_to))

  # 作图。
  top_points_plt <- ggplot() +
    # 添加箭头。
    geom_curve(
      # Bug: 只做本地人。
      data = flow %>%
        filter(source == vis_src_x) %>%
        mutate(season_mod = paste0(qua, "-", origin_mod)),
      aes(
        x = x_from, y = y_from,
        xend = x_to, yend = y_to,
        linewidth = flow # 箭头粗细代表流量
      ),
      # 箭头顺时针方向。
      curvature = -0.16,
      alpha = 0.5
      # arrow = arrow(
      #   type = "closed",
      #   length = unit(0.1, "cm"),
      #   angle = 20
      # )
    ) +
    # 定义边界。
    lims(x = c(-0.5, 1.5), y = c(-0.5, 1.5)) +
    # 绘制节点（大小反映指标1的值）
    geom_point(data = top_points,
               aes(x = square_x, y = square_y), fill = "pink",
               shape = 21, color = "black", stroke = 1, size = 7) +
    # 添加节点标签
    geom_text(
      data = top_points, aes(x = square_x, y = square_y, label = id), size = 3
    ) +
    facet_wrap(.~ season_mod, nrow = 4) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank()
    ) +
    scale_linewidth_continuous(range = c(0.5, 3)) +
    labs(x = NULL, y = NULL)

  # 返回结果。
  return(list(top_points, top_points_plt))
}
# plt_abs_subnet("local_tourist", "weighted_indegree")

# 对所有客源和网络指标的组合进行作图。
abs_subnet_comb <-
  expand.grid(
    c("local", "tourist", "local_tourist"),
    c(
      "indegree", "closness_centrality", "betweeness_centrality"
      # "outdegree", "degree",
      # "weighted_indegree", "weighted_outdegree", "weighted_degree",
      # "eccentricity",
      # "harmonicclosness_centrality"
    )
  ) %>%
  rename_with(~ c("vis_src", "index"))

map2(
  abs_subnet_comb$vis_src %>% as.character(),
  abs_subnet_comb$index %>% as.character(),
  function(x, y) {
    png(
      paste0("data_proc/abs_subnet/", x, "_", y, ".png"),
      width = 1500, height = 1500, res = 200
    )
    plt_abs_subnet(x, y)[[2]] %>% print()
    dev.off()
  }
)
