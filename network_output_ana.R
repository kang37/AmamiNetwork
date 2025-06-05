# Node index ----
# 加载包。
library(stringr)

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
    ~ gsub("([a-z])(centrality)", "\\1_\\2", .x),
    matches("centrality$")
  )

# 输出数据。
write.csv(ref_module_node_prop, "data_raw/ref_module_node_prop.csv")

# 筛选前3个最重要的module。
combined_data <- combined_data %>%
  left_join(
    ref_module_node_prop %>%
      group_by(vis_src, season) %>%
      slice_head(n = 3) %>%
      mutate(top_mod = 1),
    by = c("vis_src", "season", "modularity_class")
  )

# 作图。
lapply(
  c("indegree", "outdegree", "degree",
    "weighted_indegree", "weighted_outdegree", "weighted_degree",
    "eccentricity", "closness_centrality", "harmonicclosness_centrality",
    "betweeness_centrality", "pageranks", "clustering", "eigen_centrality"),
  function(z) {
    ggplot(combined_data %>% filter(top_mod == 1)) +
      geom_boxplot(aes(as.character(modularity_class), get(z))) +
      facet_grid(season ~ vis_src, scales = "free_y") +
      theme_bw() +
      labs(x = "Modularity class", y = z)
  }
)

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
