# 步骤1：筛选每个分组各指标TOP4的点
top_points <-
combined_data %>%
  # 选出节点数最多的module。
  filter(top_mod == 1) %>%
  select(vis_src, season, modularity_class, id, longitude, latitude,
         weighted_outdegree, weighted_indegree) %>%
  pivot_longer(
    cols = c(weighted_outdegree, weighted_indegree),
    names_to = "net_index", values_to = "index_val"
  ) %>%
  group_by(vis_src, season, modularity_class, net_index) %>%
  slice_max(., order_by = index_val, n = 4) %>%
  # 将4个点按相对位置分配到正方形4个端点：先分左右，再分上下。
  mutate(
    x_posi = ifelse(
      rank(longitude, ties.method = "first") <= 2, "left", "right"
    )
  ) %>%
  group_by(vis_src, season, modularity_class, net_index, x_posi) %>%
  mutate(
    y_posi = ifelse(
      rank(latitude, ties.method = "first") <= 1, "bottom", "top"
    )
  ) %>%
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
  )



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

flow <- od %>%
  group_by(source, qua, origin, destination) %>%
  summarise(flow = n(), .groups = "drop") %>%
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
  # 标记流向颜色组（A→B为红，B→A为蓝）
  mutate(
    flow_group = ifelse(origin < destination, "A→B", "B→A")
  )

# 步骤2：生成弧形箭头数据（修复关键错误）
generate_curves <- function(points) {
  if(nrow(points) < 2) return(NULL)

  # 确保points是数据框且包含所需列
  required_cols <- c("id", "square_x", "square_y", "x", "y", "flow")
  if(!all(required_cols %in% names(points))) {
    stop("Missing required columns in points data")
  }

  combos <- combn(points$id, 2, simplify = FALSE)
  map_dfr(combos, function(pair) {
    from <- points %>% filter(id == pair[1])
    to <- points %>% filter(id == pair[2])

    # 添加防御性编程
    if(nrow(from) == 0 || nrow(to) == 0) return(NULL)

    x_dir = sign(to$x - from$x)
    y_dir = sign(to$y - from$y)

    curvature = case_when(
      x_dir > 0 & y_dir > 0 ~ 0.3,
      x_dir > 0 & y_dir < 0 ~ -0.3,
      x_dir < 0 & y_dir > 0 ~ -0.3,
      x_dir < 0 & y_dir < 0 ~ 0.3,
      x_dir == 0 ~ 0.5 * sign(y_dir),
      y_dir == 0 ~ 0.5 * sign(x_dir)
    )

    bind_rows(
      tibble(
        vis_src = unique(points$vis_src),
        from = from$id,
        to = to$id,
        from_x = from$square_x,
        from_y = from$square_y,
        to_x = to$square_x,
        to_y = to$square_y,
        curvature = curvature,
        flow = from$flow
      ),
      tibble(
        vis_src = unique(points$vis_src),
        from = to$id,
        to = from$id,
        from_x = to$square_x,
        from_y = to$square_y,
        to_x = from$square_x,
        to_y = from$square_y,
        curvature = -curvature,
        flow = to$flow
      )
    )
  })
}

# 继续加工点数据。
top_points_plt <- top_points %>%
  mutate(season_mod = paste0(season, "-", modularity_class)) %>%
  filter(net_index == "weighted_outdegree", vis_src == "local")

# 作图。
ggplot() +
  # 添加箭头。
  geom_segment(
    data = flow %>% mutate(season_mod = paste0(qua, "-", origin_mod)),
    aes(
      x = x_from, y = y_from,
      xend = x_to, yend = y_to,
      linewidth = flow,  # 箭头粗细代表流量
      color = flow_group  # 颜色区分流向
    ), alpha = 0.5,
    arrow = arrow(
      type = "closed",
      length = unit(0.1, "cm"),
      angle = 20
    )
  ) +
  # 绘制正方形边框
  geom_rect(xmin = -0.2, xmax = 1.2, ymin = -0.2, ymax = 1.2,
            fill = NA, color = "gray80") +
  lims(x = c(-0.5, 1.5), y = c(-0.5, 1.5)) +
  # 绘制节点（大小反映指标1的值）
  geom_point(data = top_points_plt,
             aes(x = square_x, y = square_y), fill = "pink",
             shape = 21, color = "black", stroke = 1, size = 5) +
  # 添加节点标签
  geom_text(data = top_points_plt, aes(x = square_x, y = square_y, label = id), size = 3) +
  facet_wrap(.~ season_mod) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  ) +
  scale_linewidth_continuous(range = c(0.5, 5))
