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
  # 使用正则表达式提取季节和群体。
  matches <- str_match(file_name, "(\\d+)_(\\w+)")
  # 返回解析结果。
  tibble(
    season = as.integer(matches[1, 2]),
    group = matches[1, 3],
    file_path = file_path
  )
}

# 解析所有文件信息。
file_info <- map_dfr(file_paths, parse_file_info)

# 读取并合并所有CSV文件。
combined_data <- pmap(
  list(file_info$file_path, file_info$group, file_info$season),
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

# 统计各季节各客源各module的节点数比例。
ref_module_node_prop <- combined_data %>%
  group_by(vis_src, season, modularity_class) %>%
  summarise(n_node = n(), .groups = "drop") %>%
  group_by(vis_src, season) %>%
  mutate(prop_node = n_node / sum(n_node) * 100) %>%
  ungroup() %>%
  arrange(vis_src, season, -n_node)

# 输出数据。
write.csv(ref_module_node_prop, "data_raw/ref_module_node_prop.csv")

# 筛选前6个最重要的module。
combined_data <- combined_data %>%
  left_join(
    ref_module_node_prop %>%
      group_by(vis_src, season) %>%
      slice_head(n = 6) %>%
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
