# 加载包。
library(factoextra)
library(ggalluvial)
library(cowplot)

# 函数：对数据进行聚类。
k_cluster <- function(vis_src_x, season_x, var_x) {
  # 筛选子数据集。
  df <- combined_data %>%
    filter(vis_src == vis_src_x, season == season_x) %>%
    select(all_of(c("id", var_x)))
  # 标准化以免特定变量影响整体聚类结果。
  df_scaled <- scale(df[var_x])

  # 进行聚类。
  # 判断最佳聚类数量。
  # 肘部法则。
  plt_wss <- fviz_nbclust(df_scaled, kmeans, method = "wss")
  # 轮廓系数。
  plt_silhouette <- fviz_nbclust(df_scaled, kmeans, method = "silhouette")
  # 设定随机数，保证可重复。
  set.seed(123)
  # Bug: 假设分3类。
  km_res <- kmeans(df_scaled, centers = 3)
  df$cluster <- factor(km_res$cluster)
  plt_cluster <-
    fviz_cluster(km_res, data = df_scaled, geom = "point", alpha = 0.5) +
    labs(title = paste(vis_src_x, season_x, sep = "-")) +
    theme_bw() +
    theme(legend.position = "none")

  # 桑基图。
  plt_sankey <- ggplot(
    df %>%
      left_join(loc, by = c("id" = "loc_id")) %>%
      count(spa_group, cluster),
    aes(axis1 = spa_group, axis2 = cluster, y = n)
  ) +
    geom_alluvium(aes(fill = cluster), width = 0.3, alpha = 0.9) +
    geom_stratum(aes(fill = after_stat(stratum)), width = 0.3, color = "white") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
    scale_fill_brewer(type = "qual", palette = "Set3") +
    labs(y = "") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      legend.position = "none"
    )

  # 返回结果。
  res <- list(plt_wss, plt_silhouette, plt_cluster, df, plt_sankey) %>%
    setNames(
      c("plt_wss", "plt_silhouette", "plt_cluster", "res_df", "plt_sankey")
    )
  return(res)
}

# 对每个客源每个季节执行一次聚类。
cluster_res <- map2(
  rep(c("local", "tourist"), each = 4),
  rep(1:4, 2),
  k_cluster,
  var_x = c(
    "indegree", "outdegree", "degree", "closeness", "harmonic", "betweeness"
  )
)

# 打印所有聚类图。
plot_grid(
  plotlist = lapply(cluster_res, function(x) x[["plt_cluster"]]), nrow = 2
)
# 打印所有桑基图。
plot_grid(
  plotlist = lapply(cluster_res, function(x) x[["plt_sankey"]]), nrow = 2
)

# 函数：基于数据画出雷达图。
plt_radar <- function(res_df_x) {
  lapply(
    c(1:3),
    function(x) {
      ggradar(
        res_df_x %>%
          filter(cluster == x) %>%
          mutate(across(
            c(
              "indegree", "outdegree", "degree",
              "closeness", "harmonic", "betweeness"
            ),
            function(y) (y - min(y)) / (max(y) - min(y))
          )) %>%
          select(-cluster),
        group.colours = "red", fill.alpha = 0.05, fill = T,
        values.radar = "",
        legend.position = "bottom",
        group.line.width = 0.1,
        group.point.size = 0.1,
        axis.label.size = 2, grid.label.size = 2
      ) +
        theme(legend.position = "none", title = element_text(size = 3)) +
        labs(title = x)
    }
  )
}

# 本地人其中3季度雷达图。
lapply(
  c(1, 2, 4),
  function(x) plt_radar(cluster_res[[x]]$res_df)
) %>%
  Reduce(c, .) %>%
  plot_grid(plotlist = ., nrow = 3)

# 游客其中3季度雷达图。
lapply(
  c(5, 6, 8),
  function(x) plt_radar(cluster_res[[x]]$res_df)
) %>%
  Reduce(c, .) %>%
  plot_grid(plotlist = ., nrow = 3)

