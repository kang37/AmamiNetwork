# 加载包 ----
library(dplyr)
library(ggplot2)

# 图3 ----
# 分群体-月份访问daily ID数。
png(
  paste0("temp_fig/p_pop_season_group_", Sys.Date(), ".png"),
  width = 1200, height = 1200, res = 300
)
readRDS("temp_fig/p_pop_season_group.rds")
dev.off()

# 各daily ID访问地点频率。
png(
  paste0("temp_fig/visit_loc_hist_", Sys.Date(), ".png"),
  width = 1500, height = 1500, res = 300
)
readRDS("temp_fig/visit_loc_hist.rds")
dev.off()

# 各季节各地点轨迹点数和其中游客-本地比。
# Bug：是否改成daily ID数，和前面两个图对应？
png(
  paste0("temp_fig/re_tp_num_", Sys.Date(), ".png"),
  width = 1800, height = 1500, res = 300
)
readRDS("temp_fig/re_tp_num.rds")
dev.off()
