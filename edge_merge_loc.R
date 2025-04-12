# 鉴定谢于松定义地点多边形的相邻和重叠情况，并且将重叠和相邻的多边形合并。
library(sf)
library(igraph)

# 构建邻接矩阵（touch 或 intersect 都可以）。
adj_mat <- st_intersects(loc)

# 将邻接关系转为组编号。
g <- graph.adjlist(adj_mat)
groups <- components(g)$membership

# 加入组号字段。
loc$loc_id <- groups

# 按照loc_id合并多边形。
poly_merged <- loc %>%
  group_by(loc_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# 输出。
write_sf(poly_merged, "data_raw/loc_def/loc_def.shp")
