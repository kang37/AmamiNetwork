# make a new tour_mesh
tour_mesh <- meshcode_sf(
  data.frame(
    meshcode = meshcode(c(422933124,
                          422933133, 422933134,
                          422933143,
                          422933241,  422933242,  422933244,
                          422933342, 422933344,
                          422933442, 422933444))
  ),
  meshcode
) %>%
  st_transform(my_crs)
# bug: assume the cars go in this order
tour_mesh$order <- 1:nrow(tour_mesh)
tm_shape(tour_mesh) +
  tm_polygons() +
  tm_text(text = "order")
mapView(road_buff) +
  mapview(agoop_buff, cex = 0.5) + mapview(tour_mesh)

# take one as an example
# agoop_buff_eg <- agoop_buff %>%
#   filter(dailyid == agoop_buff$dailyid[30])
agoop_buff_eg <- agoop_buff %>%
  filter(dailyid == agoop_buff$dailyid[60])
dim(agoop_buff_eg)
mapView(road_buff) +
  mapview(agoop_buff_eg, cex = 0.5) +
  mapview(tour_mesh)
agoop_buff_eg$hour
agoop_buff_eg$minute

# make new time-related mesh to the example dailyid
tour_mesh_eg <- tour_mesh
agoop_buff_eg_time <- agoop_buff_eg %>%
  mutate(time = hour * 60 + minute) %>%
  select(dailyid, date, time)
agoop_buff_eg_time

tour_mesh_eg <- st_join(tour_mesh_eg, agoop_buff_eg_time) %>%
  group_by(dailyid, meshcode, geometry, order) %>%
  # use mean time when there are multiple points in one mesh
  summarise(time = mean(time)) %>%
  arrange(order)
tour_mesh_eg$dailyid <- agoop_buff$dailyid[60]
mapview(tour_mesh_eg) +
  mapview(agoop_buff_eg_time, cex = 0.1)

# how about just focus on the mesh with most arriving time logs (inferred logs included)?
agoop_buff %>%
  # bug: take a sample
  select(dailyid, year, month, day, hour, minute) %>%
  head(50) %>%
  st_join(tour_mesh) %>%
  group_by(dailyid, month, day, order) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(have_log = ifelse(n > 0, 1, 0)) %>%
  group_by(order) %>%
  summarise(num = n()) %>%
  ggplot() +
  geom_col(aes(as.factor(order), num))

agoop_buff %>%
  # bug: take a sample
  select(dailyid, year, month, day, hour, minute) %>%
  st_join(tour_mesh) %>%
  group_by(dailyid, order) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(have_log = ifelse(n > 0, 1, 0)) %>%
  ggplot() +
  geom_tile(aes(x = as.factor(order), y = dailyid, fill = have_log)) +
  theme(axis.text.y = element_blank())

agoop_buff_tar <- agoop_buff %>%
  # bug: take a sample
  select(dailyid, year, month, day, hour, minute) %>%
  st_join(tour_mesh) %>%
  group_by(dailyid, order) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(have_log = ifelse(n > 0, 1, 0)) %>%
  # bug: why there are NAs?
  filter(!is.na(order)) %>%
  # only keep the min and max order of each dailyid
  group_by(dailyid) %>%
  summarise(order_min = min(order), order_max = max(order)) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  select(dailyid, order_min, order_max) %>%
  pivot_longer(cols = c(order_min, order_max), names_to = "order_attr", values_to = "order_val")

# 对于每个dailyid，填充最小值和最大值之间的序号
agoop_buff_tar <-
  lapply(unique(agoop_buff_tar$dailyid),
         function(x) {
           x <- agoop_buff_tar %>% filter(dailyid == x)
           x_fill <- data.frame(
             dailyid = unique(x$dailyid),
             order = min(x$order_val) : max(x$order_val)
           )
         })
agoop_buff_tar <- do.call(rbind, agoop_buff_tar)

ggplot(agoop_buff_tar) +
  geom_tile(aes(x = as.factor(order), y = dailyid, fill = 1)) +
  theme(axis.text.y = element_blank())
agoop_buff_tar %>%
  group_by(order) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(as.factor(order), n))

# focus on mesh of order 5, 6, 7, and 8 since they have most log data (including infered ones)
# bug: take mesh order 5 as an example
tar_dailyid <- agoop_buff_tar %>%
  filter(order == 5) %>%
  pull(dailyid) %>%
  unique
agoop_buff_timelag <-
  agoop_buff %>%
  filter(dailyid %in% tar_dailyid) %>%
  # bug: only use the first time value when a dailyid enters a mesh
  select(dailyid, year, month, day, hour, minute) %>%
  st_join(tour_mesh) %>%
  filter(!is.na(order)) %>%
  mutate(time = 60 * hour + minute) %>%
  # 保留每个dailyid进入一个mesh的第一个记录
  arrange(dailyid, order, year, month, day, time) %>%
  group_by(dailyid, order) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%
  st_drop_geometry() %>%
  # 重构time：把月和日都加进去，因为可能存在跨夜晚12点的情况
  mutate(datetime = as_datetime(paste0(year, "-", month, "-", day, " ", hour, ":", minute, ":00")))
# 进行插值，补全中间缺失的时间
# function: interpolate the time staying in a mesh linearly
# input: a data.frame indicating the stay-time of a dailyid in each mesh along the trip road. The data.frame should include the columns: dailyid, date, order, time, geometry.
# bug: dailyid column is not complete
# bug: if the location of time 2 is futher than the location of time 1 due to higher uncertainty, that causes problem
# bug: should change the comments
line_interp <- function(tar_dailyid) {
  x <- agoop_buff_timelag %>%
    filter(dailyid == tar_dailyid) %>%
    arrange(order) %>%
    select(dailyid, order, datetime)

  if(nrow(x) != 1) {
    know_val_order <- x$order
    # 用于存储结果的列表
    res <- vector("list", length = (length(know_val_order) - 1))
    # 先将原始数据分成几段
    for (i in 1 : (length(know_val_order)-1)) {
      res_know <- x %>% filter(order >= know_val_order[i] & order <= know_val_order[i+1])
      diff <- (res_know$datetime[1] - res_know$datetime[2]) /
        (res_know$order[1] - res_know$order[2])
      res[[i]] <- data.frame(
        dailyid = unique(res_know$dailyid),
        order = res_know$order[1] : res_know$order[2]
      ) %>%
        left_join(res_know)
      res[[i]]$diff <- diff
      res[[i]]$num <- 0 : (nrow(res[[i]]) - 1)
      res[[i]]$start_datetime <- res_know$datetime[1]
      res[[i]]$datetime <- res[[i]]$start_datetime + res[[i]]$diff * res[[i]]$num
    }
    # combine the results
    res <- do.call(rbind, res) %>%
      select(dailyid, order, datetime)
  } else {
    res <- x %>% select(order, datetime, dailyid)
  }
  return(res)
}
# bug: the result of the function is the time when the target dailyid arrives each mesh - the time can exceed the night time - e.g., someone might stay there from 14:00 to 1:00 (am) according to the results
agoop_buff_timelag_res <-
  lapply(unique(agoop_buff_timelag$dailyid), line_interp)

# test
# lapply(agoop_buff_timelag_res, function(x) plot(x$datetime))

agoop_buff_timelag_res <- do.call(rbind, agoop_buff_timelag_res)
agoop_buff_timelag_res <- agoop_buff_timelag_res %>%
  distinct() %>%
  filter(order == 5) %>%
  arrange(order, datetime)
View(agoop_buff_timelag_res)
agoop_buff_timelag_res$datetime_pre <- lag(agoop_buff_timelag_res$datetime)
agoop_buff_timelag_res <- agoop_buff_timelag_res %>%
  mutate(time_diff = datetime - datetime_pre) %>%
  # 如果超过几个小时就没必要统计了
  # bug: 这个时长需要根据研究目的调整，目前只是随便定一个
  filter(time_diff < 10*60*60)
