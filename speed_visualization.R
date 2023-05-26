# hot map of speed
# speed is opposite to time lag

# focus on mesh of order 5, 6, 7, and 8 since they have most log data (including infered ones)
# bug: take mesh order 5 as an example
tar_dailyid <- agoop_buff_tar %>%
  pull(dailyid) %>%
  unique()
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
agoop_buff_timelag_res <-
  lapply(unique(agoop_buff_timelag$dailyid), line_interp)

# test
# lapply(agoop_buff_timelag_res, function(x) plot(x$datetime))

agoop_buff_timelag_res <- do.call(rbind, agoop_buff_timelag_res)
agoop_buff_timelag_res <- agoop_buff_timelag_res %>%
  distinct() %>%
  arrange(order, datetime)
View(agoop_buff_timelag_res)


agoop_buff_timelag_res$datetime_pre <- lag(agoop_buff_timelag_res$datetime)
agoop_buff_timelag_res <- agoop_buff_timelag_res %>%
  mutate(time_diff = datetime - datetime_pre) %>%
  # 如果超过几个小时就没必要统计了
  # bug: 这个时长需要根据研究目的调整，目前只是随便定一个
  filter(time_diff < 10*60*60)


