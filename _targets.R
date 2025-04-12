library(targets)

# 加载程序包。
tar_option_set(packages = c(
  "jmastats", "lubridate", "openxlsx", "stringr", "dplyr", "tidyr", "ggplot2",
  "geojsonsf", "sf", "tmap", "parallel", "showtext", "patchwork", "jpmesh",
  "mapview", "data.table", "purrr"
))

# 构建变量。
list(
  # Pref and cities ----
  # Prefcode and city code.
  tar_target(
    pref_city_code,
    read.csv("data_raw/prefcode_citycode_master_UTF-8.csv") %>%
      tibble() %>%
      select(prefcode, prefname, citycode, cityname) %>%
      distinct()
  ),
  # City code in Amami.
  tar_target(
    tar_city_code,
    data.frame(city_in_amami = c(
      "奄美市", "大和村", "宇検村", "瀬戸内町", "龍郷町"
    )) %>%
      left_join(pref_city_code, by = c("city_in_amami" = "cityname")) %>%
      pull(citycode)
  ),
  # Amami boundary.
  tar_target(
    amami,
    st_read(
      dsn = "data_raw/KagoshimaAdmin/N03-180101_46_GML",
      layer = "N03-18_46_180101"
    ) %>%
      rename(citycode = N03_007) %>%
      # cities (villages) in Amamioshima island
      filter(citycode %in% tar_city_code) %>%
      st_union() %>%
      st_sf()
  ),
  # 定义目标地点。
  tar_target(
    # Bug: Need to determine minPts and eps first, manually. If k is larger, the calc is slower. The following plot takes 2 min.
    # 各个轨迹点落在哪个地点内。
    # 谢于松地点定义文件。
    # 漏洞：ID列编号不连续；坐标是什么。
    loc,
    st_read(
      dsn = "data_raw/loc_def", layer = "大区域与勾画的进行重叠和叠加"
    ) %>%
      st_set_crs(4326) %>%
      select(loc_id = OBJECTID) %>%
      # 计算每个定义地点的面积，单位为平方米。
      mutate(
        loc_id = as.character(loc_id), loc_area = st_area(.) %>% as.numeric()
      )
  ),
  # Agoop ----
  # Get all file names.
  tar_target(
    agoop_file,
    list.files(
      "data_raw/23_Agoop_amami_data", recursive = TRUE, full.names = TRUE
    ) %>%
      grep("PDP", x = ., value = TRUE)
  ),
  # 并行计算准备。
  # 获取Agoop原始数据。
  tar_target(
    agoop_raw,
    # 读取Agoop数据。
    lapply(agoop_file, fread) %>%
      bind_rows() %>%
      filter(accuracy <= 100) %>%
      select(
        dailyid, year, month, day, dayofweek, hour, minute,
        latitude, longitude, home_prefcode, home_citycode, accuracy, gender
      )
  ),
  # 计算原始数据的数据量。
  tar_target(
    agoop_raw_dt_size,
    nrow(agoop_raw)
  ),
  tar_target(
    agoop_amami,
    agoop_raw %>%
      # 删除奄美大岛及附近岛屿之外的点。
      filter(
        latitude > st_bbox(amami)["ymin"], latitude < st_bbox(amami)["ymax"],
        longitude > st_bbox(amami)["xmin"], longitude < st_bbox(amami)["xmax"]
      ) %>%
      # Conclusion: most dailyid start at 0 and end at 24. Can only keep the dailyid with min start time <= 22 and max end time >= 4.
      # Keep trajectory points between 4 and 22 everyday.
      filter(hour <= 22, hour >= 4) %>%
      # Make date time.
      mutate(time = as_datetime(
        paste(
          paste(year, month, day, sep = "-"),
          paste(hour, minute, "00", sep = "-"),
          sep = " "
        )
      )) %>%
      # If 2 points at the same time, keep only one with higher accuracy.
      arrange(dailyid, time, accuracy) %>%
      group_by(dailyid, time) %>%
      mutate(position_conflict_id = row_number()) %>%
      ungroup() %>%
      filter(position_conflict_id == 1) %>%
      select(-position_conflict_id) %>%
      # 删除活动时间范围较小的dailyID：记录分布少于8个小时的删除。
      group_by(dailyid) %>%
      mutate(n_hour = length(unique(hour))) %>%
      ungroup() %>%
      filter(n_hour > 8) %>%
      select(-n_hour) %>%
      # 根据时间进行重采样，每个人每10分钟仅保留时间最早的一个数据点。
      mutate(minute_step = substr(sprintf("%02i", minute), 1, 1)) %>%
      arrange(dailyid, time) %>%
      group_by(dailyid, hour, minute_step) %>%
      mutate(minute_step_filt = row_number()) %>%
      ungroup() %>%
      filter(minute_step_filt == 1) %>%
      select(-minute_step_filt, -minute_step) %>%
      # 漏洞：排序的时候应以dailyid为优先，否则dailyid会被分散到不相邻的行中。
      arrange(month, day, dailyid, time) %>%
      # 增加客源和季度信息。
      mutate(
        source = case_when(
          home_citycode %in% tar_city_code ~ "local", TRUE ~ "tourist"
        ),
        qua = case_when(
          month <= 3 ~ "1", month <= 6 ~ "2",
          month <= 9 ~ "3", month <= 12 ~ "4"
        )
      ) %>%
      # 增加记录点编号。
      arrange(time, dailyid) %>%
      mutate(res_id = row_number()) %>%
      # 加入来源县市名称。
      left_join(
        pref_city_code,
        by = c("home_prefcode" = "prefcode", "home_citycode" = "citycode")
      ) %>%
      rename("home_prefname" = "prefname", "home_cityname" = "cityname") %>%
      # 转化成sf数据。
      st_as_sf(
        coords = c("longitude", "latitude"), crs = 4326, agr = "constant"
      ) %>%
      # 判断各个轨迹点所属地点。
      mutate(
        loc_id = loc$loc_id[as.numeric(st_intersects(., loc))]
        loc_id = case_when(is.na(loc_id) ~ "r", TRUE ~ loc_id)
      ) %>%
      # 加入对应地点面积。
      left_join(loc, by = "loc_id")
  )
)
