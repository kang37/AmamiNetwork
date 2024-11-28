library(targets)

# Set target-specific options such as packages.
tar_option_set(packages = c(
  "jmastats", "lubridate", "openxlsx", "stringr", "dplyr", "tidyr", "ggplot2",
  "geojsonsf", "sf", "tmap", "parallel", "showtext", "patchwork", "jpmesh",
  "mapview", "data.table", "purrr"
))

# Ensure sample to be consistent.
set.seed(1234)
# End this file with a list of target objects.
list(
  # Constant ----
  # Default CRS for the project: JGD2011.
  # By the way, EPSG for JGD2000 is 4612.
  tar_target(
    my_crs, 6668
  ),
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
  # Amami boundary
  tar_target(
    amami,
    st_read(dsn = "data_raw/KagoshimaAdmin/N03-180101_46_GML",
            layer = "N03-18_46_180101") %>%
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
    st_read(dsn = "data_raw/loc_def", layer = "大区域与勾画的进行重叠和叠加") %>%
      st_set_crs(4326) %>%
      select(loc_id = OBJECTID)
  ),
  # Agoop ----
  # Get all file names.
  tar_target(
    agoop_file,
    list.files(
      "data_raw/23_Agoop_amami_data", recursive = TRUE, full.names = TRUE
    ) %>%
      grep("PDP", x = ., value = TRUE) %>%
      .[!grepl("zip", x = .)]
  ),
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
      ) %>%
      left_join(
        pref_city_code,
        by = c("home_prefcode" = "prefcode", "home_citycode" = "citycode")
      ) %>%
      rename("home_prefname" = "prefname", "home_cityname" = "cityname")
  ),
  # 计算原始数据的数据量。
  tar_target(
    agoop_raw_dt_size,
    nrow(agoop_raw)
  ),
  tar_target(
    agoop_amami_pre,
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
      select(-minute_step_filt, -minute_step)
  ),
  # Re-sampling.
  # 根据每个月的人数进行采样，首先确定每个月的人数。
  # 每个月取多少DailyID进行分析。
  tar_target(
    smp_dailyid_num,
    agoop_amami_pre %>%
      select(month, dailyid) %>%
      distinct() %>%
      group_by(month) %>%
      summarise(n_dailyid = n(), .groups = "drop") %>%
      mutate(smp_dailyid = round(n_dailyid / sum(n_dailyid) * 1000))
  ),
  # 随机取所需数量的DailyID。
  tar_target(
    smp_dailyid,
    map2(
      smp_dailyid_num$month,
      smp_dailyid_num$smp_dailyid,
      function(x, y) {
        agoop_amami_pre %>%
          select(month, dailyid) %>%
          distinct() %>%
          filter(month == x) %>%
          slice_sample(n = y)
      }
    ) %>%
      bind_rows() %>%
      mutate(smp = TRUE)
  ),
  # 从原始数据中取样。
  tar_target(
    agoop_amami,
    agoop_amami_pre %>%
      left_join(smp_dailyid, by = c("month", "dailyid")) %>%
      filter(smp) %>%
      # 转化成sf数据。
      st_as_sf(
        coords = c("longitude", "latitude"), crs = 4326, agr = "constant"
      ) %>%
      # 判断每个轨迹点是否在定义地点中。
      st_intersection(st_geometry(loc))
  )
)
