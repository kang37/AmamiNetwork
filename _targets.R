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
  # Bug: Default CRS for the project: JGD2011.
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
    city_code_in_amami,
    data.frame(
      city_in_amami = c(
        "奄美市", "大和村", "宇検村", "瀬戸内町", "龍郷町", "喜界町",
        "徳之島町", "天城町", "伊仙町", "和泊町", "知名町", "与論町"
      )
    ) %>%
      left_join(pref_city_code, by = c("city_in_amami" = "cityname")) %>%
      pull(citycode)
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
  # Get all Agoop *.csv data.
  tar_target(
    raw_agoop,
    lapply(agoop_file, fread) %>%
      bind_rows() %>%
      left_join(pref_city_code)
  ),
  # Turn into GIS data.
  tar_target(
    gis_agoop,
    # Turn raw data into simple feature for GIS analysis.
    st_as_sf(raw_agoop, coords = c("longitude", "latitude")) %>%
      # Add projection.
      st_set_crs(my_crs)
  ),
  # GIS layer ----
  # Amami boundary
  tar_target(
    amami,
    st_read(dsn = "data_raw/KagoshimaAdmin/N03-180101_46_GML",
            layer = "N03-18_46_180101") %>%
      rename(citycode = N03_007) %>%
      # cities (villiges) in Amamioshima island
      filter(citycode %in% c(46222, 46527, 46523, 46524, 46525)) %>%
      st_union() %>%
      st_sf()
  ),
  # Analysis ----
  # Monthly change of dailyid number of original data.
  # Bug: takes too long.
  tar_target(
    plt_agoop_raw,
    gis_agoop %>%
      st_drop_geometry() %>%
      select(dailyid, month) %>%
      distinct() %>%
      group_by(month) %>%
      summarise(num = n()) %>%
      ungroup() %>%
      ggplot() +
      geom_col(aes(month, num))
  ),
  # Keep the dailyid with log inside the Kinsakubaru.
  # Bug: Take an example.
  # The dailyid with most logs.
  tar_target(
    gis_agoop_lognum,
    gis_agoop %>%
      st_drop_geometry() %>%
      group_by(dailyid) %>%
      summarise(n_log = n()) %>%
      ungroup() %>%
      arrange(-n_log)
  ),
  tar_target(
    gis_agoop_coord_pre,
    gis_agoop %>%
      # Bug: Need to eliminate local residents for the raw data?
      filter(!home_citycode %in% city_code_in_amami) %>%
      st_transform(4326)
  ),
  tar_target(
    gis_agoop_coord,
    cbind(
      st_drop_geometry(gis_agoop_coord_pre),
      st_coordinates(gis_agoop_coord_pre) %>%
        matrix(ncol = 2) %>%
        data.frame() %>%
        tibble() %>%
        rename_with(~ c("lon", "lat"))
    ) %>%
      tibble() %>%
      # Bug: Eliminate logs out of the island. Should have done that for the raw data.
      filter(lat > 28.10, lat < 28.55, lon > 129.13, lon < 129.73)
  ),
  tar_target(
    gis_agoop_coord_filt,
    gis_agoop_coord %>%
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
      # 删除活动时间范围较小的dailyID：记录分布少于3个小时的删除。
      group_by(dailyid) %>%
      mutate(n_hour = length(unique(hour))) %>%
      ungroup() %>%
      filter(n_hour > 3) %>%
      select(-n_hour)
  ),
  tar_target(
    # 定义地点范围。
    # Bug: Need to determine minPts and eps first, manually. If k is larger, the calc is slower. The following plot takes 2 min.
    # 各个轨迹点落在哪个地点内。
    # 谢于松地点定义文件。
    # 漏洞：ID列编号不连续；坐标是什么。
    loc,
    st_read(dsn = "data_raw/loc_def", layer = "大区域与勾画的进行重叠和叠加") %>%
      st_set_crs(4326) %>%
      select(loc_id = OBJECTID)
  ),
  # 根据时间进行重采样，每个人每10分钟仅保留时间最早的一个数据点。
  tar_target(
    gis_agoop_coord_time,
    gis_agoop_coord_filt %>%
      mutate(minute_step = substr(sprintf("%02i", minute), 1, 1)) %>%
      arrange(dailyid, time) %>%
      group_by(dailyid, hour, minute_step) %>%
      mutate(minute_step_filt = row_number()) %>%
      ungroup() %>%
      filter(minute_step_filt == 1) %>%
      select(-minute_step_filt) %>%
      # 漏洞：可能可以早点删除？
      group_by(dailyid) %>%
      mutate(n_hour = length(unique(hour))) %>%
      ungroup() %>%
      filter(n_hour > 10) %>%
      select(-n_hour)
  ),
  # Re-sampling.
  # 根据每个月的人数进行采样，首先确定每个月的人数。
  # 每个月取多少DailyID进行分析。
  tar_target(
    smp_num_dailyid,
    gis_agoop_coord_time %>%
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
      smp_num_dailyid$month,
      smp_num_dailyid$smp_dailyid,
      function(x, y) {
        gis_agoop_coord_time %>%
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
    agoop_smp,
    gis_agoop_coord_time %>%
      left_join(smp_dailyid, by = c("month", "dailyid")) %>%
      filter(smp)
  ),
  tar_target(
    # 漏洞：要花1小时。
    agoop_filt,
    agoop_smp %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
      st_intersection(loc) %>%
      # 漏洞：应早点重命名。
      rename("cluster" = "loc_id")
  )
)
