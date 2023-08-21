library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.

# Set target-specific options such as packages.
tar_option_set(packages = c(
  "jmastats", "lubridate", "openxlsx", "stringr", "dplyr", "tidyr", "ggplot2",
  "geojsonsf", "sf", "tmap", "parallel", "showtext", "patchwork", "jpmesh",
  "mapview", "data.table"
))

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
  # Agoop ----
  # Agoop folder.
  tar_target(
    agoop_folder, "data_raw/23_Agoop_amami_data", format = "file"
  ),
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
  # Holiday ----
  tar_target(
    holiday,
    read.csv("data_raw/Japan_holiday_2018.csv") %>%
      mutate(
        year = 2018, month = substr(.$月日, 1, 2), day  = substr(.$月日, 4, 5)
      ) %>%
      rename(holiday_name = 名称) %>%
      mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
      select(date, holiday_name)
  ),
  # Weather ----
  tar_target(
    # Block: 名瀬 in Amami island.
    weather,
    lapply(1:12, function(x) {
      jma_collect(item = "daily", block_no = 47909, year = 2018, month = x)
    }) %>%
      do.call(rbind, .)
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
  # Kinsakubaru range.
  # Bug: Rough range.
  tar_target(
    kinsakubaru,
    data.frame(
      lon = 129.44814145842295,
      lat = 28.339060364857982
    ) %>%
      st_as_sf(coords = c("lon", "lat")) %>%
      st_set_crs(6668) %>%
      st_buffer(dist = 1000)
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
  # Agoop of the dailyid with the most logs.
  tar_target(
    gis_agoop_head_pre_pre,
    gis_agoop %>%
      filter(dailyid %in% gis_agoop_lognum$dailyid[1:10000])
  ),
  # Function to get the relationship of each point in a multi-points object and a polygon.
  tar_target(
    get_inter_id,
    function(point_x, polygon_x) {
      res <- st_intersects(point_x, polygon_x)
      res[unlist(lapply(res, function(x) length(x) == 0))] <- 0
      res <- unlist(res)
      return(res)
    }
  ),
  tar_target(
    inter_res,
    get_inter_id(gis_agoop_head_pre_pre, kinsakubaru)
  ),
  # Add information column of the intersect relationship between the sample points data and the Kinsakubaru polygon.
  tar_target(
    gis_agoop_head_pre,
    gis_agoop_head_pre_pre %>%
      mutate(inter = inter_res)
  ),
  tar_target(
    gis_agoop_head_inter,
    gis_agoop_head_pre %>%
      group_by(dailyid) %>%
      summarise(inter = sum(inter) > 0) %>%
      ungroup() %>%
      filter(inter)
  ),
  tar_target(
    gis_agoop_head,
    gis_agoop_head_pre %>%
      filter(dailyid %in% gis_agoop_head_inter$dailyid) %>%
      mutate(time = hour * 60 + minute) %>%
      arrange(month, day, dailyid, time)
  )
)





