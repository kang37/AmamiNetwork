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
      do.call(rbind, .) %>%
      unnest(cols = c(
        pressure, precipitation, temperature, humidity, wind, sunshine,
        snow, weather_time
      )) %>%
      rename_with(~ gsub(")", "", .x)) %>%
      rename_with(~ gsub("\\(", "_", .x))
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
      select(dailyid, month) %>%
      distinct() %>%
      group_by(month) %>%
      summarise(num = n()) %>%
      ungroup() %>%
      ggplot() +
      geom_col(aes(month, num))
  ),
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
  # Boundary of target area.
  tar_target(
    kinsakubaru_coord,
    st_coordinates(kinsakubaru) %>%
      as.data.frame() %>%
      tibble() %>%
      select(X, Y) %>%
      rename_with(~ c("long", "lat"))
  ),
  tar_target(
    kinsakubaru_boundary,
    data.frame(
      long_min = min(kinsakubaru_coord$long),
      long_max = max(kinsakubaru_coord$long),
      lat_min = min(kinsakubaru_coord$lat),
      lat_max = max(kinsakubaru_coord$lat)
    )
  ),
  # Remove the logs out of the target area boundary.
  tar_target(
    gis_agoop_screen,
    cbind(
      gis_agoop,
      st_coordinates(gis_agoop) %>%
        as.data.frame() %>%
        select(X, Y) %>%
        rename_with(~ c("long", "lat"))
    ) %>%
      filter(
        long >= kinsakubaru_boundary$long_min,
        long <= kinsakubaru_boundary$long_max,
        lat >= kinsakubaru_boundary$lat_min,
        lat <= kinsakubaru_boundary$lat_max
      )
  ),
  # Add information column of the intersect relationship between the sample points data and the Kinsakubaru polygon.
  # Bug: Takes about 5 hours.
  tar_target(
    gis_agoop_inter,
    gis_agoop_screen %>%
      mutate(inter = get_inter_id(gis_agoop_screen, kinsakubaru))
  ),
  tar_target(
    gis_agoop_inter_dailyid,
    gis_agoop_inter %>%
      group_by(dailyid) %>%
      summarise(inter = sum(inter) > 0) %>%
      ungroup() %>%
      filter(inter) %>%
      pull(dailyid)
  ),
  tar_target(
    gis_agoop_kinsakubaru,
    gis_agoop_inter %>%
      filter(dailyid %in% gis_agoop_inter_dailyid) %>%
      mutate(time = hour * 60 + minute) %>%
      arrange(month, day, dailyid, time) %>%
      # Add holiday information.
      mutate(date = as_date(paste(year, month, day, sep = "-"))) %>%
      left_join(holiday, by = "date") %>%
      # Add weather column.
      left_join(weather, by = "date")
  )
)

