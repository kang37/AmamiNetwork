library(moveVis)
library(move)
library(lubridate)

# Function to get trajectory_animation.gif by dailyid.
get_anim <- function(dailyid_id) {
  # Make move file with GIS data of a dailyid.
  gis_agoop_smp_head <- gis_agoop_smp %>%
    filter(dailyid == temp_id_num$dailyid[dailyid_id]) %>%
    st_transform(4326)
  gis_agoop_smp_head_coord <- st_coordinates(gis_agoop_smp_head) %>%
    data.frame() %>%
    tibble() %>%
    rename_with(~c("x", "y"))
  gis_agoop_smp_head <-
    gis_agoop_smp_head %>%
    cbind(gis_agoop_smp_head_coord) %>%
    mutate(time = as_datetime(
      paste(
        paste(year, month, day, sep = "-"),
        paste(hour, minute, "00", sep = "-"),
        sep = " "
      )
    )) %>%
    arrange(time) %>%
    # Keep only one log for a unique hour-minute.
    # Bug: Should group by time (minute).
    group_by(hour) %>%
    mutate(rowid = row_number()) %>%
    ungroup() %>%
    filter(rowid == 1)
  move_data <- move(
    x = gis_agoop_smp_head$x, y = gis_agoop_smp_head$y,
    time = gis_agoop_smp_head$time,
    proj = projection(gis_agoop_smp_head)
  )

  # Align move_data to a uniform time scale.
  m <- align_move(move_data, res = "mean")

  frames <- frames_spatial(m, alpha = 0.5) %>%
    add_labels(x = "Longitude", y = "Latitude") %>%
    add_northarrow() %>%
    add_scalebar() %>%
    add_timestamps(type = "label") %>%
    add_progress()

  # Animate frames.
  animate_frames(
    frames, out_file = paste0("data_edited/", dailyid_id, ".gif"),
    display = FALSE, fps = 1
  )
}
# Exporte 1-50 examples, some of them are not valid.
get_anim(1)
