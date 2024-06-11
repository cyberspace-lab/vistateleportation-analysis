library(dplyr)
library(stringr)

extract_settings <- function(df_generic) {
  df_settings <- filter(df_generic, event %in% c("Movement", "LevelName", "LevelSize")) %>%
    select(event, information) %>%
    pivot_wider(names_from = event, values_from = information)
  df_settings$Movement <- case_match(as.numeric(df_settings$Movement),
                                     c(0) ~ "Teleport", c(1) ~ "OpticFlow")
  return(df_settings)
}

is_training_level <- function(df_generic) {
  setings <- extract_settings(df_generic)
  return(setings$LevelName == "" || is.na(setings$LevelName))
}

create_visitation_table <- function(df) {
  df_visit <- filter(df, event %in% c("ItemVisited")) %>%
    select(time, information)
  return(df_visit)
}

create_pointing_table <- function(df_generic) {
  df_pointing_target <- filter(df_generic, event == "ShouldPointTo") %>%
    select(information, time)
  df_point <- filter(df_generic, event %in% c("FirstPointing", "ConfirmedPointing")) %>%
    # information is as a string None(X=167.732 Y=1025.189 Z=-8.000)
    # remove the None and the brackets and split the string three columns x y z
    mutate(information = str_remove(information, "None\\("),
           information = str_remove(information, "\\)"),
           x = as.numeric(str_extract(information, "X=([-]{0,1}\\d+\\.\\d+).*", 1)),
           y = as.numeric(str_extract(information, "Y=([-]{0,1}\\d+\\.\\d+).*", 1)),
           z = as.numeric(str_extract(information, "Z=([-]{0,1}\\d+\\.\\d+).*", 1))) %>%
           select(-information) %>%
           mutate(id = rep(1:(nrow(.) / 2), each = 2)) %>%
           pivot_wider(id_cols = id, names_from = event,
                       values_from = c(time, x, y, z), names_glue = "{event}_{.value}")
  df_point$target <- df_pointing_target$information
  df_point$start_time <- df_pointing_target$time
  df_point <- select(df_point, -id, start_time, target, everything()) %>%
    # bind the settings to the pointing table, although settings only has one row and 
    bind_cols(extract_settings(df_generic))
  return(df_point)
}

create_participant_pointing <- function(participant_data, skip_training = TRUE) {
  pointings <- data.frame()
  for (session_name in names(participant_data)) {
    session <- participant_data[[session_name]]
    pointing <- create_pointing_table(session$generic)
    if (!skip_training) {
      pointings <- rbind(pointings, pointing)
      next
    }
    if (is_training_level(session$generic)) next
    pointing$session <- session_name
    pointings <- rbind(pointings, pointing)
  }
  pointings <- add_object_positions(pointings)
  pointings <- add_pointing_positions(pointings)
  return(pointings)
}


analyze_pointing_data <- function(pointings) {
  analysis_pointings <- pointings %>%
    rowwise() %>%
    mutate(target_pointed_distance = euclid_distance(c(ConfirmedPointing_x, ConfirmedPointing_y), c(x, y)),
          pointingpoint_target_distance = euclid_distance(c(pointingpoint_x, pointingpoint_y), c(x, y)),
          pointingpoint_pointed_distance = euclid_distance(c(pointingpoint_x, pointingpoint_y), 
                                                           c(ConfirmedPointing_x, ConfirmedPointing_y)),
          pointingpoint_target_angle = angle_from_positions(c(pointingpoint_x, pointingpoint_y), c(x, y)),
          pointingpoint_pointed_angle = angle_from_positions(c(pointingpoint_x, pointingpoint_y),
                                                             c(ConfirmedPointing_x, ConfirmedPointing_y)),
          pointed_angle_difference = angle_diff(pointingpoint_pointed_angle, pointingpoint_target_angle)) %>%
    ungroup() %>%
    select(target, LevelName, LevelSize, session, target_pointed_distance,
           pointingpoint_target_distance, pointingpoint_pointed_distance,
           pointingpoint_target_angle, pointingpoint_pointed_angle, pointed_angle_difference)
  return(analysis_pointings)
}

analyze_timings <- function(df_generic, skip_training = TRUE) {
  ## This is a hacky way, but it works
  if (is_training_level(df_generic) && skip_training) {
    message("Skipping training level analysis")
    return(NULL)
  }
  out <- extract_settings(df_generic)
  timings <- df_generic %>%
    filter(event %in% c("Movement", "ItemVisited", "PhaseStarted",
                        "ShouldPointTo", "FirstPointing")) %>%
    select(time) %>%
    mutate(Event = c("time_experimentstarted", "times_item1", "time_item2",
                     "time_item3", "time_item4", "time_returnstarted",
                     "time_returned", "time_pointingorigin_start",
                     "time_pointingorigin_end", "time_pointing1_start",
                     "time_pointing1_end", "time_pointing2_start",
                     "time_pointing2_end", "time_pointing3_start",
                     "time_pointing3_end", "time_pointing4_start",
                     "time_pointing4_end")) %>%
    pivot_wider(names_from = Event, values_from = time) %>%
    mutate(duration_searching = time_returnstarted - time_experimentstarted,
           duration_return = time_returned - time_returnstarted,
           duration_pointing1 = time_pointing1_end - time_pointing1_start,
           duration_pointing2 = time_pointing2_end - time_pointing2_start,
           duration_pointing3 = time_pointing3_end - time_pointing3_start,
           duration_pointing4 = time_pointing4_end - time_pointing4_start)
  out <- bind_cols(out, timings)
  return(out)
}

analyze_distances <- function(session, skip_training = TRUE) {
  if (is_training_level(session$generic) && skip_training) {
    message("Skipping training level analysis")
    return(NULL)
  }
  out <- extract_settings(session$generic)
  times <- filter(session$generic, event %in% c("PhaseStarted")) %>% pull(time)
  timestamps <- sapply(times, \(x) which(session$position$timestamp > x)[1])
  distances = session$navr$data$distance_total[timestamps]
  out <- bind_cols(out, data.frame(distance_items = distances[1], distance_return = distances[2]))
  return(out)
}

analyze_participant <- function(participant_data, skip_training = TRUE) {
  pointings <- analyze_pointing_data(create_participant_pointing(participant_data))
  timings <- data.frame()
  distances <- data.frame()
  session_names <- names(participant_data)
  for (session_name in session_names) {
    session <- participant_data[[session_name]]
    timing <- analyze_timings(session$generic)
    if (is.null(timing)) next
    timing$session <- session_name
    timings <- rbind(timings, timing)
    distance <- analyze_distances(session)
    distance$session <- session_name
    distances <- rbind(distances, distance)
  }
  return(list(pointing = pointings, timing = timings, distnace = distances))
}

analyze_participants <- function(participants) {
  results <- list(pointing = data.frame(), timing = data.frame(), distance = data.frame())
  for (participant in names(participants)) {
    message("Analyzing participant ", participant)
    res <- analyze_participant(participants[[participant]])
    res$pointing$participant <- participant
    res$timing$participant <- participant
    res$distance$participant <- participant

    results$pointing <- rbind(results$pointing, res$pointing)
    results$timing <- rbind(results$timing, res$timing)
    results$distance <- rbind(results$distance, res$distance)
  }
  return(results)
}
