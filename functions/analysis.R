library(dplyr)
library(stringr)
library(navr)
library(cyberframer)
library(tidyr)

create_visitation_table <- function(df_events) {
  df_visit <- filter(df_events, event %in% c("ItemVisited")) %>%
    select(time, information)
  return(df_visit)
}

analyze_pointing_data <- function(pointings, return_only_results = TRUE) {
  analysis_pointings <- pointings %>%
    rowwise() %>%
    mutate(pointingpoint_target_distance = euclid_distance(c(pointingpoint_x, pointingpoint_y),
                                                           c(object_x, object_y)),
           pointingpoint_pointed_distance = euclid_distance(c(pointingpoint_x, pointingpoint_y),
                                                            c(ConfirmedPointing_x, ConfirmedPointing_y)),
           target_pointed_distance_difference = pointingpoint_pointed_distance - pointingpoint_target_distance,
           pointingpoint_target_angle = angle_from_positions(c(pointingpoint_x, pointingpoint_y),
                                                             c(object_x, object_y)),
           pointingpoint_pointed_angle = angle_from_positions(c(pointingpoint_x, pointingpoint_y),
                                                              c(ConfirmedPointing_x, ConfirmedPointing_y)),
           pointed_angle_difference = angle_diff(pointingpoint_pointed_angle, pointingpoint_target_angle)) %>%
    ungroup()
  if (return_only_results) {
    analysis_pointings <- analysis_pointings %>%
      select(target, Movement, LevelName, LevelSize, session, target_pointed_distance_difference,
             pointingpoint_target_distance, pointingpoint_pointed_distance,
             pointingpoint_target_angle, pointingpoint_pointed_angle, pointed_angle_difference)
  }
  return(analysis_pointings)
}

analyze_timings <- function(df_events, skip_training = TRUE) {
  ## This is a hacky way, but it works
  if (is_training_level(df_events) && skip_training) {
    message("Skipping training level analysis")
    return(NULL)
  }
  out <- extract_settings(df_events)
  timings <- df_events %>%
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
  if (is_training_level(session$events) && skip_training) {
    message("Skipping training level analysis")
    return(NULL)
  }
  out <- extract_settings(session$events)
  times <- filter(session$events, event %in% c("PhaseStarted")) %>% pull(time)
  timestamps <- sapply(times, \(x) which(session$navr$data$timestamp > x)[1])
  distances <- session$navr$data$distance_total[timestamps]
  out <- bind_cols(out, data.frame(distance_items = distances[1], distance_return = distances[2]))
  return(out)
}

analyze_participant <- function(participant_data, skip_training = TRUE) {
  pointings <- data.frame()
  timings <- data.frame()
  distances <- data.frame()
  session_names <- names(participant_data)
  for (session_name in session_names) {
    session <- participant_data[[session_name]]
    if(skip_training && session$training) {
      message("Skipping training level ", session_name)
      next
    }
    timing <- analyze_timings(session$events)
    if (is.null(timing)) next
    timing$session <- session_name
    timings <- rbind(timings, timing)
    distance <- analyze_distances(session)
    distance$session <- session_name
    distances <- rbind(distances, distance)
    pointings <- rbind(pointings, session$pointing)
  }
  return(list(pointing = pointings, timing = timings, distance = distances))
}


analyze_participants <- function(participants) {
  results <- list(pointing = data.frame(),
                  timing = data.frame(),
                  distance = data.frame())
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
