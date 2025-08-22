library(dplyr)
library(navr)

add_object_positions <- function(df_pointing) {
  df_positions <- load_object_positions()
  df_pointing <- df_pointing %>%
    left_join(df_positions, by = c("target" = "Object",
                                   "LevelName" = "LevelName",
                                   "LevelSize" = "LevelSize"))
  return(df_pointing)
}

add_pointing_positions <- function(df_pointings) {
  df_pointing_positions <- load_pointing_positions()
  df_pointings <- df_pointings %>%
    left_join(select(df_pointing_positions, starts_with("pointingpoint"),
                     LevelName, LevelSize),
              by = c("LevelName" = "LevelName",
                     "LevelSize" = "LevelSize"))
  return(df_pointings)
}


create_participant_pointing <- function(participant_data,
                                        skip_training = TRUE) {
  pointings <- data.frame()
  for (session_name in names(participant_data)) {
    pointing <- participant_data[[session_name]]$pointing
    pointings <- rbind(pointings, pointing)
  }
  if (skip_training) pointings <- pointings[pointings$training == FALSE, ]
  return(pointings)
}

create_session_pointing <- function(session, date, skip_training = TRUE) {
  is_training <- is_training_level(session$events)
  if (skip_training && is_training) {
    return(NULL)
  }
  pointing <- create_pointing_table(session$events)
  pointing$session <- date
  pointing$training <- is_training
  pointing <- add_object_positions(pointing)
  pointing <- add_pointing_positions(pointing)
  pointing <- analyze_pointing_data(pointing, return_only_results = FALSE)
  return(pointing)
}

create_pointing_table <- function(df_events) {
  df_pointing_target <- filter(df_events, event == "ShouldPointTo") %>%
    select(information, time)
  df_point <- df_events %>%
    filter(event %in% c("FirstPointing", "ConfirmedPointing")) %>%
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
                       values_from = c(time, x, y, z),
                       names_glue = "{event}_{.value}")
  df_point$target <- df_pointing_target$information
  df_point$start_time <- df_pointing_target$time
  df_point <- select(df_point, -id, start_time, target, everything()) %>%
    bind_cols(extract_settings(df_events))
  return(df_point)
}

#' Processes ssq data on a questionnaire which contains columns question1 - question 16
process_ssq <- function(ssq_data) {
  ssq_results <- ssq_data  %>%
    # from the questions 1-16 remove the word item from the value
    rowwise() %>%
    mutate(ssq_sum = sum(across(starts_with("question"), as.numeric)),
           ssq_avg = ssq_sum / 16,
           ssq_nausea = question1 + question6 + question7 +
            question8 + question9 + question15 + question16,
           ssq_nausea_total = ssq_nausea * 9.54,
           ssq_oculomotor = question1 + question2 + question3 + question4 +
            question5 + question9 + question11,
           ssq_oculomotor_total = ssq_oculomotor * 7.58,
           ssq_desorientation = question5 + question8 + question10 +
             question11 + question12 + question13 + question14,
           ssq_desorientation_total = ssq_desorientation * 13.92,
           ssq_total = (ssq_desorientation + ssq_oculomotor + ssq_nausea) * 3.74,
           vrsq_oculomotor = question1 + question2 + question4 + question5 / 12,
           vrsq_disorientation = question3 + question10 + question11 + question12 +
            question13 + question14 / 15,
           vrsq_total = (vrsq_oculomotor + vrsq_disorientation)/2) %>%
    ungroup()
  return(ssq_results)
}

#' processes vrleq questionnaire. It expects to have questions 1-10
process_vrleq <- function(vrleq_data) {
  vrleq_results <- vrleq_data %>%
    mutate(across(c("question2", "question3", "question6",
                    "question8", "question10"),
                  ~case_when(.x == 5 ~ 1, .x == 4 ~ 2, .x == 2 ~ 4,
                             .x == 1 ~ 5, .x == 3 ~ 3))) %>%
    rowwise() %>%
    mutate(vrleq_sum = sum(across(starts_with("question"))),
           vrleq_avg = vrleq_sum / 10) %>%
    ungroup()
  return(vrleq_results)
}
