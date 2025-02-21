library(dplyr)
library(stringr)
library(tidyr)

is_training_level <- function(df_events) {
  setings <- extract_settings(df_events)
  return(setings$LevelName == "" || is.na(setings$LevelName))
}

extract_settings <- function(df_events) {
  df_settings <- df_events %>%
    filter(event %in% c("Movement", "LevelName", "LevelSize")) %>%
    select(event, information) %>%
    pivot_wider(names_from = event, values_from = information)
  df_settings$Movement <- case_match(as.numeric(df_settings$Movement),
                                     c(0) ~ "Teleport", c(1) ~ "OpticFlow")
  return(df_settings)
}

load_object_positions <- function() {
  df_positions <- NULL
  load("necessarydata/positions.RData")
  df_positions <- df_positions %>%
    rename_with(~str_c("object_", .), c(x, y, z))
  return(df_positions)
}

load_pointing_positions <- function() {
  df_positions <- load_object_positions()
  df_pointing_positions <- df_positions %>%
    filter(Object == "Pointing Position") %>%
    rename_with(~str_replace(., "object_", "pointingpoint_"),
                c(object_x, object_y, object_z))
  return(df_pointing_positions)
}
