library(dplyr)
library(stringr)

extract_settings <- function(df) {
  df_settings <- filter(df, event %in% c("Movement", "LevelName", "LevelSize")) %>%
    select(event, information) %>%
    pivot_wider(names_from = event, values_from = information)
  return(df_settings)
}

create_visitation_table <- function(df) {
  df_visit <- filter(df, event %in% c("ItemVisited")) %>%
    select(time, information)
  return(df_visit)
}

create_pointing_table <- function(df) {
  df_pointing_target <- filter(df, event == "ShouldPointTo") %>%
    select(information, time)
  df_point <- filter(df, event %in% c("FirstPointing", "ConfirmedPointing")) %>%
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
    bind_cols(extract_settings(df))
  return(df_point)
}

