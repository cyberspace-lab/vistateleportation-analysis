extract_settings <- function(df) {
  df_settings <- filter(df, event %in% c("Movement", "LevelName", "LevelSize")) %>%
    select(event, information)
  return(df_settings)
}


create_visitation_table <- function(df) {
  df_visit <- filter(df, event %in% c("ItemVisited")) %>%
    select(time, information)
  return(df_visit)
}

create_pointing_table <- function(df) {
  df_pointing_order <- filter(df, event %in% c("ShouldPointTo"))
  df_first_pointing <- filter(df, event %in% c("First %>%Pointing"))
  df_confirmed_pointing <- filter(df, event %in% c("ConfirmedPointing"))
  return(df_point)
}