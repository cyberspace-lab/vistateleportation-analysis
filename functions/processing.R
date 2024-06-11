library(dplyr)

add_object_positions <- function(df_pointing) {
  df_positions <- get_object_position()
  df_pointing <- df_pointing %>%
    left_join(df_positions, by = c("target" = "Object",
                                   "LevelName" = "LevelName",
                                   "LevelSize" = "LevelSize"))
  return(df_pointing)
}

add_pointing_positions <- function(df_pointings) {
  df_pointing_positions <- get_pointing_position()
  df_pointings <- df_pointings %>%
    left_join(select(df_pointing_positions, starts_with("pointingpoint"),
                     LevelName, LevelSize),
              by = c("LevelName" = "LevelName",
                     "LevelSize" = "LevelSize"))
  return(df_pointings)
}
