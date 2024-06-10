library(stringr)
library(navr)
library(cyberframer)
source("functions/loading.R")
source("functions/analysis.R")

folder <- "example-data/VT1/"
dates <- find_session_dates(folder)

session <- load_session(folder, dates[2])
head(session$position)

create_pointing_table(session$generic)
extract_settings(session$generic)
session$position

pointings <- data.frame()
for (date in dates) {
  session <- load_session(folder, date)
  if (is.null(session)) next
  pointing <- create_pointing_table(session$generic)
  pointings <- rbind(pointings, pointing)
}

df_positions <- get_object_position()

pointings <- pointings %>%
  left_join(df_positions, by = c("target" = "Object",
                                 "LevelName" = "LevelName",
                                 "LevelSize" = "LevelSize"))

df_pointing_positions <- df_positions %>%
  filter(Object == "Pointing Position") %>%
  # add word start to each of the columns names x y z so that it becomes start_x
  rename_with(~str_c("pointingpoint_", .), c(x, y, z))

df_start_positions

pointings <- pointings %>%
  left_join(select(df_start_positions, starts_with("pointingpoint"), LevelName, LevelSize),
            by = c("LevelName" = "LevelName",
                   "LevelSize" = "LevelSize"))

pointings %>%
  rowwise() %>%
  mutate(target_pointed_distance = euclid_distance(c(ConfirmedPointing_x, ConfirmedPointing_y), c(x, y)),
         target_pointingpoint_distance = euclid_distance(c(x, y), c(pointingpoint_x, pointingpoint_y)),
         pointed_pointingpoint_distance = euclid_distance(c(ConfirmedPointing_x, ConfirmedPointing_y),
                                                          c(pointingpoint_x, pointingpoint_y))) %>%
  select(LevelName, LevelSize, Movement, target, target_pointed_distance,
         target_start_distance, pointed_start_distance) %>%
  View()
