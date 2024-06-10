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

pointings <- add_object_positions(pointings)
pointings <- add_pointing_positions(pointings)
pointings$pointingpoint_x
pointings$pointingpoint_y

pointings <- filter(pointings, !is.na(LevelName), LevelName != "")

pointings %>%
  rowwise() %>%
  mutate(target_pointed_distance = euclid_distance(c(ConfirmedPointing_x, ConfirmedPointing_y), c(x, y)),
         pointingpoint_target_distance = euclid_distance(c(pointingpoint_x, pointingpoint_y), c(x, y)),
         pointingpoint_pointed_distance = euclid_distance(c(pointingpoint_x, pointingpoint_y), 
                                                          c(ConfirmedPointing_x, ConfirmedPointing_y)),
         pointingpoint_target_angle = angle_from_positions(c(pointingpoint_x, pointingpoint_y), c(x, y)),
         pointingpoint_pointed_angle = angle_from_positions(c(pointingpoint_x, pointingpoint_y),
                                                            c(ConfirmedPointing_x, ConfirmedPointing_y)),
         pointed_angle_difference = pointingpoint_pointed_angle - pointingpoint_target_angle)

navr::angle_from_positions()