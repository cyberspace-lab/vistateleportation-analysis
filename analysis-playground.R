library(stringr)
library(navr)
library(cyberframer)
library(ggplot2)
source("functions/loading.R")
source("functions/getters.R")
source("functions/processing.R")
source("functions/analysis.R")
source("functions/visualisation.R")
folder <- "temp/data/VT1/"

participant_data <- load_participant(folder)
participant_data[[1]]$events

session_pointings <- participant_data[[2]]$pointing
colnames(session_pointings)
View(session_pointings)

ggplot(session_pointings) +
  geom_point(aes(x = ConfirmedPointing_x, y = ConfirmedPointing_y),
             color = "blue", size = 10) +
  geom_label(aes(x = object_x, y = object_y - 50,
                 label = target),
             color = "green") +
  geom_point(aes(object_x, object_y), color = "green", size = 10) +
  geom_point(aes(pointingpoint_x, pointingpoint_y),
             color = "purple", size = 5) +
  theme_minimal() +
  geom_pointing_directions(session_pointings)

View(exp$pointing)
class(exp$events)
class(exp$navr)

df_pos <- load_object_positions()
View(df_pos)
load_pointing_positions()

results <- analyze_participant(participant_data)
