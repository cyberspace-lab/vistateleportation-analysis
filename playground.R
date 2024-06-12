library(stringr)
library(navr)
library(cyberframer)
library(ggplot2)
source("functions/loading.R")
source("functions/processing.R")
source("functions/analysis.R")

folder <- "example-data/VT1/"

participant_data <- load_participant(folder)
pointings <- create_participant_pointing(participant_data)

df_generic <- participant_data$`20240606-110843`$generic

create_pointing_table(a)
View(pointings)
create_participant_pointing(participant_data)

analysis_pointings <- analyze_pointing_data(pointings)

sesh <- participant_data$`20240606-110843`
participant_data
View(sesh$generic)
extract_settings(sesh$generic)

results <- analyze_participant(participant_data)

View(results$pointing)
ggplot(analysis_pointings) +
  geom_point(aes(x = ConfirmedPointing_x, y = ConfirmedPointing_y), color = "blue", size = 10) +
  geom_label(aes(x = ConfirmedPointing_x, y = ConfirmedPointing_y - 100, label = target), color="blue") +
  geom_point(aes(x = x, y = y), color = "green", size = 10) +
  geom_label(aes(x = x, y = y + 100, label = target), color = "green") +
  geom_point(aes(x = pointingpoint_x, y = pointingpoint_y), color = "purple", size = 5) +
  geom_label(aes(x = x, y = y + 150, label = target), color = "green") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Distance between target and pointed",
       y = "Distance between pointing point and target") +
  theme_minimal()

analyze_distances(participants$VT1$`20240606-110843`)
names(participants$VT1)
analyze_participant(participants$VT1)
