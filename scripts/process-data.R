library(stringr)
library(navr)
library(cyberframer)
library(ggplot2)
source("functions/loading.R")
source("functions/processing.R")
source("functions/analysis.R")

participants <- load_participants("Data/")
results <- analyze_participants(participants)

write.csv(results$pointing, "pointing20240611-vt1-12.csv")
write.csv(results$distance, "distance20240611-vt1-12.csv")
write.csv(results$timing, "timing20240611-vt1-12.csv")

str(results$pointing)

agg_pointing <- results$pointing %>%
  filter(target != "OriginDoor") %>%
  group_by(participant, LevelName, LevelSize) %>%
  summarize(across(target_pointed_distance_difference:pointed_angle_difference,
                   list(mean = mean, median = median))) %>%
  ungroup()

df_all <- agg_pointing %>%
  left_join(results$distance, by = c("participant" = "participant",
                                     "LevelName" = "LevelName",
                                     "LevelSize" = "LevelSize")) %>%
  left_join(results$timing, by = c("participant" = "participant",
                                   "LevelName" = "LevelName",
                                   "LevelSize" = "LevelSize"))
