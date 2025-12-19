library(stringr)
library(cyberframer)
library(ggplot2)
library(dplyr)
library(tidyr)
library(psychotronr)

source("functions/getters.R")
source("functions/loading.R")
source("functions/processing.R")
source("functions/analysis.R")

if (file.exists("temp/participants.RData")) {
  load("temp/participants.RData")
} else {
  participants <- load_participants("temp/data/")
  save(participants, file = "temp/participants.RData")
}

results <- analyze_participants(participants)
dir.create("temp/processed", recursive = TRUE, showWarnings = FALSE)
write.csv(results$pointing, "temp/processed/pointing20251218.csv")
write.csv(results$distance, "temp/processed/distance20251218.csv")
write.csv(results$timing, "temp/processed/timing20251218.csv")

# Questionnaires --------------------------------------------------

## Run 1 ---------------------------------------------------------
source("scripts/process-run-1.R")

## Run 2 ---------------------------------------------------------
source("scripts/process-run-2.R")

## Run 3 ---------------------------------------------------------
source("scripts/process-run-3.R")

## Merge all surveys ---------------------------------------------
df_demog <- bind_rows(select(df_questionnaire, ID, Age, Gender),
                      df_demog_run2,
                      df_demog_run3) %>%
  distinct()

df_surveys <- bind_rows(df_surveys_run1, df_surveys_run2, df_surveys_run3) %>%
  left_join(df_demog, by = "ID")

## Merging all data ------------------------------------------------
agg_pointing <- results$pointing %>%
  filter(target != "OriginDoor") %>%
  mutate(abs_angle_error = abs(pointed_angle_difference),
         abs_distance_error = abs(target_pointed_distance_difference)) %>%
  group_by(participant, LevelName, LevelSize) %>%
  summarize(across(target_pointed_distance_difference:abs_distance_error,
                   list(mean = mean, median = median))) %>%
  ungroup()

df_all <- agg_pointing %>%
  left_join(results$distance, by = c("participant" = "participant",
                                     "LevelName" = "LevelName",
                                     "LevelSize" = "LevelSize")) %>%
  left_join(results$timing, by = c("participant" = "participant",
                                   "LevelName" = "LevelName",
                                   "LevelSize" = "LevelSize"))

df_all <- df_all %>%
  mutate(Movement = Movement.x,
         Vista = Vista.x) %>%
  select(-c(Movement.x, Movement.y, Vista.x, Vista.y))

df_all %>%
  left_join(df_surveys,
            by = c("participant" = "ID", "Movement" = "Movement")) %>%
  write.csv("temp/processed/all_combined20250822.csv")

df_all_trials <- results$pointing %>%
  left_join(results$distance, by = c("participant" = "participant",
                                     "LevelName" = "LevelName",
                                     "LevelSize" = "LevelSize")) %>%
  left_join(results$timing, by = c("participant" = "participant",
                                   "LevelName" = "LevelName",
                                   "LevelSize" = "LevelSize"))
df_all_trials %>%
  left_join(df_surveys,
            by = c("participant" = "ID", "Movement.x" = "Movement")) %>%
  write.csv("temp/processed/all_combined_trials20250822.csv")
