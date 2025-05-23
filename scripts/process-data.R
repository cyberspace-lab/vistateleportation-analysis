library(stringr)
library(navr)
library(cyberframer)
library(ggplot2)
library(dplyr)
library(tidyr)

source("functions/getters.R")
source("functions/loading.R")
source("functions/processing.R")
source("functions/analysis.R")

participants <- load_participants("temp/data/")
results <- analyze_participants(participants)

dir.create("temp/processed", recursive = TRUE, showWarnings = FALSE)
write.csv(results$pointing, "temp/processed/pointing20250221.csv")
write.csv(results$distance, "temp/processed/distance20250221.csv")
write.csv(results$timing, "temp/processed/timing20250221.csv")

### Read questionnarie
df_questionnaire <- read.csv("temp/results_survey_vistateleport.csv", sep = ";")

question_keys <- c("Vůbec" = 1, "Mírně" = 2, "Středně" = 3, "Velmi" = 4)
question_keys2 <- c("Rozhodně souhlasím" = 1, "Spíše souhlasím" = 2,
                    "Ani souhlasím, ani nesouhlasím" = 3,
                    "Spíše nesouhlasím" = 4, "Rozhodně nesouhlasím" = 5)
movement_keys <- c("Teleportace s optickým tokem" = "OpticFlow",
                   "Teleportace" = "Teleport")

### FIRST SSQ (ssq1)
df_ssq1 <- df_questionnaire[, c("ID", "Movement1", "X.SQ001.", "X.SQ002.",
                                "X.SQ003.", "X.SQ004.", "X.SQ005.", "X.SQ006.",
                                "X.SQ007.", "X.SQ008.", "X.SQ009.", "X.SQ010.",
                                "X.SQ011.", "X.SQ012.", "X.SQ013.", "X.SQ014.",
                                "X.SQ015.", "X.SQ016.")]

df_ssq1 <- df_ssq1 %>%
  mutate(across(-c(ID, Movement1), ~recode(.x, !!!question_keys))) %>%
  rowwise() %>%
  mutate(Movement1 = recode(Movement1, !!!movement_keys),
         ssq_sum1 = sum(across(-c(ID, Movement1))),
         ssq_avg1 = ssq_sum1 / 16,
         ssq_nausea1 = X.SQ001. + X.SQ006. + X.SQ007. + X.SQ008. +
           X.SQ009. + X.SQ015. + X.SQ016.,
         ssq_nausea1_total = ssq_nausea1 * 9.54,
         ssq_oculomotor1 = X.SQ001. + X.SQ002. + X.SQ003. +
           X.SQ004. + X.SQ005. + X.SQ009. + X.SQ011.,
         ssq_oculomotor1_total = ssq_oculomotor1 * 7.58,
         ssq_desorientation1 = X.SQ005. + X.SQ008. + X.SQ010. +
           X.SQ011. + X.SQ012. + X.SQ013. + X.SQ014.,
         ssq_desorientation1_total = (ssq_desorientation1 * 13.92),
         ssq_total1 = (ssq_desorientation1 + ssq_oculomotor1 + ssq_nausea1) * 3.74) %>%
  ungroup()

### SECOND SSQ ---------------
df_ssq2 <- df_questionnaire [, c("ID", "Movement2", "Copy.SQ001.", "Copy.SQ002.",
                                "Copy.SQ003.", "Copy.SQ004.", "Copy.SQ005.",
                                "Copy.SQ006.", "Copy.SQ007.", "Copy.SQ008.",
                                "Copy.SQ009.", "Copy.SQ010.", "Copy.SQ011.",
                                "Copy.SQ012.", "Copy.SQ013.", "Copy.SQ014.",
                                "Copy.SQ015.", "Copy.SQ016.")]

df_ssq2 <- df_ssq2 %>%
  mutate(across(-c(ID, Movement2), ~recode(.x, !!!question_keys))) %>%
  rowwise() %>%
  mutate(Movement2 = recode(Movement2, !!!movement_keys),
         ssq_sum2 = sum(across(-c(ID, Movement2))),
         ssq_avg2 = ssq_sum2 / 16,
         ssq_nausea2 = Copy.SQ001. + Copy.SQ006. + Copy.SQ007. +
           Copy.SQ008. + Copy.SQ009. + Copy.SQ015. + Copy.SQ016.,
         ssq_nausea2_total = ssq_nausea2 * 9.54,
         ssq_oculomotor2 = Copy.SQ001. + Copy.SQ002. + Copy.SQ003. +
           Copy.SQ004. + Copy.SQ005. + Copy.SQ009. + Copy.SQ011.,
         ssq_oculomotor2_total = ssq_oculomotor2 * 7.58,
         ssq_desorientation2 = Copy.SQ005. + Copy.SQ008. + Copy.SQ010. +
           Copy.SQ011. + Copy.SQ012. + Copy.SQ013. + Copy.SQ014.,
         ssq_desorientation2_total = ssq_desorientation2 * 13.92,
         ssq_total2 = (ssq_desorientation2 + ssq_oculomotor2 + ssq_nausea2) * 3.74) %>%
  ungroup()

### FIRST VRLEQ ---------------
df_vrleq1 <- df_questionnaire[, c("ID", "Movement1", "X.VRQ001.",
                                 "X.VRQ002.", "X.VRQ003.", "X.VRQ004.",
                                 "X.VRQ005.", "X.VRQ006.", "X.VRQ007.",
                                 "X.VRQ008.", "X.VRQ009.", "X.VRQ010.")]
df_vrleq1 <- mutate(df_vrleq1, across(-c(ID, Movement1),
                     ~recode(.x, !!!question_keys2))) %>%
  mutate(across(c("X.VRQ002.", "X.VRQ004.", "X.VRQ006.",
                  "X.VRQ008.", "X.VRQ010."),
                ~ case_when(.x == 5 ~ 1, .x == 4 ~ 2, .x == 2 ~ 4,
                            .x == 1 ~ 5, .x == 3 ~ 3))) %>%
  rowwise() %>%
  mutate(Movement1 = recode(Movement1, !!!movement_keys),
         vrleq_sum1 = sum(across(-c(ID, Movement1))),
         vrleq_avg1 = vrleq_sum1 / 10) %>%
  ungroup()

### SECOND VRLEQ ------
df_vrleq2 <- df_questionnaire[, c("ID", "Movement2", "Copy.VRQ001.",
                                  "Copy.VRQ002.", "Copy.VRQ003.", "Copy.VRQ004.",
                                  "Copy.VRQ005.", "Copy.VRQ006.", "Copy.VRQ007.",
                                  "Copy.VRQ008.", "Copy.VRQ009.", "Copy.VRQ010.")]
df_vrleq2 <- mutate(df_vrleq2, across(-c(ID, Movement2),
                    ~recode(.x, !!!question_keys2))) %>%
  mutate(across(c("Copy.VRQ002.", "Copy.VRQ004.", "Copy.VRQ006.",
                  "Copy.VRQ008.", "Copy.VRQ010."),
                ~ case_when(.x == 5 ~ 1, .x == 4 ~ 2, .x == 2 ~ 4,
                            .x == 1 ~ 5, .x == 3 ~ 3)))  %>%
  rowwise() %>%
  mutate(Movement2 = recode(Movement2, !!!movement_keys),
         vrleq_sum2 = sum(across(-c(ID, Movement2))),
         vrleq_avg2 = vrleq_sum2 / 10) %>%
  ungroup()

## df_questionnaires
convert_to_long <- function(df, val) {
  out <- df %>%
    ungroup() %>%
    pivot_longer(cols = -c(ID, Movement),
                 names_to = "score", values_to = "value") %>%
    mutate(score = stringr::str_remove(score, val))
  return(out)
}

df_ssq1_long <- convert_to_long(select(df_ssq1, ID, Movement = Movement1,
                                       starts_with("ssq_")), "1")
df_ssq2_long <- convert_to_long(select(df_ssq2, ID, Movement = Movement2,
                                       starts_with("ssq_")), "2")
df_vrleq1_long <- convert_to_long(select(df_vrleq1, ID, Movement = Movement1,
                                         starts_with("vrleq_")), "1")
df_vrleq2_long <- convert_to_long(select(df_vrleq2, ID, Movement = Movement2,
                                         starts_with("vrleq_")), "2")

df_questionnaire_summaries <- bind_rows(df_ssq1_long, df_ssq2_long,
                                        df_vrleq1_long, df_vrleq2_long) %>%
  pivot_wider(id_cols = c(ID, Movement),
              names_from = score, values_from = value)

write.csv(df_questionnaire_summaries, "temp/processed/questionnaire_summaries20250221.csv")

## Merging all data -----
str(results$pointing)
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
df_all %>%
  left_join(df_questionnaire_summaries,
            by = c("participant" = "ID", "Movement.x" = "Movement")) %>%
  write.csv("temp/processed/all_combined20250221.csv")

df_all_trials <- df_pointing %>%
  left_join(results$distance, by = c("participant" = "participant",
                                     "LevelName" = "LevelName",
                                     "LevelSize" = "LevelSize")) %>%
  left_join(results$timing, by = c("participant" = "participant",
                                   "LevelName" = "LevelName",
                                   "LevelSize" = "LevelSize"))
df_all_trials %>%
  left_join(df_questionnaire_summaries,
            by = c("participant" = "ID", "Movement.x" = "Movement")) %>%
  write.csv("temp/processed/all_combined_trials20250221.csv")
