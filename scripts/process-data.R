library(stringr)
library(navr)
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

write.csv(results$pointing, "temp/processed/pointing20250804.csv")
write.csv(results$distance, "temp/processed/distance20250804.csv")
write.csv(results$timing, "temp/processed/timing20250804.csv")

# Questionnaires --------------------------------------------------

## Run 1 ---------------------------------------------------------
### Read questionnaire
df_questionnaire <- read.csv("temp/run1_survey_vistateleport.csv", sep = ";")
question_keys <- c("Vůbec" = 1, "Mírně" = 2, "Středně" = 3, "Velmi" = 4)
question_keys2 <- c("Rozhodně souhlasím" = 1, "Spíše souhlasím" = 2,
                    "Ani souhlasím, ani nesouhlasím" = 3,
                    "Spíše nesouhlasím" = 4, "Rozhodně nesouhlasím" = 5)
movement_keys <- c("Teleportace s optickým tokem" = "OpticFlow",
                   "Teleportace" = "Teleport")

### FIRST SSQ (ssq1) ----------
df_ssq1 <- df_questionnaire[, c("ID", "Movement1", "X.SQ001.", "X.SQ002.",
                                "X.SQ003.", "X.SQ004.", "X.SQ005.", "X.SQ006.",
                                "X.SQ007.", "X.SQ008.", "X.SQ009.", "X.SQ010.",
                                "X.SQ011.", "X.SQ012.", "X.SQ013.", "X.SQ014.",
                                "X.SQ015.", "X.SQ016.")]
colnames(df_ssq1) <- c("ID", "Movement", paste0("question", 1:16))

df_ssq1 <- df_ssq1 %>%
  mutate(across(-c(ID, Movement), ~recode(.x, !!!question_keys))) %>%
  process_ssq() %>%
  select(ID, Movement, starts_with(c("ssq_", "vrsq_")))

### SECOND SSQ ---------------
df_ssq2 <- df_questionnaire[, c("ID", "Movement2", "Copy.SQ001.", "Copy.SQ002.",
                                "Copy.SQ003.", "Copy.SQ004.", "Copy.SQ005.",
                                "Copy.SQ006.", "Copy.SQ007.", "Copy.SQ008.",
                                "Copy.SQ009.", "Copy.SQ010.", "Copy.SQ011.",
                                "Copy.SQ012.", "Copy.SQ013.", "Copy.SQ014.",
                                "Copy.SQ015.", "Copy.SQ016.")]
colnames(df_ssq2) <- c("ID", "Movement", paste0("question", 1:16))
df_ssq2 <- df_ssq2 %>%
  mutate(across(-c(ID, Movement), ~recode(.x, !!!question_keys))) %>%
  process_ssq() %>%
  select(ID, Movement, starts_with(c("ssq_", "vrsq_")))

### FIRST VRLEQ ---------------

df_vrleq1 <- df_questionnaire[, c("ID", "Movement1", "X.VRQ001.",
                                  "X.VRQ002.", "X.VRQ003.", "X.VRQ004.",
                                  "X.VRQ005.", "X.VRQ006.", "X.VRQ007.",
                                  "X.VRQ008.", "X.VRQ009.", "X.VRQ010.")]
colnames(df_vrleq1) <- c("ID", "Movement1", paste0("question", 1:10))
df_vrleq1 <- mutate(df_vrleq1,
                    across(-c(ID, Movement1), ~recode(.x, !!!question_keys2)),
                    Movement1 = recode(Movement1, !!!movement_keys)) %>%
  process_vrleq() %>%
  select(ID, Movement = Movement1, starts_with("vrleq_"))

### SECOND VRLEQ ------
df_vrleq2 <- df_questionnaire[, c(
    "ID", "Movement2", "Copy.VRQ001.", "Copy.VRQ002.", "Copy.VRQ003.",
    "Copy.VRQ004.", "Copy.VRQ005.", "Copy.VRQ006.", "Copy.VRQ007.",
    "Copy.VRQ008.", "Copy.VRQ009.", "Copy.VRQ010.")]
colnames(df_vrleq2) <- c("ID", "Movement2", paste0("question", 1:10))
df_vrleq2 <- mutate(df_vrleq2, 
                    across(-c(ID, Movement2), ~recode(.x, !!!question_keys2)),
                    Movement2 = recode(Movement2, !!!movement_keys)) %>%
  process_vrleq() %>%
  select(ID, Movement = Movement2, starts_with("vrleq_"))

### Merging all together
convert_to_long <- function(df) {
  out <- df %>%
    pivot_longer(cols = -c(ID, Movement),
                 names_to = "score", values_to = "value")
  return(out)
}

df_ssq1_long <- convert_to_long(df_ssq1)
df_ssq2_long <- convert_to_long(df_ssq2)
df_vrleq1_long <- convert_to_long(df_vrleq1)
df_vrleq2_long <- convert_to_long(df_vrleq2)

df_surveys_run1 <-
  bind_rows(df_ssq1_long, df_ssq2_long, df_vrleq1_long, df_vrleq2_long) %>%
  pivot_wider(id_cols = c(ID, Movement),
              names_from = score, values_from = value)

write.csv(df_surveys_run1, "temp/processed/questionnaire_summaries_run1.csv")

## Run 2 -----------------------------------------------------------
surveys_run2 <- jsonlite::fromJSON("temp/run2_survey_vistateleport.json")
surveys_run2 <- organize_psychotron_data(surveys_run2)
surveys_run2 <- split_participant_email(surveys_run2)

fix_olivers_mistake <- function(df_tab) {
  fixed <- df_tab %>%
    filter(!grepl("test", participant, ignore.case = TRUE)) %>%
    arrange(completedAt) %>%
    mutate(participant = case_when(
      row_number() <= 3 & participant == "run1_1" ~ paste0("run1_", row_number()),
      TRUE ~ participant
    ))
  return(fixed)
}

## the question order is 1 18 2 4 5 ... and then regularly to 16
teleport_ssq <- surveys_run2$surveys$SSQT$data %>%
  fix_olivers_mistake() %>%
  rename(question3 = question2, question2 = question18) %>%
  mutate(across(starts_with("question"),
                ~as.numeric(str_remove(., "item")))) %>%
  process_ssq() %>%
  select(ID = participant, starts_with(c("ssq_", "vrsq_"))) %>%
  mutate(Movement = "Teleport") %>%
  pivot_longer(cols = -c(ID, Movement), names_to = "score", values_to = "value")

opticflow_ssq <- surveys_run2$surveys$SSQ_OF$data %>%
  fix_olivers_mistake() %>%
  rename(question3 = question2, question2 = question18) %>%
  mutate(across(starts_with("question"),
                ~as.numeric(str_remove(., "item")))) %>%
  process_ssq() %>%
  select(ID = participant, starts_with(c("ssq_", "vrsq_"))) %>%
  mutate(Movement = "OpticFlow") %>%
  pivot_longer(cols = -c(ID, Movement), names_to = "score", values_to = "value")

## VRLQ - použít otázky 17-26
teleport_vrleq <- surveys_run2$surveys$VRLEQT$data %>%
  fix_olivers_mistake() %>%
  select(ID = participant, num_range("question", 17:26)) %>%
  rename_with(~paste0("question", seq_along(.)), starts_with("question")) %>%
  process_vrleq() %>%
  select(ID, starts_with("vrleq_")) %>%
  mutate(Movement = "Teleport") %>%
  pivot_longer(cols = -c(ID, Movement), names_to = "score", values_to = "value")

opticflow_vrleq <- surveys_run2$surveys$VRLEQOF$data %>%
  fix_olivers_mistake() %>%
  select(ID = participant, num_range("question", 17:26)) %>%
  rename_with(~paste0("question", seq_along(.)), starts_with("question")) %>%
  process_vrleq() %>%
  select(ID, starts_with("vrleq_")) %>%
  mutate(Movement = "OpticFlow") %>%
  pivot_longer(cols = -c(ID, Movement), names_to = "score", values_to = "value")

## Demography
df_demog <- surveys_run2$surveys$VTDG$data %>%
  fix_olivers_mistake() %>%
  select(ID = participant, Age = question2, Gender = question1) %>%
  # recode question 1 so that Item 2 = Male and Item 3 = Female
  mutate(Gender = recode(Gender, "Item 1" = "Muž", "Item 2" = "Žena", "Item 3" = "Jiné"),
         Age = as.numeric(Age)) %>%
  bind_rows(select(df_questionnaire, ID, Age, Gender)) %>%
  distinct()
## Merge all surveys

df_surveys_run2 <- bind_rows(teleport_ssq, opticflow_ssq, teleport_vrleq, opticflow_vrleq) %>%
  pivot_wider(id_cols = c(ID, Movement),
              names_from = score, values_from = value)
df_surveys <- bind_rows(df_surveys_run1, df_surveys_run2) %>%
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
