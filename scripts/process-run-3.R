library(dplyr)
library(tidyr)

surveys_run3 <- jsonlite::fromJSON("temp/run3_survey_vistateleport.json")
surveys_run3 <- organize_psychotron_data(surveys_run3)
surveys_run3 <- split_participant_email(surveys_run3)

run3_ssq <- function(ssq_data, movement_type) {
  res <- ssq_data %>%
    mutate(across(starts_with("SSQ"),
                  ~as.numeric(str_remove(., "item")))) %>%
    rename_with(~paste0("question", seq_along(.)), starts_with("SSQ")) %>%
    process_ssq() %>%
    select(ID = participant, starts_with(c("ssq_"))) %>%
    mutate(Movement = movement_type) %>%
    pivot_longer(cols = -c(ID, Movement), names_to = "score", values_to = "value")
  return(res)
}

teleport_ssq <- run3_ssq(surveys_run3$surveys$SSQ_TELEPORT$data, "Teleport")
opticflow_ssq <- run3_ssq(surveys_run3$surveys$SSQ_OPTIC$data, "OpticFlow")

run3_vrleq <- function(vrleq_data, movement_type) {
  res <- vrleq_data %>%
    rename_with(~paste0("question", seq_along(.)), starts_with("VRLEQ")) %>%
    select(ID = participant, starts_with("question")) %>%
    process_vrleq() %>%
    select(ID, starts_with("vrleq_")) %>%
    mutate(Movement = movement_type) %>%
    pivot_longer(cols = -c(ID, Movement), names_to = "score", values_to = "value")
  return(res)
}

teleport_vrleq <- run3_vrleq(surveys_run3$surveys$VRLEQ_TELEPORT$data, "Teleport")
opticflow_vrleq <- run3_vrleq(surveys_run3$surveys$VRLEQ_OPTIC$data, "OpticFlow")

df_surveys_run3 <- bind_rows(teleport_ssq, opticflow_ssq, teleport_vrleq, opticflow_vrleq) %>%
  pivot_wider(id_cols = c(ID, Movement),
              names_from = score, values_from = value)

## Demography -------------------------------------------------
df_demog_run3 <- surveys_run3$surveys$Vista_demography$data %>%
  select(ID = participant, Age = question2, Gender = question1) %>%
  # recode question 1 so that Item 2 = Male and Item 3 = Female
  mutate(Gender = recode(Gender, "Item 1" = "Muž", "Item 2" = "Žena", "Item 3" = "Jiné"),
         Age = as.numeric(Age))
