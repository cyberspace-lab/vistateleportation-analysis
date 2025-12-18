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