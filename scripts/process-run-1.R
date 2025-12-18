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
  mutate(across(-c(ID, Movement), ~recode(.x, !!!question_keys)),
         Movement = recode(Movement, !!!movement_keys)) %>%
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
  mutate(across(-c(ID, Movement), ~recode(.x, !!!question_keys)),
         Movement = recode(Movement, !!!movement_keys)) %>%
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
                    Movement = recode(Movement1, !!!movement_keys)) %>%
  process_vrleq() %>%
  select(ID, Movement, starts_with("vrleq_"))

### SECOND VRLEQ ------
df_vrleq2 <- df_questionnaire[, c(
    "ID", "Movement2", "Copy.VRQ001.", "Copy.VRQ002.", "Copy.VRQ003.",
    "Copy.VRQ004.", "Copy.VRQ005.", "Copy.VRQ006.", "Copy.VRQ007.",
    "Copy.VRQ008.", "Copy.VRQ009.", "Copy.VRQ010.")]
colnames(df_vrleq2) <- c("ID", "Movement2", paste0("question", 1:10))
df_vrleq2 <- mutate(df_vrleq2,
                    across(-c(ID, Movement2), ~recode(.x, !!!question_keys2)),
                    Movement = recode(Movement2, !!!movement_keys)) %>%
  process_vrleq() %>%
  select(ID, Movement, starts_with("vrleq_"))

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