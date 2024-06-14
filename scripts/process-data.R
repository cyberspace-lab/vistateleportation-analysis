library(stringr)
library(navr)
library(cyberframer)
library(ggplot2)
library(dplyr)
source("functions/loading.R")
source("functions/processing.R")
source("functions/analysis.R")

participants <- load_participants("Data/")
results <- analyze_participants(participants)

write.csv(results$pointing, "pointing20240612.csv")
write.csv(results$distance, "distance20240612.csv")
write.csv(results$timing, "timing20240612.csv")

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

write.csv(df_all, "all_combined20240612.csv")

### Read questionnarie
df_questionnaire <- read.csv("Data/results-survey553517_coded_reversed.csv", sep = ";")
head(df_questionnaire)

colnames(df_questionnaire)

### FIRST SSQ (ssq1)

df_ssq1 <- df_questionnaire[,c("ID", "Movement1", "X.SQ001.","X.SQ002.",
                               "X.SQ003.", "X.SQ004.", "X.SQ005.", "X.SQ006.",
                               "X.SQ007.", "X.SQ008.", "X.SQ009.", "X.SQ010.", 
                               "X.SQ011.", "X.SQ012.", "X.SQ013.", "X.SQ014.", 
                               "X.SQ015.", "X.SQ016.")]

## sumacni Score + avg score SSQ1

df_ssq1 <- df_ssq1 %>%
  rowwise() %>%
  mutate(ssq_sum1 = sum(across(-c(ID, Movement1))),
         ssq_avg1 = ssq_sum1/16) %>%
  ungroup()

# Score nausea ssq1
df_ssq1 <- df_ssq1 %>%
  rowwise() %>%
  mutate(ssq_nausea1 = (X.SQ001. + X.SQ006. + X.SQ007.
                        + X.SQ008. + X.SQ009. + X.SQ015. 
                        + X.SQ016.)) %>%
  mutate(ssq_nausea1_total = (ssq_nausea1 * 9.54)) %>%
  ungroup()    

# Score oculomotor ssq1
df_ssq1 <- df_ssq1 %>%
  rowwise() %>%
  mutate(ssq_oculomotor1 = (X.SQ001. + X.SQ002. + X.SQ003.
                            + X.SQ004. + X.SQ005. + X.SQ009. 
                            + X.SQ011.)) %>%
  mutate(ssq_oculomotor1_total = (ssq_oculomotor1 * 7.58)) %>%
  ungroup()  

# Score desorientation ssq1
df_ssq1 <- df_ssq1 %>%
  rowwise() %>%
  mutate(ssq_desorientation1 = (X.SQ005. + X.SQ008. + X.SQ010.
                                + X.SQ011. + X.SQ012. + X.SQ013. 
                                + X.SQ014.)) %>%
  mutate(ssq_desorientation1_total = (ssq_desorientation1 * 13.92)) %>%
  ungroup()  

# Score total ssq1
df_ssq1 <- df_ssq1 %>%
  rowwise() %>%
  mutate(ssq_total1 = (ssq_desorientation1 + ssq_oculomotor1 + ssq_nausea1)*3.74) %>%
  ungroup() 

 

### SECOND SSQ

df_ssq2 <- df_questionnaire [,c("ID", "Movement2", "Copy.SQ001.", "Copy.SQ002.", "Copy.SQ003.", "Copy.SQ004.", "Copy.SQ005.", 
                                "Copy.SQ006.", "Copy.SQ007.", "Copy.SQ008.", "Copy.SQ009.", "Copy.SQ010.", "Copy.SQ011.", 
                                "Copy.SQ012.", "Copy.SQ013.", "Copy.SQ014.", "Copy.SQ015.", "Copy.SQ016.")]


df_ssq2 <- df_ssq2 %>%
  rowwise() %>%
  mutate(ssq_sum2 = sum(across(-c(ID, Movement2))),
         ssq_avg2 = ssq_sum2/16) %>%
  ungroup()

# Score nausea ssq2
df_ssq2 <- df_ssq2 %>%
  rowwise() %>%
  mutate(ssq_nausea2 = (Copy.SQ001. + Copy.SQ006. + Copy.SQ007.
                        + Copy.SQ008. + Copy.SQ009. + Copy.SQ015. 
                        + Copy.SQ016.)) %>%
  mutate(ssq_nausea2_total = (ssq_nausea2 * 9.54)) %>%
  ungroup()    

# Score oculomotor ssq1
df_ssq2 <- df_ssq2 %>%
  rowwise() %>%
  mutate(ssq_oculomotor2 = (Copy.SQ001. + Copy.SQ002. + Copy.SQ003.
                            + Copy.SQ004. + Copy.SQ005. + Copy.SQ009. 
                            + Copy.SQ011.)) %>%
  mutate(ssq_oculomotor2_total = (ssq_oculomotor2 * 7.58)) %>%
  ungroup()  

# Score desorientation ssq1
df_ssq2 <- df_ssq2 %>%
  rowwise() %>%
  mutate(ssq_desorientation2 = (Copy.SQ005. + Copy.SQ008. + Copy.SQ010.
                                + Copy.SQ011. + Copy.SQ012. + Copy.SQ013.
                                + Copy.SQ014.)) %>%
  mutate(ssq_desorientation2_total = (ssq_desorientation2 * 13.92)) %>%
  ungroup()  

# Score total ssq1
df_ssq1 <- df_ssq1 %>%
  rowwise() %>%
  mutate(ssq_total2 = (ssq_desorientation2 + ssq_oculomotor2 + ssq_nausea2)*3.74) %>%
  ungroup() 



###FIRST VRLEQ

df_vrleq1 <- df_questionnaire[,c("ID", "Movement1", "X.VRQ001.", "X.VRQ002.", "X.VRQ003.", "X.VRQ004.",
                                 "X.VRQ005.", "X.VRQ006.", "X.VRQ007.", "X.VRQ008.", "X.VRQ009.", "X.VRQ010.")]

df_vrleq1 <- df_vrleq1 %>%
  rowwise() %>%
  mutate(vrleq_sum1 = sum(across(-c(ID, Movement1))),
         vrleq_avg1 = vrleq_sum1/10) %>%
  ungroup()

###SECOND VRLEQ 
df_vrleq2 <- df_questionnaire[,c("ID", "Movement2", "Copy.VRQ001.", "Copy.VRQ002.", "Copy.VRQ003.", "Copy.VRQ004.",
                                 "Copy.VRQ005.", "Copy.VRQ006.", "Copy.VRQ007.", "Copy.VRQ008.", "Copy.VRQ009.", 
                                 "Copy.VRQ010.")]

df_vrleq2 <- df_vrleq2 %>%
  rowwise() %>%
  mutate(vrleq_sum2 = sum(across(-c(ID, Movement2))),
         vrleq_avg2 = vrleq_sum2/10) %>%
  ungroup()

