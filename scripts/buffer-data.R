library(googlesheets4)
library(googledrive)

dir.create("temp/data", showWarnings = FALSE, recursive = TRUE)
drive_auth()

# Loading questionnaires ---------
drive_download(file = as_id("1pgQyD39k5KPln4wkyJXhH5215Krmq5Mb"),
                path = file.path("temp", "run1_survey_vistateleport.csv"))
drive_download(file = as_id("1pHEZzkKUFpZYzUsPc7N4YinztXeFdmIM"),
               path = file.path("temp", "run2_survey_vistateleport.json"))

# Load the position information
positions_sheet <- "18mrx8kSpGEbWkHkKVoYaUkAHyi1bZZVgaL7mDlRHkbc"
positions <- read_sheet(positions_sheet)
positions <- positions[, 1:5]
write.csv(positions, file.path("temp", "positions.csv"), row.names = FALSE)

# Loading experimental data
data_id <-  as_id("1gWFPSp4iao4PDg9FssHPrRIwExmeWU5Y")
drive_download(file = data_id, path = file.path("temp", "data.zip"))
unzip(file.path("temp", "data.zip"), exdir = file.path("temp", "data"))

# Loading processed data
dir.create(file.path("temp", "processed"), showWarnings = FALSE)
processed_data_id <- as_id("1Kc0MbFtfw11-D878F3NCbKDo1qX_Wrz7")
drive_download(file = processed_data_id, path = file.path("temp", "processed", "combined_data.csv"))

trial_data_id <- as_id("1R49jsVgxh0Vh9FoiczpqIsBx9wIgId_O")
drive_download(file = trial_data_id, path = file.path("temp", "processed", "trial_data.csv"))
