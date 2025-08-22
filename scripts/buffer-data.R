library(googlesheets4)
library(googledrive)

dir.create("temp/data", showWarnings = FALSE, recursive = TRUE)

# Loading questionnaires
drive_auth()
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
