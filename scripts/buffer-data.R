library(googlesheets4)
library(googledrive)

# Authenticate
drive_auth()
file_id <- as_id("1pgQyD39k5KPln4wkyJXhH5215Krmq5Mb")
drive_download(file = file_id, path = "temp/results_survey_vistateleport.csv")

# Load the data
positions_sheet <- "18mrx8kSpGEbWkHkKVoYaUkAHyi1bZZVgaL7mDlRHkbc"
positions <- read_sheet(positions_sheet)

positions <- positions[, 1:5]
dir.create("temp", showWarnings = FALSE)
write.csv(positions, "temp/positions.csv", row.names = FALSE)
