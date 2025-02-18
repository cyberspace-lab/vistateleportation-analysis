library(googlesheets4)
library(googledrive)

dir.create("temp/data", showWarnings = FALSE, recursive = TRUE)

drive_auth()
file_id <- as_id("1pgQyD39k5KPln4wkyJXhH5215Krmq5Mb")
drive_download(file = file_id, path = "temp/results_survey_vistateleport.csv")

# Load the data
positions_sheet <- "18mrx8kSpGEbWkHkKVoYaUkAHyi1bZZVgaL7mDlRHkbc"
positions <- read_sheet(positions_sheet)
positions <- positions[, 1:5]
write.csv(positions, "temp/positions.csv", row.names = FALSE)

data_id <-  as_id("1K2uInHO3pnEJv3xZQnza_Jas2tYk9468")
drive_download(file = data_id, path = "temp/data.zip")
unzip("temp/data.zip", exdir = "temp/data")
