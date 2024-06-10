library(googlesheets4)

# Load the data
positions_sheet <- "18mrx8kSpGEbWkHkKVoYaUkAHyi1bZZVgaL7mDlRHkbc"
positions <- read_sheet(positions_sheet)

positions <- positions[, 1:5]
dir.create("temp", showWarnings = FALSE)
write.csv(positions, "temp/positions.csv", row.names = FALSE)
