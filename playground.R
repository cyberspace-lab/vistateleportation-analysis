library(stringr)
library(navr)
library(cyberframer)
source("functions/loading.R")
source("functions/analysis.R")

folder <- "example-data/VT1/"
dates <- find_session_dates(folder)

session <- load_session(folder, dates[2])
head(session$position)

create_pointing_table(session$generic)

extract_settings(session$generic)

session$position 
