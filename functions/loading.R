library(stringr)
library(dplyr)
library(navr)

extract_date <- function(filename) {
  date <- str_extract(filename, "\\d+-\\d+")
  return(date)
}

find_session_dates <- function(folder) {
  files <- list.files(folder, pattern = "*.txt")
  codes <- sapply(files, function(x) str_extract(x, "\\d+-\\d+"))
  if (!all(table(codes) == 3)) {
    print(table(codes))
    stop("There is not three of everyone")
  }
  return(as.vector(unique(codes)))
}

load_participants <- function(folder) {
  folders <- list.dirs(folder, full.names = TRUE, recursive = FALSE)
  participants <- list()
  for (folder in folders) {
    message("------------------------------")
    message("Loading participant ", folder)
    participant <- basename(folder)
    participants[[participant]] <- load_participant(folder)
  }
  return(participants)
}

load_participant <- function(folder) {
  dates <- find_session_dates(folder)
  sessions <- list()
  for (date in dates) {
    session <- load_session(folder, date)
    if (is.null(session)) next
    sessions[[date]] <- session
  }
  return(sessions)
}

load_session <- function(folder, date) {
  message("Loading session ", date)
  res <- list()
  res$events <- open_generic_log(folder, date)
  if (!has_finished(res$events)) {
    message("Session not finished, skipping and returning nll")
    return(NULL)
  }
  df_pos <- open_position_log(folder, date)
  res$navr <- navr::NavrObject()
  res$navr$data <- df_pos
  res$navr <- navr::prepare_navr(res$navr)
  res$training <- is_training_level(res$events)
  if (!res$training) {
    res$pointing <- create_session_pointing(res, date, skip_training = TRUE)
  }
  return(res)
}

has_finished <- function(events_log) {
  return("Finished" %in% events_log$information)
}

open_generic_log <- function(folder, date) {
  log <- find_log(folder, "GenericLogName", date)
  df <- read.table(log, sep = ";", fill = TRUE, skip = 1, header = FALSE)
  df[, 4] <- NULL
  colnames(df) <- c("time", "event", "information")
  df <- mutate(df, time = as.numeric(str_trim(time)))
  return(df)
}

open_position_log <- function(folder, date) {
  log <- find_log(folder, "position", date)
  df <- read.table(log, sep = ";", fill = TRUE, skip = 1, header = FALSE)
  df[, 4] <- NULL
  colnames(df) <- c("timestamp", "pos", "rot")
  df$timestamp <- as.numeric(str_trim(df$timestamp))
  df[, c("position_x", "position_y", "position_z")] <-
    unity_vector_to_numeric(df$pos)
  df[, c("rotation_x", "rotation_y", "rotation_z")] <-
    unity_vector_to_numeric(df$rot)
  return(df)
}

open_session_log <- function(folder, date) {
  log <- find_log(folder, "Session", date)
}

find_log <- function(folder, type, date) {
  list_files <- list.files(folder, pattern = str_glue("*.{type}_{date}.txt"),
                           full.names = TRUE)
  if (length(list_files) == 0) {
    stop("No file found")
  }
  return(list_files[1])
  # return the file path
}

