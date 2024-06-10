library(stringr)

extract_date <- function(filename) {
  date <- str_extract(filename, "\\d+-\\d+")
  return(date)
}

find_session_dates <- function(folder) {
  files <- list.files(folder, pattern = "*.txt")
  codes <- sapply(files, function(x) str_extract(x, "\\d+-\\d+"))
  if (!all(table(codes) == 3)) {
    stop("There is not three of everyone")
  }
  return(as.vector(unique(codes)))
}

load_session <- function(folder, date) {
  message("Loading session ", date)
  res <- list()
  res$generic <- open_generic_log(folder, date)
  if (!has_finished(res$generic)) {
    message("Session not finished, skipping and returning nll")
    return(NULL)
  }
  res$position <- open_position_log(folder, date)
  return(res)
}

has_finished <- function(generic_log) {
  return("Finished" %in% generic_log$information)
}

open_generic_log <- function(folder, date) {
  log <- find_log(folder, "GenericLogName", date)
  df <- read.table(log, sep = ";", fill = TRUE, skip = 1, header = FALSE)
  df[, 4] <- NULL
  colnames(df) <- c("time", "event", "information")
  return(df)
}

open_position_log <- function(folder, date) {
  log <- find_log(folder, "position", date)
  df <- read.table(log, sep = ";", fill = TRUE, skip = 1, header = FALSE)
  df[, 4] <- NULL
  colnames(df) <- c("time", "position", "rotation")
  df[, c("position.x", "position.y", "position.z")] <- unity_vector_to_numeric(df$position)
  df[, c("rotation.x", "rotation.y", "rotation.z")] <- unity_vector_to_numeric(df$rotation)
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

get_object_position <- function() {
  df_positions <- read.csv("temp/positions.csv")
  # split the position column into x, y, z separated by ;
  df_positions <- tidyr::separate(df_positions, Position,
                                  into = c("x", "y", "z"),
                                  sep = ";", convert = TRUE)
  return(df_positions)
}
