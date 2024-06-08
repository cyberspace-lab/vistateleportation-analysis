library(stringr)

extract_date <- function(filename){
  date <- str_extract(filename, "\\d+-\\d+")
  return(date)
}
