plot_trial <- function(df) {
    
}

geom_pointing_directions <- function(df_pointings) {
  origin <- c(df_pointings$pointingpoint_x[1], df_pointings$pointingpoint_y[1])
  output <- list()
  for (i in seq_len(nrow(df_pointings))) {
    output[[i]] <- geom_navr_direction(origin, df_pointings$pointingpoint_pointed_angle[i], 100)
  }
  return(output)
}
