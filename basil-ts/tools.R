

suppressPackageStartupMessages({
  library("methods")
  library("forecast")
  library("lubridate")
  library("jsonlite")
  library("stringr")
  library("truncnorm")
  
  library("tidyverse")
  library("ggplot2")
})


plot_ts <- function(ifp_id) {
  input_fh <- sprintf("tests/io/andy_input_%s.json", ifp_id)
  request  <- jsonlite::fromJSON(input_fh)

  target <- data.frame(
    date  = as.Date(request$payload$historical_data$ts[, 1]),
    value = as.numeric(request$payload$historical_data$ts[, 2])
  )
  ifp_name  <- request$ifp$name
  p <- ggplot(target, aes(x = date, y = value)) +
    geom_line() +
    ggtitle(ifp_name)
  p
}

plot_fcast <- function(ifp_id) {
  NULL
}