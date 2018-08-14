#
#   Tools for interactive R debugging
#

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


get_target <- function(ifp_id) {
  input_fh <- sprintf("tests/io/andy_input_%s.json", ifp_id)
  request  <- jsonlite::fromJSON(input_fh)

  target <- data.frame(
    date  = as.Date(request$payload$historical_data$ts[, 1]),
    value = as.numeric(request$payload$historical_data$ts[, 2])
  )
  target
}

plot_ts <- function(ifp_id) {
  input_fh <- sprintf("tests/io/andy_input_%s.json", ifp_id)
  request  <- jsonlite::fromJSON(input_fh)

  target <- get_target(ifp_id)
  ifp_name  <- request$ifp$name
  p <- ggplot(target, aes(x = date, y = value)) +
    geom_line() +
    ggtitle(ifp_name)
  p
}

plot_fcast <- function(ifp_id) {
  NULL
}


brier <- function(prediction, outcome, ordered = TRUE) {
  stopifnot(length(prediction)==length(outcome))
  stopifnot(all(outcome %in% c(0L, 1L, NA)))
  stopifnot(all.equal(sum(prediction), sum(outcome), 1L) | any(is.na(prediction)))

  if (ordered) {
    pairs <- NULL
    for (i in 1:(length(prediction)-1)) {
      p_i <- sapply(split(prediction, 1:length(prediction) > i), sum)
      o_i <- sapply(split(outcome, 1:length(outcome) > i), max)
      pairs <- c(pairs, sum((p_i - o_i)^2))
    }
    brier <- mean(pairs)
  } else {
    brier <- sum((prediction - outcome)^2)
  }

  return(brier)
}

# D1 from T&E page 51: correct is .20833
# brier(c(.25, .25, .5, .0), c(0, 1, 0, 0), TRUE)
# D2 from T&E page 51: correct is .235
# brier(c(.25, .25, .3, .2), c(0, 1, 0, 0), TRUE)
# D1 from T&E page 52: .21
# brier(c(.0, .5, .25, .25), c(0, 1, 0, 0), TRUE)
# D2 from T&E page 52: .20
# brier(c(.2, .3, .4, .1), c(0, 1, 0, 0), TRUE)
