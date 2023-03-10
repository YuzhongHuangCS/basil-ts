#
#   Helper functions for interactive R use

library("tidyverse")
library("jsonlite")

BASE_PATH <- ifelse(str_detect(getwd(), "basil-ts"),
                    ".",
                    "basil-ts")
BASE_PATH <- ifelse(str_detect(getwd(), "basil-ts/tests"),
                    "..",
                    BASE_PATH)

load_request <- function(ifp, base_path = BASE_PATH) {
  request_file <- sprintf("tests/io/andy_input_%s.json", ifp)
  request <- fromJSON(file.path(base_path, request_file))
  request
}

get_historical_data <- function(ifp) {
  request <- load_request(ifp)
  
  target <- data.frame(
    date  = as.Date(request$payload$historical_data$ts[, 1]),
    value = as.numeric(request$payload$historical_data$ts[, 2])
  )
  target
}

get_separation_cutoffs <- function(ifp) {
  request <- load_request(ifp)
  
}