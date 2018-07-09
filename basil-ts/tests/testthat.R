
if (!require("testthat")) {
  msg <- paste(sep = "\n",
               "R unit tests require 'testthat' package, install with:",
               "  bash: install2.r testthat",
               "  R: install.packages('testthat')")
  stop(msg)
}

suppressMessages({
  library("testthat")
  library("jsonlite")
  library("dplyr")
  library("lubridate")
})


source("basil-ts/r-basil-ts.R")
source("basil-ts/models.R")

target_getter <- function(request) {
  target <- request$payload$historical_data$ts %>%
    `colnames<-`(c("date", "value")) %>%
    as.tibble() %>%
    mutate(date = date %>% substr(1, 10) %>% lubridate::ymd(), 
           value = as.numeric(value))
  target
}

test_dir("basil-ts/tests/testthat")
