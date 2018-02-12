
# testthat will set wd to location of this file, but sourcing and loading the
# sample requests depends on being in the top level basil-ts directory; this 
# should get us there. 
setwd("../../..")
if (!file.exists("basil-ts/tests/testthat.R")) stop("Path is wrong, check test.R")

test_that("Question dates and periods are correctly parsed", {
  x <- parse_question_period("in August 2017")
  expect_equal(x$period$period, "month")
  expect_equal(x$dates, as.Date(c("2017-08-01", "2017-08-31")))
  
  x <- parse_question_period("on 27 December 2017")
  expect_equal(x$period$period, "day")
  expect_equal(x$date, as.Date(rep("2017-12-27", 2)))
  
  x <- parse_question_period("between 1 December 2017 and 31 December 2017")
  expect_equal(x$period$period, "month")
  expect_equal(x$date, as.Date(c("2017-12-01", "2017-12-31")))
  
  x <- parse_question_period("between 1 December 2017 and 1 January 2018")
  expect_equal(x$period$period, "fixed")
  expect_equal(x$period$days, 32)
  expect_equal(x$date, as.Date(c("2017-12-01", "2018-01-01")))
  
  # a week
  x <- parse_question_period("between 29 January 2018 and 4 February 2018")
  expect_equal(x$period$period, "fixed")
  expect_equal(x$period$days, 7)
  
  # these below were halfmonth, but not anymore; keep if want to re-add
  x <- parse_question_period("between 1 December 2017 and 15 December 2017")
  expect_equal(x$period$period, "fixed")
  expect_equal(x$period$days, 15)
  expect_equal(x$date, as.Date(c("2017-12-01", "2017-12-15")))
  
  x <- parse_question_period("between 1 December 2017 and 16 December 2017")
  expect_equal(x$period$period, "fixed")
  expect_equal(x$period$days, 16)
  expect_equal(x$date, as.Date(c("2017-12-01", "2017-12-16")))
  
  x <- parse_question_period("between 15 December 2017 and 31 December 2017")
  expect_equal(x$period$period, "fixed")
  expect_equal(x$date, as.Date(c("2017-12-15", "2017-12-31")))
  
  x <- parse_question_period("between 16 December 2017 and 31 December 2017")
  expect_equal(x$period$period, "fixed")
  expect_equal(x$date, as.Date(c("2017-12-16", "2017-12-31")))
})


test_that("Sample requests throw correct error", {
  expect_error(r_basil_ts("tests/requests/example1.json"), NA)
  expect_error(r_basil_ts("tests/requests/example2.json"), NA)
  expect_error(r_basil_ts("tests/requests/example3.json"),
               "character string is not in a standard unambiguous format")
  expect_error(r_basil_ts("tests/requests/example4.json"), 
               "exceed question end date")
  expect_error(r_basil_ts("tests/requests/example5.json"), 
               "Separations contain ambiguous decimal separator")
})

test_that("Series types are correctly ID's", {
  expect_equal(guess_series_type(c(0, 1, 0, 1, 0, 1), "will there be any"), "binary")
  expect_equal(guess_series_type(c(0, 1, 0, 1, 0, 1), "how many ACLED events will there"), "count")
  expect_equal(guess_series_type(c(0, 1, 0, 1, 0, 1), "what will the price be"), "binary")
  
  expect_equal(guess_series_type(rnorm(5, 10, 2), "what will the oil price be"), "continuous")
})

test_that("Binary questions are ID'd and parsed", {
  
  q1 <- "Will there be any ACLED events in xxx?"
  q2 <- "Will ACLED record more than 500 events in xxx?"
  q3 <- "Will ACLED record less than 800 events in xxx?"
  q4 <- "Will ACLED record more than 5 but less than 800 events in xxx?"
  
  expect_equal(binary_seps(q1), 1)
  expect_equal(binary_seps(q2), 500)
  expect_equal(binary_seps(q3), 800)
  expect_error(binary_seps(q4))
  
})

