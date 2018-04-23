
# testthat will set wd to location of this file, but sourcing and loading the
# sample requests depends on being in the top level basil-ts directory; this 
# should get us there. 
setwd("../../..")
if (!file.exists("basil-ts/tests/testthat.R")) stop("Path is wrong, check test.R")


# Date and misc helpers ---------------------------------------------------

context("Date and misc helpers")

test_that("Correct date sequences are generated", {
  expect_equal(
    bb_seq_period("2017-08-01", length.out = 5, bb_period("month")),
    structure(c(17379, 17410, 17440, 17471, 17501), class = "Date")
    )
  
  expect_equal(
    bb_seq_period("2017-08-01", length.out = 5, bb_period("day")),
    structure(c(17379, 17380, 17381, 17382, 17383), class = "Date")
  )
  
  expect_equal(  
    bb_seq_period("2017-08-01", length.out = 5, bb_period("fixed", 7)),
    structure(c(17379, 17386, 17393, 17400, 17407), class = "Date")
  )
  
  expect_equal(  
    bb_seq_period("2017-08-01", length.out = 5, bb_period("fixed", 14)),
    structure(c(17379, 17393, 17407, 17421, 17435), class = "Date")
  )
})


# Question parsing --------------------------------------------------------

context("Question parsing")

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
  
  # exception for (Month XX) type
  x <- parse_question_period("in April (Month 04) 2018")
  expect_equal(x$period$period, "month")
  expect_equal(x$date, structure(c(17622, 17651), class = "Date"))
})

test_that("Binary questions are ID'd and parsed", {
  
  q1 <- "Will there be any ACLED events in xxx?"
  q2 <- "Will ACLED record more than 500 events in xxx?"
  q3 <- "Will ACLED record less than 800 events in xxx?"
  q4 <- "Will ACLED record more than 5 but less than 800 events in xxx?"
  
  expect_equal(binary_seps(q1), c(">0", "0"))
  expect_equal(binary_seps(q2), c(">500", "<500"))
  expect_equal(binary_seps(q3), c(">800", "<800"))
  expect_error(binary_seps(q4))
  
})

test_that("Separations are correctly parsed", {
  seps   <- list(values = c("Yes", "No"), units = "boolean")
  expect_equal(
    parse_separations(seps, "count", "Will there be any")$cutpoints,
    c(Inf, 0.5, -Inf)
  )

  
  # negative values
  seps   <- list(values = c("<-1", "-1 - 0", "0 - 2", ">2"), units = "USD")
  expect_equal(
    parse_separations(seps, "continuous", "What will be")$cutpoints,
    c(-Inf, -1, 0, 2, Inf))
  
  # non-monotonic
  seps   <- list(values = c("<-1", "-1 - 1", ">2", "1 - 2"))
  expect_error(validate_seps(seps$values), "Looks like")
  
  # from 1514
  seps <- structure(list(values = c("<100", "100 - 140", "140 - 170", "170 - 210", 
                                    ">210"), unit = "number"), .Names = c("values", "unit"))
  qq <- "How many earthquakes of magnitude 5 or stronger will occur worldwide in May 2018?"
  parse_separations(seps, "count", qq)
  expect_equal(
    parse_separations(seps, "count", qq)$cutpoints,
    c(-Inf, 100, 140, 170, 210, Inf)
  )
  
  # from 1271
  seps <- list(values = c("0", "1 - 2", ">2"), unit = "number")
  qq <- "How many United Nations Security Council Resolutions concerning Syria will be vetoed by Russia between 22 April 2018 and 22 August 2018?"
  expect_equal(
    parse_separations(seps, "count", qq)$cutpoints,
    c(-Inf, 0.5, 2.5, Inf)
  )

})


# Data and forecast handling ----------------------------------------------

context("Data and forecast handling")

test_that("Series types are correctly ID's", {
  expect_equal(guess_series_type(c(0, 1, 0, 1, 0, 1), "will there be any"), "binary")
  expect_equal(guess_series_type(c(0, 1, 0, 1, 0, 1), "how many ACLED events will there"), "count")
  expect_equal(guess_series_type(c(0, 1, 0, 1, 0, 1), "what will the price be"), "binary")
  
  expect_equal(guess_series_type(rnorm(5, 10, 2), "what will the oil price be"), "continuous")
})

test_that("ACLED is recognized as count", {
  expect_equal(guess_series_type(c(1:5), "ACLED"), "count")
})

# Need to redo this, this works on the fcast object now
# test_that("Value constraints are enforced", {
#   x <- c(-1, 0, 1, 2)
#   expect_equal(enforce_series_type(x, "continuous"), x)
#   expect_equal(enforce_series_type(x, "count"), c(0, 0, 1, 2))
#   expect_equal(enforce_series_type(x, "binary"), c(0, 0, 1, 1))
# })

test_that("Category forecasts return correct length and order", {
  # check that length is correct
  cp <- c(-Inf, 1221, 1303, 1374, 1456, Inf)
  fc <- readRDS("basil-ts/tests/fcast_1028.rds")
  expect_equal(length(category_forecasts(fc, cp)), (length(cp) - 1))
  
  expect_equal(category_forecasts(fc, cp), rev(category_forecasts(fc, rev(cp))))
}) 



# Sample requests ---------------------------------------------------------

context("Sample requests")

test_that("Sample requests throw correct error", {
  expect_error(r_basil_ts("tests/io/example1.json"), NA)
  expect_error(r_basil_ts("tests/io/example2.json"), NA)
  expect_error(r_basil_ts("tests/io/example3.json"),
               "character string is not in a standard unambiguous format")
  expect_error(r_basil_ts("tests/io/example4.json"), 
               "exceed question end date")
  expect_error(r_basil_ts("tests/io/example5.json"), 
               "Separations contain ambiguous decimal separator")
})




