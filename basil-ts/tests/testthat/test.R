
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


test_that("Days in period are correctly identified", {
  daily <- list(period = "day", days = NA)
  expect_equal(find_days_in_period(as.Date("2018-05-01"), daily), 1)
  
  monthly <- list(period = "month", days = NA)
  expect_equal(find_days_in_period(as.Date("2018-05-01"), monthly), 31)
  expect_equal(find_days_in_period(as.Date("2018-02-01"), monthly), 28)
})

test_that("Category forecasts return correct length and order", {
  # check that length is correct
  cp <- c(-Inf, 1221, 1303, 1374, 1456, Inf)
  fc <- readRDS("basil-ts/tests/fcast_1028.rds")
  expect_equal(length(category_forecasts(fc, cp)), (length(cp) - 1))
  
  expect_equal(category_forecasts(fc, cp), rev(category_forecasts(fc, rev(cp))))
}) 


test_that("Forecast generator runs", {
  pr <- structure(list(ifp_name = "What will be the FAO Dairy Price Index in May 2018?", 
                       binary_ifp = FALSE, data_period = structure(list(period = structure(list(
                         period = "month", days = NA), .Names = c("period", "days"
                         ))), .Names = "period"), was_data_aggregated = FALSE, separations = structure(list(
                           cutpoints = c(-Inf, 130, 150, 170, 190, Inf), values = c("<130", 
                                                                                    "130 - 150", "150 - 170", "170 - 190", ">190"), unit = "number", 
                           numeric_values = c("<130", "130 - 150", "150 - 170", 
                                              "170 - 190", ">190")), .Names = c("cutpoints", "values", 
                                                                                "unit", "numeric_values")), question_period = structure(list(
                                                                                  period = "month", days = NA), .Names = c("period", "days"
                                                                                  )), question_date = structure(c(17652, 17682), class = "Date"), 
                       series_type = "continuous", partial_train = "discarded", 
                       partial_outcome = FALSE, last_event_date = structure(17591, class = "Date"), 
                       h = 3L, lambda = NULL, fcast_dates = structure(c(17591, 17622, 
                                                                        17652), class = "Date"), backcast = FALSE, agg_method = "mean"), .Names = c("ifp_name", 
                                                                                                                                                    "binary_ifp", "data_period", "was_data_aggregated", "separations", 
                                                                                                                                                    "question_period", "question_date", "series_type", "partial_train", 
                                                                                                                                                    "partial_outcome", "last_event_date", "h", "lambda", "fcast_dates", 
                                                                                                                                                    "backcast", "agg_method"))
  target <- structure(c(228.781223321491, 258.845200617942, 253.476162103, 
                    246.185592129569, 243.64839304623, 247.636223376, 250.203514336736, 
                    251.053984996, 250.802768103388, 264.110173878422, 267.655955958, 
                    275.379487640126, 268.482312306, 251.4985238244, 238.918872115775, 
                    236.454222031913, 226.110797117, 200.815467689814, 187.790184181212, 
                    184.278143557363, 178.066762992358, 173.991435850538, 173.793078297, 
                    181.823956808, 184.871672847048, 172.414225617212, 167.451619876624, 
                    160.542260830909, 149.061056232, 135.501144205822, 142.250382321967, 
                    155.647988794557, 151.079134906987, 149.500747896137, 145.09225663056, 
                    142.002354343, 130.294972875805, 127.430500493, 127.985684328176, 
                    137.928902503703, 142.275705125463, 154.561132657, 175.960088246, 
                    182.800977665175, 186.362359493449, 192.596390146546, 192.977112443729, 
                    194.218777040535, 189.801180413434, 183.570696165926, 193.023877137, 
                    208.965396010707, 216.560823968, 219.651298940275, 224.165476189761, 
                    214.783717057641, 204.210702628176, 184.399570862264, 179.928492898455, 
                    191.141189148282), .Tsp = c(1, 5.91666666666667, 12), class = "ts")
  ff <- create_forecast(target, "RW", pr)
  expect_equal(ff$model, "RW")
  
})


# Sample requests ---------------------------------------------------------

context("Sample requests")

test_that("Basic examples run",  {
  expect_warning(r_basil_ts("tests/io/example1.json"), 
                 "no seasonal differencing is selected")
  expect_warning(r_basil_ts("tests/io/example2.json"), 
                 "no seasonal differencing is selected")
})

test_that("Malformatted separations are rejected", {
  expect_error(r_basil_ts("tests/io/example5.json"), 
               "Separations contain ambiguous decimal separator")
})

test_that("Historical data with integer dates is rejected", {
  expect_error(r_basil_ts("tests/io/example3.json"),
               "character string is not in a standard unambiguous format")
})

test_that("Request with data past IFP end date is rejected", {
  expect_error(r_basil_ts("tests/io/example4.json"), 
               "exceed question end date")
})

test_that("Request with aggregated data for fixed period IFP is rejected", {
  expect_error(r_basil_ts("tests/io/ex_938_input.json"), 
               "Send daily data in the request, not aggregated data. Question is over 100 day periods.")
})
