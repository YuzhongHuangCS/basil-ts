
# testthat will set wd to location of this file, but sourcing and loading the
# sample requests depends on being in the top level basil-ts directory; this 
# should get us there. 
setwd("../../..")
if (!file.exists("basil-ts/tests/testthat.R")) stop("Path is wrong, check test.R")

context("Test forecast infrastructure")

test_that("Category forecasts return correct length and order", {
  # check that length is correct
  cp <- c(-Inf, 1221, 1303, 1374, 1456, Inf)
  fc <- readRDS("basil-ts/tests/fcast_1028.rds")
  
  expect_equal(length(category_forecasts(fc, cp)), (length(cp) - 1))
  
  expect_equal(category_forecasts(fc, cp), rev(category_forecasts(fc, rev(cp))))
}) 

test_that("Forecast probabilities are in correct order", {
  pr <- readRDS("basil-ts/tests/parsed_request_1019.rds")
  target <- readRDS("basil-ts/tests/target_1019.rds")
  target <- ts(tail(target$value, 200))
  
  fcast <- create_single_forecast(target, "Auto ARIMA", pr)
  expect_equal(fcast$option_labels, c("<130", "130 - 150", "150 - 170", "170 - 190", ">190"))
  expect_equal(fcast$option_probabilities, c(3.81254250391658e-05, 0.00143372306109369, 0.0217561767463437, 
                                             0.133284495397449, 0.843487479370075),
               tolerance = 1e-7)
  
  pr_rev <- pr
  pr_rev$separations <- lapply(pr_rev$separations, rev)
  
  fcast_rev <- create_single_forecast(target, "Auto ARIMA", pr_rev)
  expect_equal(fcast_rev$option_labels, c(">190", "170 - 190", "150 - 170", "130 - 150", "<130"))
  expect_equal(fcast_rev$option_probabilities, c(0.843487479370075, 0.133284495397449, 0.0217561767463437, 0.00143372306109369, 
                                                 3.81254250391658e-05),
               tolerance = 1e-7)
  
  pr_binary <- pr
  pr_binary$separations <- list(
    cutpoints = c(-Inf, 200, Inf),
    values = c("no", "yes")
  )
  pr_binary$binary_ifp <- TRUE
  fcast_binary <- create_single_forecast(target, "Auto ARIMA", pr_binary)
  expect_equal(fcast_binary$option_probabilities, c(0.3023236, 0.6976764),
               tolerance = 1e-7)
  expect_equal(fcast_binary$option_labels, c("no", "yes"))
  
  pr_binary$separations <- list(
    cutpoints = c(-Inf, 200, Inf),
    values = c("yes", "no")
  )
  fcast_binary <- create_single_forecast(target, "Auto ARIMA", pr_binary)
  expect_equal(fcast_binary$option_probabilities, c(0.6976764, 0.3023236),
               tolerance = 1e-7)
  expect_equal(fcast_binary$option_labels, c("yes", "no"))
})