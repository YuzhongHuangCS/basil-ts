
context("Model and forecast functions")

test_that("Model functions run without error", {
  target1019 <- readRDS("../target_1019.rds")
  target1019 <- ts(target1019$value, frequency = 12)
  
  expect_error(auto_arima_forecast(target1019, NULL, 12), NA)
  expect_error(constant_mean_forecast(target1019, NULL, 12), NA)
  expect_error(rw_forecast(target1019, NULL, 12), NA)
  expect_error(rw_drift_forecast(target1019, NULL, 12), NA)
  expect_error(ets_forecast(target1019, NULL, 12), NA)
  expect_error(arithmetic_rw_forecast(target1019, NULL, 12), NA)
  expect_error(geometric_rw_forecast(target1019, NULL, 12), NA)
})