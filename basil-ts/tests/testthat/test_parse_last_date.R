
context("parse_last_date")

test_that("It works with monthly data", {
  target <- data.frame(
    date = seq(as.Date("2018-01-01"), as.Date("2018-07-01"), by = "month"),
    value = rnorm(7)
  )
  
  pr = list(
    data_period = list(
      period = list(
        period = "month",
        days = NULL
      )
    ),
    `aggregated-data` = FALSE,
    `last-event-date` = as.Date("2018-07-31")
  )
  
  pr$`aggregated-data` = FALSE
  expect_equal(
    parse_last_date(pr$`last-event-date`, pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-31"))
  expect_equal(
    parse_last_date(NA, pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-31"))
  expect_equal(
    parse_last_date(as.Date("2018-07-15"), pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-31"))
  
  # aggregated monthly data
  pr$`aggregated-data` = TRUE
  expect_equal(
    parse_last_date(pr$`last-event-date`, pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-31"))
  expect_error(
    parse_last_date(NA, pr$`aggregated-data`, pr$data_period, target),
    "last_event_date cannot be missing")
  expect_equal(
    parse_last_date(as.Date("2018-07-15"), pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-15"))
})


test_that("It works with daily data", {
  # daily data
  target <- data.frame(
    date = seq(as.Date("2018-01-01"), as.Date("2018-07-01"), by = "day"),
    value = rnorm(182)
  )
  
  pr = list(
    data_period = list(
      period = list(
        period = "day",
        days = 1
      )
    ),
    `aggregated-data` = FALSE,
    `last-event-date` = "2018-07-01"
  )
  
  pr$`aggregated-data` = FALSE
  expect_equal(
    parse_last_date(pr$`last-event-date`, pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-01"))
  expect_equal(
    parse_last_date(NA, pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-01"))
  expect_equal(
    parse_last_date(as.Date("2018-07-01"), pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-01"))
  
  pr$`aggregated-data` = TRUE
  expect_equal(
    parse_last_date(pr$`last-event-date`, pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-01"))
  expect_error(
    parse_last_date(NA, pr$`aggregated-data`, pr$data_period, target),
    "last_event_date cannot be missing for aggregated data")
  expect_equal(
    parse_last_date(as.Date("2018-07-01"), pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-01"))
  
})

test_that("It works with fixed period data", {
  # fixed data
  target <- data.frame(
    date = as.Date("2018-07-01") - rev(c(0, 100, 200)),
    value = rnorm(3)
  )
  pr <- list(
    data_period = list(
      period = list(
        period = "fixed",
        days = 100
      )
    ),
    `aggregated-data` = FALSE,
    `last-event-date` = as.Date("2018-07-01")
  )
  
  pr$`aggregated-data` = FALSE
  expect_equal(
    parse_last_date(pr$`last-event-date`, pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-10-08"))
  expect_equal(
    parse_last_date(NA, pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-10-08"))
  expect_equal(
    parse_last_date(as.Date("2018-07-15"), pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-10-08"))

  pr$`aggregated-data` = TRUE
  expect_equal(
    parse_last_date(pr$`last-event-date`, pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-01"))
  expect_error(
    parse_last_date(NA, pr$`aggregated-data`, pr$data_period, target),
    "last_event_date cannot be missing for aggregated data")
  expect_equal(
    parse_last_date(as.Date("2018-07-15"), pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-15"))
})

test_that("last_event_date outside implied range throws an error", {
  # daily data
  target <- data.frame(
    date = seq(as.Date("2018-01-01"), as.Date("2018-07-01"), by = "day"),
    value = rnorm(182)
  )
  
  pr = list(
    data_period = list(
      period = list(
        period = "day",
        days = 1
      )
    ),
    `aggregated-data` = TRUE,
    `last-event-date` = "2018-07-01"
  )
  
  expect_equal(
    parse_last_date(as.Date("2018-07-01"), pr$`aggregated-data`, pr$data_period, target),
    as.Date("2018-07-01"))
  expect_error(
    parse_last_date(as.Date("2018-06-15"), pr$`aggregated-data`, pr$data_period, target),
    "last_event_date seems to be wrong, outside the date range"
  )
  expect_error(
    parse_last_date(as.Date("2018-07-15"), pr$`aggregated-data`, pr$data_period, target),
    "last_event_date seems to be wrong, outside the date range"
  )
})
