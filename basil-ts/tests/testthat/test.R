

test_that("Question dates and periods are correctly parsed", {
  x <- parse_question_period("in August 2017")
  expect_equal(x$period, "month")
  expect_equal(x$date, as.Date("2017-08-01"))
  
  x <- parse_question_period("on 27 December 2017")
  expect_equal(x$period, "day")
  expect_equal(x$date, as.Date("2017-12-27"))
  
  x <- parse_question_period("between 1 December 2017 and 31 December 2017")
  expect_equal(x$period, "month")
  expect_equal(x$date, as.Date("2017-12-01"))
  
  x <- parse_question_period("between 1 December 2017 and 1 January 2018")
  expect_equal(x$period, "custom")
  expect_equal(x$date, as.Date(c("2017-12-01", "2018-01-01")))
  
  x <- parse_question_period("between 1 December 2017 and 15 December 2017")
  expect_equal(x$period, "halfmonth")
  expect_equal(x$date, as.Date("2017-12-01"))
  
  x <- parse_question_period("between 1 December 2017 and 16 December 2017")
  expect_equal(x$period, "custom")
  expect_equal(x$date, as.Date(c("2017-12-01", "2017-12-16")))
  
  x <- parse_question_period("between 15 December 2017 and 31 December 2017")
  expect_equal(x$period, "halfmonth")
  expect_equal(x$date, as.Date("2017-12-16"))
  
  x <- parse_question_period("between 16 December 2017 and 31 December 2017")
  expect_equal(x$period, "halfmonth")
  expect_equal(x$date, as.Date("2017-12-16"))
})


test_that("Sample requests do not throw error", {
  expect_error(main("test/requests/example1.json"), NA)
  expect_error(main("test/requests/ifp65a.json"), NA)
  expect_error(main("test/requests/ifp12a.json"), NA)
  
  expect_error(main("test/requests/ifp5a.json"), NA)
  expect_error(main("test/requests/ifp5b.json"), NA)
  expect_error(main("test/requests/ifp5c.json"), NA)
  expect_error(main("test/requests/ifp5d.json"), NA)
  
  expect_error(main("test/requests/ifp68a.json"), NA)
})
