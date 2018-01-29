
source("basil-ts/ts-forecast.R")

library("testthat")

parse_question_period("August 2017")
parse_question_period("between 15 October 2017 and 31 October 2017")
parse_question_period("between 1 October 2017 and 15 October 2017")
parse_question_period("between 1 October 2017 and 31 October 2017")
parse_question_period("On 27 December 2017")

bb_seq_period("2017-08-01", length.out = 5, bb_period("month"))
bb_seq_period("2017-08-01", length.out = 5, bb_period("day"))
bb_seq_period("2017-08-01", length.out = 5, bb_period("fixed", 7))
bb_seq_period("2017-08-01", length.out = 5, bb_period("fixed", 14))


bb_diff_period("2017-08-01", "2018-05-01", pd = bb_period("month"))

x <- as.Date(c("2017-01-01", "2017-01-15", "2017-01-16", "2017-02-01"))
cast_date(x, "day")
cast_date(x, "week")
cast_date(x, "half-month")
cast_date(x, "month")

samples <- dir("test/requests", pattern = "*\\.json", full.names = TRUE)
samples <- lapply(samples, jsonlite::fromJSON)
questions <- unique(sapply(samples, function(x) x$metadata$title))

# Guessing type of data series
guessed_type <- lapply(samples, function(x) {
  c(guess_series_type(x$ts[, 2], x$metadata$title), x$metadata$title)
})
guessed_type

# Enforce value constraints
x <- c(-1, 0, 1, 2)
expect_equal(enforce_series_type(x, "continuous"), x)
expect_equal(enforce_series_type(x, "count"), c(0, 0, 1, 2))
expect_equal(enforce_series_type(x, "binary"), c(0, 0, 1, 1))



# Sample requests from backcast IFPs --------------------------------------

x <- main("test/requests/example1.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("test/responses/example1.json")
x$options
x$model_info

# 65: brent oil price; fin. daily; question daily
x <- main("test/requests/ifp65a.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("test/responses/ifp65a.json")
x$options
x$model_info

# 12
x <- main("test/requests/ifp12a.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("test/responses/ifp12a.json")
x$options
x$model_info

# 5: ACLED
x <- main("test/requests/ifp5a.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("test/responses/ifp5a.json")
x$options
x$model_info

# 5b: data ends whole month before question
x <- main("test/requests/ifp5b.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("test/responses/ifp5b.json")
x$options
x$model_info

# 5c: partial data for month prior to question
x <- main("test/requests/ifp5c.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("test/responses/ifp5c.json")
x$options
x$model_info

# 5d: partial data for question answer
x <- main("test/requests/ifp5d.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("test/responses/ifp5d.json")
x$options
x$model_info

# 68: earthquakes, half-month question
x <- main("test/requests/ifp68a.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("test/responses/ifp68a.json")
x$options
x$model_info

