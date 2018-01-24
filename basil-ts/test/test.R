
source("basil-ts/ts-forecast.R")

library("testthat")

df <- rbind(
  c("August 2017", "month"),
  c("between 15 October 2017 and 31 October 2017", "half-month"),
  # week
  c("27 December 2017", "day")
  # custom for unusual periods? just # of days?
) %>% as.data.frame() %>% setNames(c("string", "answer"))

lapply(df$string, parse_question_period)

make_dates(as.Date("2017-01-01"), 5, "day")
make_dates(as.Date("2017-01-01"), 5, "week")
make_dates(as.Date("2017-01-01"), 5, "half-month")
make_dates(as.Date("2017-01-16"), 5, "half-month")
make_dates(as.Date("2017-01-01"), 5, "month")

x <- as.Date(c("2017-01-01", "2017-01-15", "2017-01-16", "2017-02-01"))
cast_date(x, "day")
cast_date(x, "week")
cast_date(x, "half-month")
cast_date(x, "month")

samples <- dir("/test/requests", pattern = "*\\.json", full.names = TRUE)
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

# TODO save a couple of example responses for ISI

x <- main("/test/requests/example1.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("/test/responses/example1.json")
x$options
x$model_info

# 65: brent oil price; fin. daily; question daily
x <- main("/test/requests/ifp65a.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("/test/responses/ifp65a.json")
x$options
x$model_info

# 12
x <- main("/test/requests/ifp12a.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("/test/responses/ifp12a.json")
x$options
x$model_info

# 5: ACLED
x <- main("/test/requests/ifp5a.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("/test/responses/ifp5a.json")
x$options
x$model_info

# 68: earthquakes, half-month question
x <- main("/test/requests/ifp68a.json")
x %>% toJSON(., "columns", POSIXt = "ISO8601") %>% writeLines("/test/responses/ifp68a.json")
x$options
x$model_info

