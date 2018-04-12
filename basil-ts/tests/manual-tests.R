
source("basil-ts/ts-forecast.R")

library("testthat")


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



# Aggregating dates to index date -----------------------------------------

df <- data.frame(
  input_dates = structure(
    c(17569, 17570, 17571, 17572, 17573, 17574, 17575, 
      17576, 17577, 17578, 17579, 17580, 17581, 17582, 17583, 17584, 
      17585, 17586, 17587, 17588, 17589, 17590, 17591, 17592, 17593, 
      17594, 17595, 17596, 17597, 17598, 17599, 17600, 17601, 17602, 
      17603, 17604, 17605, 17606, 17607, 17608, 17609, 17610, 17611, 
      17612, 17613, 17614, 17615, 17616, 17617, 17618), class = "Date")
)

index_dates <- structure(c(17506, 17527, 17548, 17569, 17590, 17611), class = "Date")
pd_days <- 21

df$int_d <- as.integer(df$input_dates)
df$d1 <- as.integer(df$input_dates) %/% pd_days * pd_days
df$d2 <- df$d1 - shift
df$d3 <- (as.integer(df$input_dates) + shift) %/% pd_days * pd_days - shift

shift <- pd_days - (as.integer(index_dates[1]) %% pd_days)
df$index_date <- ((as.integer(target$date) %/% pd_days)*pd_days) - shift
target$index_date <- as.Date(target$index_date, origin = "1970-01-01")






# Forecast updating -------------------------------------------------------

# from 1208
mu_f <- 1457223
se_f <- 84764.33

