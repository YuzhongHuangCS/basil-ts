

library("forecast")
library("lubridate")
library("jsonlite")
library("stringr")

#' Segmentize forecast density
#' 
#' Given answer categories and forecast::forecast object, determine forecast
#' probabilities for each category.
#' 
#' @param fc forecast object
#' @param cp cutpoints
category_forecasts <- function(fc, cp) {
  # for forecasts with h > 1, assume last is the one we want
  # determine forecast density SE
  mu <- tail(as.numeric(fc$mean), 1)
  ul <- tail(fc$upper[, 1], 1)
  
  # BoxCox is lambda was given
  if (!is.null(fc$model$lambda) & is.null(fc$model$constant)) {
    lambda <- fc$model$lambda
    cp <- BoxCox(cp, lambda)
    mu <- BoxCox(mu, lambda)
    ul <- BoxCox(ul, lambda)
  }
  
  # re-calculate forecast density SE
  level <- 80
  if (!grepl("80", colnames(fc$upper)[1])) {
    stop("Only works if first level is 80%, fix function")
  }
  se <- as.numeric((ul - mu) / qnorm(.5 * (1 + level/100)))
  
  cumprob <- c(0, pnorm(cp, mean = mu, sd = se), 1)
  catp <- diff(cumprob)
  
  # for binary questions, return only "yes" answer
  if (length(catp)==2) {
    catp <- catp[2]
  }
  catp
}


parse_options <- function(x) {
  cutpoints <- x %>% 
    str_extract_all(., "[-0-9\\.,]+") %>%
    unlist() %>%
    str_replace_all(., ",", "") %>%
    as.numeric() %>%
    unique()
  if (length(cutpoints)==0) cutpoints <- 1
  
  list(cutpoints = cutpoints, options = x)
}

identify_date_units <- function(x) {
  NULL
}

identify_question_time_period <- function(x) {
  dates <- str_extract_all(x, "([0-9]{0,2}[A-Za-z ]+[0-9]{4})")
  if (length(dates)==1 & str_detect(dates, "[0-9]{1,2}[A-Za-z ]+[0-9]{4}")) {
    period <- "day"
  } else if (lenght(dates)==1 & str_detect(dates, "[A-Za-z ]+[0-9{4}")) {
    period <- "month"
  }
  period
}


fh <- "basil-ts/request.json"
#fh = "basil-ts/in-data.json"
#fh = "test/example1.json"

request <- jsonlite::fromJSON(fh)

options      <- parse_options(request$metadata$options$name)
forecast_for <- str_extract_all(request$metadata$title, "([0-9]{0,2}[A-Za-z ]+[0-9]{4})")[[1]] %>%
  as.Date(., format = "%d %b %Y")
data_dates <- as.Date(request$ts[, 1])

# How many time periods do I need to forecast ahead?
h = forecast_for - max(data_dates)

target <- ts(
  data  = as.numeric(request$ts[, 2])
  # start = c(as.integer(substr(min(in_data$date), 1, 4)), 
  #           as.integer(substr(min(in_data$date), 6, 7))),
  # frequency = 12
)

mdl   <- auto.arima(target)
fcast <- forecast(mdl, h = h)
catfcast <- category_forecasts(fcast, options$cutpoints)

result <- list(
  raw_forecasts = data.frame(
    date = NA,
    l95  = fcast$lower[, 2],
    l80  = fcast$lower[, 1],
    mean = fcast$mean,
    u80  = fcast$upper[, 1],
    u95  = fcast$upper[, 2]
  ),
  options = data.frame(
    name = options$options,
    phat = catfcast
  ),
  metadata = list(
    hfcId = request$metadata$hfcId,
    forecastCreatedAt = now()
  )
)

toJSON(result, "columns") %>% writeLines("basil-ts/forecast.json")
#write.csv(result, file = "basil-ts/forecast.csv", row.names = FALSE)

