

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

parse_data_period <- function(x) {
  tdiff <- as.integer(unique(diff(x)))
  if (all(tdiff==1)) {
    period <- "day"
  } else if (all(tdiff==7)) {
    period <- "week"
  } else if (all(tdiff %in% 13:17)) {
    period <- "half-month"
  } else if (all(tdiff %in% 28:31)) {
    period <- "month"
  } else {
    stop("Unrecognized period")
  }
  period
}

parse_question_period <- function(x) {
  dates <- str_extract_all(x, "([0-9]{0,2}[A-Za-z ]+[0-9]{4})")[[1]]
  if (length(dates)==1 & all(str_detect(dates, "[0-9]{1,2}[A-Za-z ]+[0-9]{4}"))) {
    period <- "day"
    date   <- as.Date(dates, format = "%d %B %Y")
  } else if (length(dates)==1 & all(str_detect(dates, "[A-Za-z]+[ ]{1}[0-9]{4}"))) {
    period <- "month"
    date   <- as.Date(sprintf("1 %s", dates), format = "%d %B %Y")
  } else if (length(dates)==2) {
    period <- "half-month"
    date   <- as.Date(dates[1], format = "%d %B %Y")
    date   <- `day<-`(date, ifelse(day(date) < 15, 1, 15))
  } else {
    stop("no clue")
  }
  list(period = period, date = date)
}

cast_date <- function(x, period) {
  stopifnot(is.Date(x))
  if (period=="day") {
    out <- x
  } else if (period=="week") {
    stop("not implemented")
  } else if (period=="half-month") {
    out <- `day<-`(x, ifelse(day(x) < 15, 1, 16))
  } else if (period=="month") {
    out <- `day<-`(x, 1)
  } else {
    stop("Unrecognized period")
  }
  out
}

make_dates <- function(data_end, h, pd) {
  if (pd=="day") {
    x <- data_end + 1:h
  } else if (pd=="week") {
    x <- data_end + 7*1:h
  } else if (pd=="half-month") {
    if (day(start)==16) {
      mm <- rep(1:ceiling(h/2), each = 2)[1:h]
      x <- data_end %m+% months(mm)
      x <- `day<-`(x, rep_len(c(1, 16), length = h))
    } else {
      mm <- c(0, rep(1:floor(h/2), each = 2))[1:h]
      x <- data_end %m+% months(mm)
      x <- `day<-`(x, rep_len(c(16, 1), length = h))
    }
  } else if (pd=="month") {
    x <- data_end %m+% months(1:h)
  }
  x
}


fh <- "basil-ts/request.json"
#fh = "test/requests/example1.json"
fh = "basil-ts/test/example1.json"

main <- function(fh = "basil-ts/request.json") {
  request <- jsonlite::fromJSON(fh)
  
  options         <- parse_options(request$metadata$options$name)
  question_period <- parse_question_period(request$metadata$title)
  
  target <- data.frame(
    date  = as.Date(request$ts[, 1]),
    value = as.numeric(request$ts[, 2])
  )
  data_period     <- parse_data_period(target$date)
  target$normdate <- cast_date(target$date, question_period$period)
  
  # Aggregate
  # TODO fill in missing time periods when aggregating
  partial    <- target[target$date >= question_period$date, ]
  target_agg <- target[target$date < question_period$date, ]
  target_agg <- aggregate(target$value, by = list(target$normdate), FUN = sum) %>%
    setNames(c("normdate", "value"))
  
  # How many time periods do I need to forecast ahead?
  h  <- as.integer(question_period$date - max(target_agg$normdate))
  pd <- question_period$period
  if (pd=="half-month") {
    h <- round(h / 15.18)
  } else if (pd=="month") {
    h <- round(h / 30.35)
  }
  
  # TODO set start and frequency based on period
  target_ts <- ts(
    data = as.numeric(target_agg$value)
    # start = c(as.integer(substr(min(in_data$date), 1, 4)), 
    #           as.integer(substr(min(in_data$date), 6, 7))),
    # frequency = 12
  )
  
  # TODO set lambda as neede
  # TODO enforce forecast value constraints
  # TODO update forecast with partial info
  mdl      <- auto.arima(target_ts)
  fcast    <- forecast(mdl, h = h)
  catfcast <- category_forecasts(fcast, options$cutpoints)
  
  data_end <- max(target_agg$normdate)
  pd <- question_period$period
  fcast_dates <- make_dates(data_end, h, pd)
  
  result <- list(
    raw_forecasts = data.frame(
      date = fcast_dates,
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
  invisible(result)
}


