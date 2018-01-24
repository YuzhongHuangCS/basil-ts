
suppressPackageStartupMessages({
  library("methods")
  library("forecast")
  library("lubridate")
  library("jsonlite")
  library("stringr")
})

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

halfmonth <- function(x) {
  stopifnot(is.Date(x))
  paste(substr(x, 6, 7), ifelse(substr(x, 9, 10) < 16, "01", "16"), sep = "-")
}

lbl <- paste(sprintf("%02d", rep(1:12, 1, each = 2)), c("01", "16"), sep = "-")
N_DAYS_IN_HALFMONTHS <- as.Date(c(sprintf("2017-%s", lbl), "2018-01-01")) %>% diff() %>% as.integer()
names(N_DAYS_IN_HALFMONTHS) <- lbl

days_in_halfmonth <- function(x) {
  halfmonth_x <- halfmonth(x)
  n_days <- N_DAYS_IN_HALFMONTHS[halfmonth_x]
  n_days[halfmonth_x == "02-16" & leap_year(x)] <- 14L
  n_days
}

days_in_period <- function(x, period) {
  if (period=="month") {
    days_in_month(x)
  } else if (period=="halfmonth") {
    days_in_halfmonth(x)
  } else {
    stop("Unrecognized period")
  }
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
  if (all(tdiff < 7)) {
    # why not ==1? because finanical time series skip weekends and holidays...
    period <- "day"
  } else if (all(tdiff==7)) {
    period <- "week"
  } else if (all(tdiff %in% 13:17)) {
    period <- "halfmonth"
  } else if (all(tdiff %in% 28:31)) {
    period <- "month"
  } else {
    stop("Unrecognized period")
  }
  period
}

parse_question_period <- function(x) {
  dates <- str_extract_all(x, "([0-9 ]{0,3}[A-Za-z]+[ ]{1}[0-9]{4})")[[1]]
  if (length(dates)==1 & all(str_detect(dates, "[0-9]{1,2}[A-Za-z ]+[0-9]{4}"))) {
    period <- "day"
    date   <- as.Date(dates, format = "%d %B %Y")
  } else if (length(dates)==1 & all(str_detect(dates, "[A-Za-z]+[ ]{1}[0-9]{4}"))) {
    period <- "month"
    date   <- as.Date(sprintf("1 %s", dates), format = "%d %B %Y")
  } else if (length(dates)==2) {
    dates <- as.Date(dates, format = "%d %B %Y")
    if ( (day(dates[1])==1 & day(dates[2])==15) | (day(dates[1]) %in% c(15, 16) & day(dates[2]) %in% 28:31)) {
      period <- "halfmonth"
      date   <- dates[1]
      if (day(date)==15) day(date) <- 16L
    } else if (day(dates[1])==1 & day(dates[2]) %in% c(28:31)) {
      period <- "month"
      date   <- dates[1]
    } else {
      period <- "custom"
      date   <- dates
    }
  } else {
    stop("Unrecognized question period")
  }
  list(period = period, date = date)
}

cast_date <- function(x, period) {
  stopifnot(is.Date(x))
  if (period %in% c("day", "custom")) {
    out <- x
  } else if (period=="week") {
    stop("not implemented")
  } else if (period=="halfmonth") {
    out <- `day<-`(x, ifelse(day(x) < 16, 1, 16))
  } else if (period=="month") {
    out <- `day<-`(x, 1)
  } else {
    stop("Unrecognized period")
  }
  out
}

make_dates <- function(data_end, h, pd) {
  if (pd %in% c("day", "custom")) {
    x <- data_end + 1:h
  } else if (pd=="week") {
    x <- data_end + 7*1:h
  } else if (pd=="halfmonth") {
    if (day(data_end)==16) {
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

guess_series_type <- function(x, question) {
  stopifnot(length(question)==1)
  xvals <- unique(x)
  distinctvals <- length(xvals)
  min0  <- min(xvals)==0
  max1  <- max(xvals)==1
  q_count <- str_detect(tolower(question), "(how many)(ACLED)(atrocities)")
  # default
  out <- "continuous"
  if ((min0 & !max1) | q_count)      out <- "count"
  if (min0 & max1 & distinctvals==2) out <- "binary"
  out
}

enforce_series_type <- function(x, type) {
  if (type=="continuous") {
    x <- x
  } else if (type=="count") {
    x[x < 0] <- 0
  } else if (type=="binary") {
    x[x < 0] <- 0
    x[x > 1] <- 1
  }
  x
}

skewness <- function(x) {
  n <- length(x)
  (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
}


main <- function(fh = "basil-ts/request.json") {
  #fh = "test/requests/ifp68a.json"
  #fh = "basil-ts/basil-ts/request.json"
  
  request <- jsonlite::fromJSON(fh)
  #unlink(fh)
  # missing file makes error more obvious in Flask
  unlink("basil-ts/forecast.json")
  
  options         <- parse_options(request$metadata$options[, 1])
  question_period <- parse_question_period(request$metadata$title)
  
  target <- data.frame(
    date  = as.Date(request$ts[, 1]),
    value = as.numeric(request$ts[, 2])
  )
  data_period     <- parse_data_period(target$date)
  series_type     <- guess_series_type(target$value, request$metadata$title)
  target$normdate <- cast_date(target$date, question_period$period)
  
  # Aggregate
  # TODO fill in missing time periods when aggregating; month and halfmonth data only, i think
  partial    <- target[target$date >= question_period$date[1], ]
  target_agg <- target[target$date < question_period$date[1], ]
  # check for partial training data
  if (question_period$period %in% c("month", "halfmonth") & data_period=="day" & series_type=="count") {
    target_agg$rows <- 1
    target_agg <- aggregate(target_agg[, c("value", "rows")], by = list(target_agg$normdate), FUN = sum) %>%
      setNames(c("normdate", "value", "rows"))
    target_agg$days  <- days_in_period(target_agg$normdate, question_period$period)
    # don't extrapolate if less than half period; drop in that case
    target_agg <- target_agg[target_agg$rows / target_agg$days > .5, ]
    # extrapolate for partials with more than x
    target_agg$value <- as.integer(target_agg$days / target_agg$rows * target_agg$value)
  } else {
    target_agg <- aggregate(target_agg[, c("value")], by = list(target_agg$normdate), FUN = sum) %>%
      setNames(c("normdate", "value"))
  }
  
  # How many time periods do I need to forecast ahead?
  h  <- as.integer(question_period$date[1] - max(target_agg$normdate))
  pd <- question_period$period
  if (pd=="halfmonth") {
    h <- round(h / 15.18)
  } else if (pd=="month") {
    h <- round(h / 30.35)
  }
  
  # TODO set start based on period
  fr <- switch(question_period$period,
               "month" = 12,
               "halfmonth" = 24,
               "week" = 52.17857,
               "day" = 364.25)
  target_ts <- ts(
    data = as.numeric(target_agg$value),
    # start = c(as.integer(substr(min(in_data$date), 1, 4)), 
    #           as.integer(substr(min(in_data$date), 6, 7))),
    frequency = fr
  )
  
  # Estimate model and forecast
  # TODO enforce forecast value constraints
  # TODO update forecast with partial info
  lambda   <- NULL
  skew     <- NULL
  if (series_type %in% c("count", "continuous")) {
    skew <- skewness(as.vector(target_ts))
    if (skew > 2) lambda <- .5
  }
  mdl      <- auto.arima(target_ts, lambda = lambda)
  fcast    <- forecast(mdl, h = h)
  catfcast <- category_forecasts(fcast, options$cutpoints)
  
  data_end <- max(target_agg$normdate)
  pd <- question_period$period
  fcast_dates <- make_dates(data_end, h, pd)
  
  # Fit statistics
  # check out rwf/naive and MASE (https://www.otexts.org/fpp/2/5)
  # TODO get some basic heuristics for what is good
  resid <- mdl$x - mdl$fitted
  rmse  <- sqrt(mean(resid^2))
  rmse_mean <- sqrt(mean((mdl$x - mean(mdl$x))^2))
  resid_rwf <- mdl$x - naive(mdl$x)$fitted
  rmse_rwf  <- sqrt(mean(resid_rwf^2, na.rm = TRUE))
  # mase doesn't work when any baseline forecast is 0, bc /0
  #mase <- mean(abs(resid / resid_rwf), na.rm = TRUE)
  
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
      forecastCreatedAt = lubridate::now()
    ),
    model_info = list(
      data_period = data_period,
      question_period = question_period$period,
      series_type = series_type,
      h = h,
      skew = skew,
      lambda = lambda,
      rmse = rmse,
      rmse_mean = rmse_mean,
      rmse_rwf  = rmse_rwf
    )
  )
  
  toJSON(result, "columns", POSIXt = "ISO8601") %>% writeLines("basil-ts/forecast.json")
  #write.csv(result, file = "basil-ts/forecast.csv", row.names = FALSE)
  invisible(result)
}

main()
