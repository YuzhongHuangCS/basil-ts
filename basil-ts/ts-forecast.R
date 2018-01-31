
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

bb_period <- function(x, days = NA) {
  stopifnot(x %in% c("month", "day", "fixed"))
  list(period = x, days = days)
}

bb_diff_period <- function(d1, d2, pd) {
  d1 <- as.Date(d1)
  d2 <- as.Date(d2)
  days <- as.integer(d2 - d1)
  if (pd$period=="month") {
    out <- round(days / (365.25 / 12))
  } else if (pd$period=="day") {
    out <- days
  } else if (pd$period=="fixed") {
    out <- days / pd$days
  } else {
    stop("Unrecognized period/not implemented")
  }
  attr(out, "bb_period") <- pd
  out
}

bb_seq_period <- function(date, length.out, pd) {
  date <- as.Date(date)
  if (pd$period=="month") {
    x <- date %m+% months(0:(length.out - 1))
  } else if (pd$period=="day") {
    x <- date + 0:(length.out-1)
  } else if (pd$period=="halfmonth") {
    if (day(date)==16) {
      mm <- rep(0:ceiling((length.out-1)/2), each = 2)[1:h]
      x <- date %m+% months(mm)
      x <- `day<-`(x, rep_len(c(1, 16), length = h))
    } else {
      mm <- c(0, rep(1:floor(h/2), each = 2))[1:h]
      x <- date %m+% months(mm)
      x <- `day<-`(x, rep_len(c(16, 1), length = h))
    }
  } else if (pd$period=="fixed") {
    x <- date + 0:(length.out-1) * pd$days
  } else {
    stop("Unrecognized period/not implemented")
  }
  x
}

bb_equal_period <- function(pd1, pd2) {
  t1 <- pd1$period==pd2$period
  t2 <- TRUE
  if (pd1$period=="fixed") t2 <- pd1$days==pd2$days
  all(t1, t2)
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

parse_raw_options <- function(x) {
  cutpoints <- x %>% 
    str_extract_all(., "[-0-9\\.,]+") %>%
    unlist() %>%
    str_replace_all(., ",", "") %>%
    as.numeric() %>%
    unique()
  if (length(cutpoints)==0) cutpoints <- 1
  
  list(cutpoints = cutpoints, options = x)
}

#' Parse parsed option cutpoints
#' 
#' @example 
#' seps <- c("<1229.85", "1229.85-1268.42", "1268.42-1301.63", "1301.63-1340.19", ">1340.19")
#' parse_separations(seps)
parse_separations <- function(x) {
  cutpoints <- x %>%
    str_extract_all("[0-9\\.]+") %>%
    unlist() %>%
    as.numeric() %>%
    unique()
  if (length(cutpoints)==0) cutpoints <- 1
  list(cutpoints = cutpoints, separations = x)
}

parse_data_period <- function(x) {
  tdiff <- as.integer(unique(diff(x)))
  if (length(tdiff)==1) {
    if (tdiff[1]==1) {
      pd <- bb_period("day")
      days   <- NA
    } else {
      pd <- bb_period("fixed", tdiff[1])
    }
  } else if (all(tdiff < 7)) {
    # why not ==1? because finanical time series skip weekends and holidays...
    pd <- bb_period("day")
  } else if (all(tdiff %in% 13:17)) {
    pd <- bb_period("halfmonth")
  } else if (all(lubridate::day(x)==1)) {
    pd <- bb_period("month")
  } else {
    stop("Unrecognized period")
  }
  list(period = pd)
}

parse_question_period <- function(x) {
  dates <- str_extract_all(x, "([0-9 ]{0,3}[A-Za-z]+[ ]{1}[0-9]{4})")[[1]]
  if (length(dates)==1 & all(str_detect(dates, "[0-9]{1,2}[A-Za-z ]+[0-9]{4}"))) {
    pd <- bb_period("day")
    date   <- rep(as.Date(dates, format = "%d %B %Y"), 2)
  } else if (length(dates)==1 & all(str_detect(dates, "[A-Za-z]+[ ]{1}[0-9]{4}"))) {
    pd <- bb_period("month")
    date   <- as.Date(sprintf("1 %s", dates), format = "%d %B %Y")
  } else if (length(dates)==2) {
    dates <- as.Date(dates, format = "%d %B %Y")
    # if ( (day(dates[1])==1 & day(dates[2])==15) | (day(dates[1]) %in% c(15, 16) & day(dates[2]) %in% 28:31)) {
    #   period <- "halfmonth"
    #   date   <- dates[1]
    #   if (day(date)==15) day(date) <- 16L
    #} else 
    if (day(dates[1])==1 & day(dates[2]) %in% c(28:31)) {
      pd <- bb_period("month")
      date   <- dates[1]
    } else {
      pd <- bb_period("fixed", days = as.integer(dates[2]-dates[1]))
      date   <- dates
    }
  } else {
    stop("Unrecognized question period")
  }
  list(period = pd, date = date)
}

cast_date <- function(x, period) {
  stopifnot(is.Date(x))
  if (period %in% c("day", "fixed")) {
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

guess_series_type <- function(x, question) {
  stopifnot(length(question)==1)
  xvals <- unique(x)
  distinctvals <- length(xvals)
  min0  <- min(xvals)==0
  max1  <- max(xvals)==1
  q_count <- str_detect(tolower(question), "(how many)(ACLED)(atrocities)")
  q_cont  <- all(str_detect(tolower(question), c("what", "price")))
  # default
  out <- "continuous"
  if (q_cont) out <- "continuous"
  if ((min0 & !max1) | q_count)      out <- "count"
  if (min0 & max1 & distinctvals==2 & !q_cont) out <- "binary"
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


main <- function() {
  args <- commandArgs(trailingOnly=TRUE)
  request_id <- args[1]
  fh <- paste0("basil-ts/request-", request_id, ".json")
  
  #fh = "test/requests/example1.json"
  #fh = "basil-ts/basil-ts/request.json"
  
  request <- jsonlite::fromJSON(fh)
  # missing file makes error more obvious in Flask
  unlink(fh)
  
  ifp_name <- request$ifp$name
  
  options         <- parse_separations(request$payload$separations$values)
  question_period <- parse_question_period(ifp_name)
  
  target <- data.frame(
    date  = as.Date(request$payload$historical_data$ts[, 1]),
    value = as.numeric(request$payload$historical_data$ts[, 2])
  )
  data_period     <- parse_data_period(target$date)
  series_type     <- guess_series_type(target$value, ifp_name)
  
  # Check if need to aggregate
  if (bb_equal_period(data_period$period, question_period$period)) {
    target$normdate <- target$date
    target_agg <- target
  } else {
    # Aggregate
    target$normdate <- cast_date(target$date, question_period$period)
    
    # Aggregate
    # TODO fill in missing time periods when aggregating; month and halfmonth data only, i think
    partial    <- target[target$date >= question_period$date[1], ]
    target_agg <- target[target$date < question_period$date[1], ]
    # check for partial training data
    if (question_period$period %in% c("month", "halfmonth") & data_period$period=="day" & series_type=="count") {
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
  }
  
  # Cut down training data if needed to speed up model estimation
  if (nrow(target_agg > 2000)) {
    target_agg <- tail(target_agg, 2000)
  }
  
  # How many time periods do I need to forecast ahead?
  h <- bb_diff_period(max(target_agg$normdate), question_period$date[1], question_period$period)
  # What will those dates be?
  fcast_dates <- bb_seq_period(max(target_agg$date), length.out = h + 1, question_period$period) %>% tail(h)
  
  # TODO set start based on period
  fr <- switch(question_period$period$period,
               "month" = 12,
               "halfmonth" = 24,
               "week" = 52.17857,
               "day" = 364.25,
               "fixed" = 364.25 / question_period$days)
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
  usable <- as.integer(rmse <= rmse_mean & rmse <= rmse_rwf)
  
  
  
  result <- list(
    ts_colnames = c("date", "actual_forecast", "lower_bound_95_percent", "upper_bound_95_percent"),
    ts = data.frame(
      date = fcast_dates,
      mean = fcast$mean %>% enforce_series_type(series_type),
      l95  = fcast$lower[, 2] %>% enforce_series_type(series_type),
      u95  = fcast$upper[, 2] %>% enforce_series_type(series_type)
    ) %>% as.matrix(),
    option_probabilities = catfcast,
    forecast_is_usable = usable, 
    forecast_created_at = lubridate::now(),
    model_info = list(
      data_period = data_period$period,
      question_period = question_period$period,
      question_date = question_period$date,
      series_type = series_type,
      h = h,
      skew = skew,
      lambda = lambda,
      rmse = rmse,
      rmse_mean = rmse_mean,
      rmse_rwf  = rmse_rwf
    )
  )
  
  out_fh <- paste0("basil-ts/forecast-", request_id, ".json")
  toJSON(result, "columns", POSIXt = "ISO8601", pretty = TRUE) %>% writeLines(out_fh)
  invisible(result)
}

main()
