
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
  
  # BoxCox is lambda was given
  if (!is.null(fc$model$lambda) & is.null(fc$model$constant)) {
    lambda <- fc$model$lambda
    cp <- BoxCox(cp, lambda)
    mu <- BoxCox(mu, lambda)
  }
  
  se <- forecast_se(fc)
  
  # need to sort cutpoints otherwise this is screwed up; reorder at end
  orig_order <- order(cp)
  cp_sorted <- cp[orig_order]
  
  cumprob <- c(0, pnorm(cp_sorted, mean = mu, sd = se), 1)
  catp    <- diff(cumprob)[orig_order]
  catp
}

#' Update forecast 
#' 
#' Update forecast with partial outcome information
update_forecast <- function(x, yobs, yn, fcast_date, data_period) {
  if (length(x$mean) > 1) {
    stop("Can only update 1 forecast")
  }
  
  N <- ifelse(data_period$period$period=="month", 
              fcast_date %>% lubridate::days_in_month(),
              data_period$period$days)
  
  bc_transform <- !is.null(x$model$lambda) & is.null(x$model$constant)
  if (bc_transform) {
    lambda <- x$model$lambda
  } else {
    lambda <- NULL
  }
  
  new_pars <- update_agg_norm(x$mean, x$se, yobs, yn, N, lambda)
  
  # CI calculation has to be done on transformed scale
  level <- colnames(x$upper) %>% gsub("%", "", .) %>% as.numeric()
  nint <- length(level)
  lower <- x$lower
  upper <- x$upper
  for (i in 1:nint) {
    qq <- qnorm(0.5 * (1 + level[i] / 100))
    lower[, i] <- new_pars["mean"] - qq * new_pars["se"]
    upper[, i] <- new_pars["mean"] + qq * new_pars["se"]
  }
  
  if (bc_transform) {
    new_pars["mean"] <- InvBoxCox(new_pars["mean"], lambda)
    upper[, ] <- apply(upper, 2, InvBoxCox, lambda)
    lower[, ] <- apply(lower, 2, InvBoxCox, lambda)
  } 
  
  x$mean[]   <- new_pars[1]
  x$se     <- new_pars[2]
  x$lower[, ] <- lower
  x$upper[, ] <- upper
  
  # Enforce minimum already observed count
  x$lower[, ] <- apply(x$lower, 2, pmax, yobs)
  x
}

#' Update normal forecast
#' 
#' Update normal density with partial observed outcomes under assumption that it
#' is a sum of smaller normal densities. 
#' 
update_agg_norm <- function(mean, se, yobs, yn, N, lambda = NULL) {
  mean_t <- mean/N
  se_t   <- sqrt(se^2/N)
  n <- yn
  if (is.null(lambda)) {
    mean_star <- as.numeric(yobs + (N-n)*mean_t)
  } else {
    mean_star <- as.numeric(BoxCox(yobs + (N-n)*mean_t, lambda))
  }
  se_star <- sqrt((N-n)*se_t^2)
  c(mean = mean_star, se = se_star)
}

#' Calculate SE in forecast object
#' 
#' Calculate implicity SE used for normal density prediction intervals.
forecast_se <- function(x) {
  mu <- tail(as.numeric(x$mean), 1)
  ul <- tail(x$upper[, "95%"], 1)
  
  # BoxCox is lambda was given
  if (!is.null(x$model$lambda) & is.null(x$model$constant)) {
    lambda <- x$model$lambda
    mu <- BoxCox(mu, lambda)
    ul <- BoxCox(ul, lambda)
  } else if (!is.null(x$model$lambda) & !is.null(x$model$constant)) {
    stop("Don't know how to handle BoxCox with constant")
  }
  
  # re-calculate forecast density SE
  level <- 95
  se <- as.numeric((ul - mu) / qnorm(.5 * (1 + level/100)))
  se
}

#' Create period object
#' 
#' To handle weird time periods that are not months or days use a fixed option, which 
#' is any period with a constant day difference in dates
bb_period <- function(x, days = NA) {
  stopifnot(x %in% c("month", "day", "fixed"))
  list(period = x, days = days)
}

#' Number of periods between two periods
#' 
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
  attr(out, "period") <- pd
  out
}

#' Sequence of periods
#' 
#' This is like seq.Date, but handles fixed day difference periods (seq.Date only handles weeks like this)
#' 
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

#' Normalize dates to a arbitrary fixed time period
#' 
#' Given an arbitrary fixed time period in days and a reference date, normalize
#' input dates. 
#' 
#' @details 
#' The ref_date argument will be used as the first day of the fixed time periods.
#' For example, to normalize dates to the corresponding Monday of a week, use
#' ref_date = "2018-02-05" (a Monday) and days = 7. To do ISO style weeks, which 
#' use Thursdays as the index date, take this output and afterwards shift to 
#' Thursdays by adding 3. 
#' 
#' @examples
#' feb2018 <- seq(from=as.Date("2018-02-01"), to=as.Date("2018-02-28"), by = "day")
#' norm <- norm_fixed_period(feb2018, days = 7, ref_date = as.Date("2018-02-05"))
#' data.frame(input = feb2018, output = norm, iso_style = norm + 3)
norm_fixed_period <- function(x, days, ref_date) {
  x_int <- as.integer(x)
  x_ref <- as.integer(ref_date)
  shift <- x_ref %% days
  norm_int <- floor((x_int - shift)/days) * days + shift
  norm <- as.Date(norm_int, origin = "1970-01-01")
  norm
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
    # fixed difference in dates
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

#' Parse date period in question
#' 
parse_question_period <- function(x) {
  dates <- str_extract_all(x, "([0-9 ]{0,3}[A-Za-z]+[ ]{1}[0-9]{4})")[[1]]
  if (length(dates)==1 & all(str_detect(dates, "[0-9]{1,2}[A-Za-z ]+[0-9]{4}"))) {
    # matches like 27 December 2017
    pd <- bb_period("day")
    dates <- rep(as.Date(dates, format = "%d %B %Y"), 2)
  } else if (length(dates)==1 & all(str_detect(dates, "[A-Za-z]+[ ]{1}[0-9]{4}"))) {
    # matches like December 2017
    pd <- bb_period("month")
    dates  <- as.Date(sprintf("1 %s", dates), format = "%d %B %Y")
    dates  <- c(dates, dates + days_in_month(dates) - 1)
  } else if (length(dates)==2) {
    dates <- as.Date(dates, format = "%d %B %Y")
    # if ( (day(dates[1])==1 & day(dates[2])==15) | (day(dates[1]) %in% c(15, 16) & day(dates[2]) %in% 28:31)) {
    #   period <- "halfmonth"
    #   date   <- dates[1]
    #   if (day(date)==15) day(date) <- 16L
    #} else 
    if (day(dates[1])==1 & day(dates[2])==days_in_month(dates[2])) {
      # first and last date of month
      pd <- bb_period("month")
      date   <- dates[1]
    } else {
      # add +1 to days because in question, dates are inclusive, i.e. 1 to 6 December is a whole week
      pd <- bb_period("fixed", days = as.integer(dates[2]-dates[1] + 1))
      date   <- dates
    }
  } else {
    stop("Unrecognized question period")
  }
  list(period = pd, dates = dates %>% setNames(NULL))
}



guess_series_type <- function(x, question) {
  stopifnot(length(question)==1)
  xvals <- unique(x)
  distinctvals <- length(xvals)
  min0  <- min(xvals)==0
  max1  <- max(xvals)==1
  q_count <- any(str_detect(tolower(question), c("how many", "how much", "ACLED", "atrocities")))
  q_cont  <- all(str_detect(tolower(question), c("what", "price")))
  q_binary <- str_detect(tolower(question), "any")
  # default
  out <- "continuous"
  if (q_cont) out <- "continuous"
  if (min0 | q_count)      out <- "count"
  if (min0 & max1 & distinctvals==2 & !q_count) out <- "binary"
  out
}

#' Enforce value constraints
#' 
#' Enforce value constraints for different types of series, e.g. count
#' 
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

#' Calculate skewness
#' 
skewness <- function(x) {
  n <- length(x)
  (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
}

#' Check for comma in seps
validate_seps <- function(seps) {
  # check for ambiguous decimal separators
  comma <- any(str_detect(seps, "\\."))
  period <- any(str_detect(seps, ","))
  if (comma & period) {
    msg <- sprintf("Separations contain ambiguous decimal separator, both commas and periods detected\n  Values: [%s]",
                   paste(seps, collapse = "; "))
    stop(msg)
  } 
  # check for mis-parsed seps
  if (sum(str_detect(seps, "^[<>][0-9\\.]+$")) > 2) {
    msg <- sprintf("Separations appear to be mis-parsed, multiple '<X' or '>X'\n  Values: [%s]",
                   paste(seps, collapse = "; "))
    stop(msg)
  }
  invisible(TRUE)
}


binary_seps <- function(x) {
  # "Will there be any...?"
  if (all(str_detect(x, c("^Will", "any")))) {
    return(1)
  }
  # "Will there be more than...?"
  c1 <- str_detect(x, c("^Will", "(more|less) than [0-9]+"))
  c2 <- str_count(x, "(more|less)")  # to eliminate "more than...and less than..."
  if (all(c1) && c2==1) {
    y <- str_extract(x, "than [0-9,\\.]+")
    y <- str_replace(y, "than ", "")
    return(as.numeric(y))
  }
  stop("Unable to identify implied question separations for binary question")
}
 

create_forecast <- function(ts, model = "ARIMA", lambda, h, series_type,
                            partial_outcome = FALSE, yobs = NULL, yn = NULL, 
                            fcast_dates = NULL, data_period = NULL, 
                            binary_ifp = NULL, options = NULL) {
  result <- tryCatch({
    if (model=="ARIMA") {
      mdl <- auto.arima(ts, lambda = lambda)
    } else if (model=="ETS") {
      mdl <- ets(ts, lambda = lambda)
    } else if (model=="RWF") {
      mdl <- rwf(ts, lambda = lambda, h = h)
    } else if (model=="geometric RWF") {
      mdl <- rwf(ts, lambda = 0, h = h)
    }
    
    fcast    <- forecast(mdl, h = h)
    fcast$se <- forecast_se(fcast)
    if (partial_outcome & series_type=="count") {
      fcast <- update_forecast(fcast, yobs, yn, fcast_dates, data_period)
    } 
    
    # Fit statistics
    # check out rwf/naive and MASE (https://www.otexts.org/fpp/2/5)
    resid <- mdl$x - mdl$fitted
    rmse  <- sqrt(mean(resid^2))
    rmse_mean <- sqrt(mean((mdl$x - mean(mdl$x))^2))
    resid_rwf <- mdl$x - naive(mdl$x)$fitted
    rmse_rwf  <- sqrt(mean(resid_rwf^2, na.rm = TRUE))
    # mase doesn't work when any baseline forecast is 0, bc /0
    #mase <- mean(abs(resid / resid_rwf), na.rm = TRUE)
    usable <- as.integer(rmse <= rmse_mean & rmse <= rmse_rwf)
    
    result <- list(
      model = model,
      ts_colnames = c("date", "actual_forecast", "lower_bound_95_percent", "upper_bound_95_percent"),
      ts = data.frame(
        date = fcast_dates,
        mean = fcast$mean %>% enforce_series_type(series_type),
        l95  = fcast$lower[, "95%"] %>% enforce_series_type(series_type),
        u95  = fcast$upper[, "95%"] %>% enforce_series_type(series_type)
      ) %>% as.matrix(),
      forecast_is_usable = usable, 
      forecast_created_at = lubridate::now(),
      internal = list(
        mdl_string = capture.output(print(mdl)) %>% paste0(collapse="\n"),
        rmse = rmse
      ),
      est_model = mdl,
      fcast = fcast
    )
    rownames(result$ts) <- NULL
    
    # Get the answer option probabilities 
    catfcast <- category_forecasts(result$fcast, options$cutpoints)
    
    # for binary IFPs, category_forecast will return P for "no"/"yes" options
    # if the question is "any" or "more", then we want the second option only??
    if (binary_ifp) {
      pos <- ifelse(str_detect(ifp_name, "(any|more)"), 2L, 1L)
      catfcast         <- catfcast[pos]
    }
    result$option_probabilities <- catfcast
    
    result
  }, error = function(e) {
    result = list(
      model = model,
      estimated = FALSE,
      r_error_message = e$message
    )
    result
  })
  result
}

validate_data <- function(data, data_period, question_period) {
  # Check that data are aggregated correctly
  if (!bb_equal_period(data_period$period, question_period$period)) {
    mssg <- paste(
      sprintf("Request data appear to not be aggregated correctly"),
      sprintf("  Data dates: ...%s", paste(tail(data$date), collapse = ", ")),
      sprintf("  Parsed data period: '%s'", ifelse(question_period$period$period=="fixed", 
                                                   paste0("fixed, %s days", question_period$period$days),
                                                   question_period$period$period)),
      sprintf("  Question title: %s", ifp_name),
      sprintf("  Parsed question period: '%s'", ifelse(question_period$period$period=="fixed", 
                                                       paste0("fixed, %s days", question_period$period$days),
                                                       question_period$period$period)),
      sep = "\n"
    )
    stop(mssg)
  }
  
  # Check if dates are aligned correctly; if h is not an interger -> problem
  h <- bb_diff_period(max(data$date), question_period$dates[1], question_period$period)
  if (!h%%1==0) stop(sprintf("Historical data in request appear to not be indexed with correct dates. The question period starts %s and dates like [..., %s] are expected in the historical data, but instead they have [..., %s].", 
                             as.character(question_period$dates[1]),
                             paste0(as.character(question_period$dates[1] - 5:0*question_period$period$days), collapse = ", "),
                             paste0(as.character(tail(data$date)), collapse = ", ")
  ))
  
  invisible(TRUE)
}


# Main script -------------------------------------------------------------

#' Basil-TS time-series forecaster for SAGE
#' 
r_basil_ts <- function(fh = NULL) {
  args <- commandArgs(trailingOnly=TRUE)
  test <- FALSE
  backcast <- FALSE
  if (length(args) > 0) {
    # normal use via Rscript
    request_id <- args[1]
    backcast <- ifelse(args[2]=="True", TRUE, backcast)
    fh <- paste0("basil-ts/request-", request_id, ".json")
  } else if (length(args)==0 && is.null(fh)) {
    # function is being sourced
    return(TRUE)
  } else {
    # function is used from R
    request_id <- "test"
    test <- TRUE
  }
  
  #fh = "tests/io/andy_input_1055.json"
  
  request <- jsonlite::fromJSON(fh)
  # missing file makes error more obvious in Flask
  if (!test) unlink(fh)
  # also remove any other request files that may stick around if this function
  # fails with error
  on.exit(file.remove(dir("basil-ts", pattern = "request-", full.names = TRUE)))

  # Pull out needed info
  ifp_name   <- request$ifp$name
  binary_ifp <- request$ifp$`binary?`
  if (binary_ifp) {
    seps <- list(values = binary_seps(ifp_name))
  } else {
    seps <- request$payload$separations
  }
  validate_seps(seps$values)
  
  target <- data.frame(
    date  = as.Date(request$payload$historical_data$ts[, 1]),
    value = as.numeric(request$payload$historical_data$ts[, 2])
  )
  last_date <- ifelse(is.null(request$payload$`last-event-date`),
                      max(target$date),
                      request$payload$`last-event-date`)
  last_date <- as.Date(last_date, origin = "1970-01-01")
  if (!is.null(request$payload$`aggregated-data`)) {
    if (request$payload$`aggregated-data`=="month") {
      last_date <- last_date %m+% months(1) - 1
    }
  }
  
  # Parse characteristics
  options         <- parse_separations(seps)
  question_period <- parse_question_period(ifp_name)
  data_period     <- parse_data_period(target$date)
  series_type     <- guess_series_type(target$value, ifp_name)
  
  # Backcasting 
  if (backcast) {
    target <- target[target$date < question_period$dates[1], ]
    last_date <- max(target$date)
  }
  
  # Check data end does not exceed question end
  if (last_date >= question_period$dates[2]) {
    stop(sprintf(
      "Payload data (to '%s') exceed question end date ('%s'), there is nothing to forecast.",
      last_date, question_period$date[2]))
  }
  
  # Do aggregation if neccessary
  was_data_aggregated <- FALSE
  if (data_period$period$period=="day" & question_period$period$period=="fixed") {
    pd_days       <- question_period$period$days
    go_back_steps <- ceiling(as.integer(today() - min(target$date)) / pd_days)
    index_dates   <- question_period$dates[1] - go_back_steps:0 * pd_days
    shift <- pd_days - (as.integer(index_dates[1]) %% pd_days)
    target$index_date <- (as.integer(target$date) + shift) %/% pd_days *pd_days - shift
    target$index_date <- as.Date(target$index_date, origin = "1970-01-01")
    if (!all(target$index_date %in% index_dates)) stop("Problem with index dates in data aggregation section")
    
    # Aggregate data
    if (series_type=="count") {
      new_target <- aggregate(target[, c("value")], by = list(target$index_date), FUN = sum)
      colnames(new_target) <- c("date", "value")
    } else if (series_type=="continuous") {
      new_target <- aggregate(target[, c("value")], by = list(target$index_date), FUN = mean)
      colnames(new_target) <- c("date", "value")
    } else {
      stop("No method for aggregating data")
    }
    
    # update internal data
    target      <- new_target
    data_period <- parse_data_period(target$date)
    was_data_aggregated <- TRUE
  }
  
  # Check that data are aggregated correctly and dates are aligned
  validate_data(target, data_period, question_period)
  
  # Check for partial outcome info
  partial_outcome <- FALSE
  partial_train <- "no"
  if (series_type %in% c("count", "continuous") & data_period$period$period!="day") {
    
    gt_train_end      <- last_date >= max(target$date)
    gt_question_start <- last_date >= question_period$dates[1]
    
    if (gt_train_end & !gt_question_start) {
      # partial info in last training data period
      # if more than half of period, extrapolate, else discard that period
      
      pd_days <- ifelse(data_period$period$period=="month", 
                        target$date %>% max() %>% lubridate::days_in_month(),
                        data_period$period$days)
      avail <- (last_date - max(target$date)) %>% `+`(1) %>% as.integer()
      
      # only use if > half of period days have data; because danger of extrapolating
      if (avail > pd_days/2) {
        partial_train <- "used"
        target$value[nrow(target)] <- target$value[nrow(target)] * pd_days / avail
      } else {
        partial_train <- "discarded"
        target <- target[-nrow(target), ]
      }
      
    } else if (gt_train_end & gt_question_start) {
      # we have partial outcome info
      
      partial_outcome <- TRUE
      yobs <- target$value[nrow(target)]
      yn   <- as.integer(last_date - max(target$date) + 1)
      target <- target[-nrow(target), ]
    } 
  }
  
  # Determine periods per year for ts frequency
  x <- aggregate(target[, c("date")], by = list(year = lubridate::year(target$date)), FUN = length)$x
  x <- head(x, length(x)-1) %>% tail(length(.)-1)
  fr <- ifelse(length(x)==0, 1, mean(x))
  
  # Cut down training data if needed to speed up model estimation
  if (nrow(target > 1500)) {
    target <- tail(target, 1500)
  }
  
  target_ts <- ts(
    data = as.numeric(target$value),
    frequency = fr
  )
  
  # How many time periods do I need to forecast ahead?
  h <- bb_diff_period(max(target$date), question_period$dates[1], question_period$period)
  # What will those dates be?
  fcast_dates <- bb_seq_period(max(target$date), length.out = h + 1, question_period$period) %>% tail(h)
  
  # Estimate model and forecast
  lambda   <- NULL
  skew     <- NULL
  if (series_type %in% c("count")) {
    skew <- skewness(as.vector(target_ts))
    any0 <- any(target_ts==0)
    if (skew > 2 && !any0) lambda <- 0
    if (skew > 2 && any0)  lambda <- .5
  }
  
  # Create the actual forecast
  forecast <- create_forecast(target_ts, "ARIMA", lambda = lambda, h = h, series_type = series_type,
                              partial_outcome = partial_outcome, yobs = yobs, yn = yn, 
                              fcast_dates = fcast_dates, data_period = data_period,
                              binary_ifp = binary_ifp, options = options)
  forecast_ets <- create_forecast(target_ts, "ETS", lambda = lambda, h = h, series_type = series_type,
                                  partial_outcome = partial_outcome, yobs = yobs, yn = yn, 
                                  fcast_dates = fcast_dates, data_period = data_period,
                                  binary_ifp = binary_ifp, options = options)
  forecast_rwf <- create_forecast(target_ts, "RWF", lambda = lambda, h = h, series_type = series_type,
                                  partial_outcome = partial_outcome, yobs = yobs, yn = yn, 
                                  fcast_dates = fcast_dates, data_period = data_period,
                                  binary_ifp = binary_ifp, options = options)
  
  if (sum(target_ts<=0)==0) {
    forecast_geo_rwf <- create_forecast(target_ts, "geometric RWF", lambda = lambda, h = h, series_type = series_type,
                                        partial_outcome = partial_outcome, yobs = yobs, yn = yn, 
                                        fcast_dates = fcast_dates, data_period = data_period,
                                        binary_ifp = binary_ifp, options = options)
  } else {
    forecast_geo_rwf <- list(model = "geometric RWF")
  }

  # Fit statistics
  # check out rwf/naive and MASE (https://www.otexts.org/fpp/2/5)
  rmse_mean <- sqrt(mean((forecast$est_model$x - mean(forecast$est_model$x))^2))
  resid_rwf <- forecast$est_model$x - naive(forecast$est_model$x)$fitted
  rmse_rwf  <- sqrt(mean(resid_rwf^2, na.rm = TRUE))
  # mase doesn't work when any baseline forecast is 0, bc /0
  #mase <- mean(abs(resid / resid_rwf), na.rm = TRUE)
  
  # The estimated model and fcast object are helpers for stuff in this level,
  # can take out now, don't need actually in response.
  forecast$est_model <- NULL
  forecast$fcast <- NULL
  forecast_ets$est_model <- NULL
  forecast_ets$fcast <- NULL
  forecast_rwf$est_model <- NULL
  forecast_rwf$fcast <- NULL
  forecast_geo_rwf$est_model <- NULL
  forecast_geo_rwf$fcast <- NULL
  
  internal_info <- list(
    data_period = data_period$period,
    was_data_aggregated = was_data_aggregated,
    question_period = question_period$period,
    question_date = question_period$date,
    series_type = series_type,
    partial_train = partial_train,
    partial_outcome = partial_outcome,
    h = as.integer(h),
    skew = skew,
    lambda = lambda,
    rmse_mean = rmse_mean,
    rmse_rwf  = rmse_rwf,
    backcast = backcast
  )
  
  response <- forecast
  response[["internal2"]] <- internal_info
  response[["forecasts"]] <- list(forecast, forecast_ets, forecast_rwf, forecast_geo_rwf)
  
  if (!test) {
    out_fh <- paste0("basil-ts/forecast-", request_id, ".json")
    toJSON(response, "columns", POSIXt = "ISO8601", pretty = TRUE) %>% writeLines(out_fh)
  } else {
    invisible(response)
  }
}

r_basil_ts()
