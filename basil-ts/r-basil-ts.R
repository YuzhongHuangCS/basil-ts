#
#   Main r-basil-ts script
#
#   Can be run as a script with Rscript, or sourced to import the function
#

suppressPackageStartupMessages({
  library("methods")
  library("forecast")
  library("lubridate")
  library("jsonlite")
  library("stringr")
  library("truncnorm")
})

source("basil-ts/models.R")
source("basil-ts/parse-requests.R")
source("basil-ts/time-period.R")
source("basil-ts/forecast.R")


# Data helpers ------------------------------------------------------------

#' Identify whether aggregated data based on IFP question title
id_aggregated_data <- function(ifp_name) {
  title <- tolower(ifp_name)
  patterns <- c("(acled|icews|earthquakes|sea ice|hacking|boko haram|how many united nations security)")
  aggdata <- str_detect(title, patterns)
  aggdata
}

#' Heuristic for determining how data should be aggregated over time
#' 
#' 
determine_aggregation_method <- function(series_type, ifp_name) {
  qmax <- str_detect(ifp_name, "maximum")
  qmin <- str_detect(ifp_name, "minimum")
  agg <- "mean"
  if (series_type=="count") {
    agg <- "sum"
  }
  if (series_type=="continuous" & qmax) {
    agg <- "max"
  } else if (series_type=="continuous" & qmin) {
    agg <- "min"
  }
  agg
}

#' Aggregate daily data
#' 
#' To fixed format required for question period.
aggregate_data <- function(df, question_period, fun) {
  df$index_date <- norm_fixed_period(df$date, 
                                     question_period$period$days,
                                     question_period$dates[1])
  
  new_df <- aggregate(df[, c("value")], by = list(df$index_date), FUN = get(fun))
  colnames(new_df) <- c("date", "value")
  
  new_df
}

#' Shift index dates to match question period
#' 
#' This is a fallback in case data cannot be aggregated within the app, but
#' pre-aggregated data index dates are incompatible with question
shift_index_dates <- function(df, question_period) {
  NULL
}

index_dates_are_misaligned <- function(data, question_period) {
  # Check if dates are aligned correctly; if h is not an interger -> problem
  h1 <- bb_diff_period(max(data$date), question_period$dates[1], question_period$period)
  h2 <- bb_diff_period(min(data$date), question_period$dates[1], question_period$period)
  if ((h1 %% 1)!=0 | (h2 %% 1)!=0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

validate_data <- function(data, data_period, question_period, ifp_name) {
  # Check that data are aggregated correctly
  if (!bb_equal_period(data_period$period, question_period$period)) {
    mssg <- paste(
      sprintf("Request data appear to not be aggregated correctly"),
      sprintf("  Data dates: ...%s", paste(tail(data$date), collapse = ", ")),
      sprintf("  Parsed data period: '%s'", ifelse(data_period$period$period=="fixed", 
                                                   paste0("fixed, %s days", data_period$period$days),
                                                   data_period$period$period)),
      sprintf("  Question title: %s", ifp_name),
      sprintf("  Parsed question period: '%s'", ifelse(question_period$period$period=="fixed", 
                                                       paste0("fixed, %s days", question_period$period$days),
                                                       question_period$period$period)),
      sep = "\n"
    )
    stop(mssg)
  }
  
  if (index_dates_are_misaligned(data, question_period)) {
    stop(sprintf("Historical data in request appear to not be indexed with correct dates. The question period starts %s and dates like [..., %s] are expected in the historical data, but instead they have [..., %s].", 
                 as.character(question_period$dates[1]),
                 paste0(as.character(question_period$dates[1] - 5:0*question_period$period$days), collapse = ", "),
                 paste0(as.character(tail(data$date)), collapse = ", ")
    ))
  }
  
  invisible(TRUE)
}

#' Calculate time periods per year
determine_ts_frequency <- function(x) {
  x <- aggregate(x[, c("date")], by = list(year = lubridate::year(x$date)), FUN = length)$x
  x <- head(x, length(x)-1) %>% tail(length(.)-1)
  fr <- ifelse(length(x)==0, 1, mean(x))
  fr
}

lambda_heuristic <- function(ts, series_type) {
  lambda <- NULL
  if (series_type %in% c("count")) {
    skew <- skewness(as.vector(ts))
    any0 <- any(ts==0)
    if (skew > 2 && !any0) lambda <- 0
    if (skew > 2 && any0)  lambda <- .5
  } 
  lambda
}

#' Calculate skewness
#' 
skewness <- function(x) {
  n <- length(x)
  (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
}

#' Clean model forecast
#' 
#' Take out data we don't need to return in response
clean_model <- function(x) {
  x$est_model <- NULL
  x$fcast <- NULL
  x
}

# Main script -------------------------------------------------------------

#' Basil-TS time-series forecaster for SAGE
#' 
r_basil_ts <- function(fh = NULL) {
  args <- commandArgs(trailingOnly=TRUE)
  test <- FALSE
  backcast <- FALSE
  drop_after <- as.Date("9999-12-31")
  quick <- FALSE
  #fh = "tests/io/andy_input_1145.json"
  
  if (length(args) > 0) {
    # normal use via Rscript
    request_id   <- args[1]
    backcast     <- ifelse(args[2]=="True", TRUE, backcast)
    drop_after   <- as.Date(args[3])
    quick        <- ifelse(args[4]=="True", TRUE, backcast)
    fh <- paste0("basil-ts/request-", request_id, ".json")
  } else if (length(args)==0 && exists("fh") && is.null(fh)) {
    # function is being sourced
    return(TRUE)
  } else {
    # function is used from R
    request_id <- "test"
    test <- TRUE
  }
  
  request <- jsonlite::fromJSON(fh)
  validate_input_file_format(request)
  validate_seps(request$payload$separations$values)
  
  # missing file makes error more obvious in Flask
  if (!test) unlink(fh)
  # also remove any other request files that may stick around if this function
  # fails with error
  on.exit(file.remove(dir("basil-ts", pattern = "request-", full.names = TRUE)))
  
  target <- data.frame(
    date  = as.Date(request$payload$historical_data$ts[, 1]),
    value = as.numeric(request$payload$historical_data$ts[, 2])
  )
  
  # Initialize list parsed request data
  pr <- list()
  pr$ifp_name        <- request$ifp$name
  pr$binary_ifp      <- request$ifp$`binary?`
  pr$question_period <- parse_question_period(pr$ifp_name)
  pr$data_period     <- parse_data_period(target$date)
  pr$aggregated_data <- id_aggregated_data(pr$ifp_name)
  if (!is.null(request$payload$`data-updated-to`)) {
    req_data_updated_to <- request$payload$`data-updated-to`
  } else {
    req_data_updated_to <- request$payload$`last-event-date`
  }
  pr$last_date       <- parse_last_date(request$payload$`last-event-date`, 
                                        pr$aggregated_data, pr$data_period,
                                        target)
  pr$series_type     <- guess_series_type(target$value, pr$ifp_name)
  pr$separations     <- parse_separations(request$payload$separations, 
                                          pr$series_type, pr$ifp_name)
  pr$agg_method      <- determine_aggregation_method(pr$series_type, pr$ifp_name)
  
  # Check aggregation was not done for fixed period questions
  if (pr$question_period$period$period=="fixed" & 
      # weeks are ok, only need to catch like 100 day fixed periods
      pr$question_period$period$days!="7" & 
      pr$data_period$period$period!="day") {
    stop(sprintf(
      "Send daily data in the request, not aggregated data. Question is over %s day periods.",
      pr$question_period$period$days))
  }
  
  # Backcasting 
  if (backcast) {
    # drop-after defaults to 9999-12-31, change the default to day before 
    # question period starts
    if (drop_after==as.Date("9999-12-31")) drop_after <- pr$question_period$dates[1] - 1
    # if drop after was user supplied, make sure it does not exceed question end date
    if (drop_after > pr$question_period$dates[2]) {
      stop("Drop after argument exceeds question end date")
    }
    
    target <- target[target$date < (drop_after + 1), ]
    if (pr$aggregated_data==TRUE) {
      pr$last_date <- drop_after
    } else {
      # let the last_date parser infer last date based on data_period
      pr$last_date <- parse_last_date(NA, pr$aggregated_data, pr$data_period, target)
    }
  }
  
  # Check data end does not exceed question end
  if (pr$last_date >= pr$question_period$dates[2]) {
    stop(sprintf(
      "Payload data (to '%s') exceed question end date ('%s'), there is nothing to forecast.",
      pr$last_date, pr$question_period$date[2]))
  }
  
  # Do aggregation if neccessary
  pr$was_data_aggregated <- FALSE
  #were_dates_shifted  <- FALSE
  if (pr$data_period$period$period=="day" & pr$question_period$period$period=="fixed") {
    target      <- aggregate_data(target, pr$question_period, pr$agg_method)
    pr$data_period <- parse_data_period(target$date)
    pr$was_data_aggregated <- TRUE
    pr$aggregated_data <- TRUE
    # for backcasting, pr$last_date is still correct because we had daily data and it was set to max(target)
  }
  
  # Check that data are aggregated correctly and dates are aligned
  validate_data(target, pr$data_period, pr$question_period, pr$ifp_name)
  
  
  # Partial data handling
  
  pr$partial_outcome <- FALSE
  pr$partial_train   <- ""
  # retain original target tail for plotting
  pr$target_tail <- tail(target, 1)
  pr$yobs <- NA
  pr$yn <- NA
  
  if (pr$aggregated_data==TRUE & pr$data_period$period$period!="day") {
    
    gt_train_end      <- pr$last_date >= max(target$date)
    gt_question_start <- pr$last_date >= pr$question_period$dates[1]
    
    days_in_period <- find_days_in_period(max(target$date), pr$data_period$period)
    days_avail <- (pr$last_date - max(target$date)) %>% `+`(1) %>% as.integer()
    
    actually_partial <- days_avail < days_in_period
    
    what_to_do <- "nothing"
    if (gt_train_end & !gt_question_start & actually_partial) {
      what_to_do <- "drop or extrapolate"
    }
    if (gt_train_end & gt_question_start) {
      what_to_do <- "update forecast"
    }
    
    if (what_to_do=="nothing") {
      pr$partial_train <- "no"  
    }
    
    if (what_to_do=="drop or extrapolate") {
      # partial info in last training data period
      # if more than half of period, extrapolate, else discard that period
      # only use if > half of period days have data; because danger of extrapolating
      if (days_avail > (days_in_period/2)) {
        pr$partial_train <- "used"
        if (pr$agg_method=="sum") {
          target$value[nrow(target)] <- target$value[nrow(target)] * days_in_period / days_avail
        } else {
          target$value[nrow(target)] <- target$value[nrow(target)]
        }
      } else {
        pr$partial_train <- "discarded"
        target <- target[-nrow(target), ]
        #pr$target_tail <- tail(target, 1)  # connect to discarded data anyways, not previous point
      }
    }
    
    if (what_to_do=="update forecast") {
      # we have partial outcome info
      
      pr$partial_outcome <- TRUE
      pr$yobs <- target$value[nrow(target)]
      pr$yn   <- days_avail
      target <- target[-nrow(target), ]
      # Update the target tail used for plotting as well to exclude partial 
      # outcome data that is dropped
      pr$target_tail <- tail(target, 1)
    }
  }
  
  # Determine periods per year for ts frequency
  fr <- as.integer(determine_ts_frequency(target))
  
  # Cut down training data if needed to speed up model estimation
  upperN <- 200
  if (pr$data_period$period$period=="day") {
    upperN <- 120
  } else if (pr$data_period$period$period=="month") {
    upperN <- 12*5
  } else if (pr$data_period$period$period=="fixed") {
    upperN <- 120
  }
  if (nrow(target) > upperN) {
    target <- tail(target, upperN)
  }
  
  pr$target <- target
  
  target_ts <- ts(
    data = as.numeric(target$value),
    frequency = fr
  )
  
  # How many time periods do I need to forecast ahead?
  pr$h <- bb_diff_period(max(target$date), pr$question_period$dates[1], pr$question_period$period)
  # What will those dates be?
  pr$fcast_dates <- bb_seq_period(max(target$date), length.out = pr$h + 1, pr$question_period$period) %>% tail(pr$h)
  
  # Estimate model and forecast
  pr$lambda <- lambda_heuristic(target_ts, pr$series_type)
  
  # Identify which models to run
  model_types <- c("auto ARIMA", "ETS", "RW", "geometric RW", "mean") # names(model_dictionary)
  if (quick) model_types <- "auto ARIMA"
  forecasts   <- lapply(model_types, create_forecast, 
                        ts = target_ts, parsed_request = pr)
  names(forecasts) <- model_types
  
  # The estimated model and fcast object are helpers for stuff in this level,
  # can take out now, don't need actually in response.
  forecasts <- lapply(forecasts, clean_model)
  names(forecasts) <- model_types
  
  # Fit statistics
  rmse_mean <- sqrt(mean(residuals(Arima(target_ts, order = c(0, 0, 0), lambda = pr$lambda))^2))
  rmse_rwf  <- sqrt(mean(residuals(Arima(target_ts, order = c(0, 1, 0), lambda = pr$lambda))^2, na.rm = TRUE))
  
  internal_info <- pr
  # Legacy info, maybe take out in future
  #internal_info$forecast_created_at <- lubridate::now()
  internal_info$rmse_mean <- rmse_mean
  internal_info$rmse_rwf  <- rmse_rwf
  internal_info$backcast  <- backcast
  
  # Put ARIMA forecast at top-level; also copied in forecasts below
  # in the future maybe this will be selected by AIC/BIC/whatever
  response                     <- forecasts[["ARIMA"]]
  response[["parsed_request"]] <- internal_info
  response[["forecasts"]]      <- forecasts
  
  call <- list(
    backcast = backcast,
    drop_after = drop_after,
    quick = quick
  )
  response[["call_options"]] <- call
  
  if (test) {
    return(invisible(response))
  } else {
    out_fh <- paste0("basil-ts/forecast-", request_id, ".json")
    toJSON(response, "columns", POSIXt = "ISO8601", pretty = TRUE) %>% writeLines(out_fh)
    return(invisible(NULL))
  }
}

r_basil_ts()
