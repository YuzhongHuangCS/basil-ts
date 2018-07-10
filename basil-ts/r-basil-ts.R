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
source("basil-ts/data.R")


# Helpers -----------------------------------------------------------------



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
  
  # Parse request input file
  out <- parse_request(request)
  target <- out$target
  pr     <- out$parsed_request
  
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
      pr$data_updated_to <- drop_after
    } else {
      # let the data_updated_to parser infer last date based on data_period
      pr$data_updated_to <- parse_data_updated_to(NA, pr$aggregated_data, pr$data_period, target)
    }
  }
  
  # Check data end does not exceed question end
  if (pr$data_updated_to >= pr$question_period$dates[2]) {
    stop(sprintf(
      "Payload data (to '%s') exceed question end date ('%s'), there is nothing to forecast.",
      pr$data_updated_to, pr$question_period$date[2]))
  }
  
  # Input data processing: aggregation if needed, partial data handling
  df     <- list(target = target, data_updated_to = pr$data_updated_to)
  out    <- process_data(df, pr)
  target <- out$target
  pr     <- out$parsed_request
  
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
