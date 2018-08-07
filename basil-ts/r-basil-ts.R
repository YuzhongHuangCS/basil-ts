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
  library("xgboost")
  library("tsfeatures")
  library("M4comp2018")
  library("M4metaresults")
  library("M4metalearning")
})

# Find path to self so we can safely source/load dependencies
OWN_PATH <- getSrcDirectory(function(x) {x})
if (length(OWN_PATH)==0) {
  OWN_PATH <- "../basil-ts"
}
if (!file.exists(file.path(OWN_PATH, "models.R"))) {
  OWN_PATH <- "basil-ts"
}
if (!file.exists(file.path(OWN_PATH, "models.R"))) {
  OWN_PATH <- "../basil-ts"
}
if (!file.exists(file.path(OWN_PATH, "models.R"))) {
  OWN_PATH <- "."
}
if (!file.exists(file.path(OWN_PATH, "models.R"))) {
  stop("There is some problem with r-basil-ts OWN_PATH")
}

source(file.path(OWN_PATH, "time-period.R"))
source(file.path(OWN_PATH, "models.R"))
source(file.path(OWN_PATH, "parse-requests.R"))
source(file.path(OWN_PATH, "forecast.R"))
source(file.path(OWN_PATH, "data.R"))


#' Basil-TS time-series forecaster for SAGE
#' 
r_basil_ts <- function(fh = NULL) {
  args <- commandArgs(trailingOnly=TRUE)
  test <- FALSE
  backcast <- FALSE
  drop_after <- as.Date("9999-12-31")
  quick <- TRUE
  #fh = "tests/io/andy_input_1145.json"
  
  if (length(args) > 0) {
    # normal use via Rscript
    request_id   <- args[1]
    backcast     <- ifelse(args[2]=="True", TRUE, backcast)
    drop_after   <- as.Date(args[3])
    quick        <- ifelse(args[4]=="True", TRUE, FALSE)
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
  
  # Parse request input file
  out    <- parse_request(request)
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
    
    # make sure drop after does not exceed data_updated_to in the input file
    if (drop_after > pr$data_updated_to) {
      msg <- sprintf("The input time series data is too short to support the 'drop_after' date you specified.\n- Last data point is for '%s'\n- Input file 'data-updated-to'/'last-event-date' is '%s'\n- Inferred data_udpated_to date is '%s'\n- 'drop_after' is '%s'; the input data should go to this at least.",
                     as.character(tail(target$date, 1)), 
                     as.character(pr$orig_data_updated_to),
                     as.character(pr$data_updated_to),
                     drop_after)
      stop(msg)
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
  
  # Create the forecast(s), potentially for multiple models
  out       <- create_forecasts(target, pr, quick = quick)
  if (!quick) {
    out_rnn   <- create_forecasts(target, pr, rnn = TRUE)
    out$forecasts[["RNN"]] = out_rnn$forecasts[["RNN"]]
  }

  forecasts <- out$forecasts
  pr        <- out$parsed_request
  
  internal_info <- pr
  internal_info$backcast  <- backcast
  
  # Put ARIMA forecast at top-level; also copied in forecasts below
  # in the future maybe this will be selected by AIC/BIC/whatever
  response                     <- forecasts[["Auto ARIMA"]]
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
