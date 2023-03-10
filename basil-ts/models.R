#
#   Functions that estimate models and their forecasts
#

# Meta helpers ------------------------------------------------------------

model_dictionary <- list(
  "Auto ARIMA"    = "auto_arima_forecast",
  "Mean"          = "constant_mean_forecast",
  "ETS"           = "ets_forecast",
  "RW"            = "rw_forecast",
  "RW-DRIFT"      = "rw_drift_forecast",
  "RW-SEAS"       = "rw_seasonal_forecast",
  "Arithmetic RW" = "arithmetic_rw_forecast",
  "Geometric RW"  = "geometric_rw_forecast",
  "NNETAR"        = "nnetar_forecast",
  "TBATS"         = "tbats_forecast",
  "STLM-AR"       = "stlm_ar_forecast",
  "THETA"         = "thetaf_forecast",
  "DS-RW"         = "rw_deseasoned_forecast",
  "DS-SES"        = "ses_deseasoned_forecast",
  "DS-Holt"       = "holt_deseasoned_forecast",
  "DS-Holt-damped" = "damped_deseasoned_forecast",
  "M4-Comb"       = "m4comb_forecast",
  "M4-Meta"       = "m4meta_forecast"
)

#' Get model function based on short name
get_model <- function(short_name, mdict = model_dictionary) {
  if (short_name == "RNN") {
    function_name = "rnn_forecast"
  } else {
    function_name = mdict[[short_name]]
  }
  get(function_name)
}

#' Extract data frame of model description 
extract_model_doc = function(fun_name) {
  doc = eval(functionBody(get(fun_name))[[2L]])
  fun_name = eval(fun_name)
  out <- as_tibble(doc)
  out$function_name = fun_name
  out
}

#' Make a table of descriptions for all models
make_model_list = function() {
  model_funcs = ls(pattern = "[[:alnum:]\\_]+\\_forecast", envir = .GlobalEnv)
  model_funcs = model_funcs[sapply(model_funcs, function(x) is.function(get(x)))]
  
  df <- lapply(model_funcs, extract_model_doc)
  df <- bind_rows(df)
  df <- arrange(df, short_name)
  df
}



# Model definitions -------------------------------------------------------


auto_arima_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "Auto ARIMA",
    long_name = "Seasonal ARIMA model with automatic selection of the ARIMA model form",
    basis_function = "forecast::auto.arima()",
    lambda_heuristic = TRUE,
    notes = "This chooses and estimates a $\\textrm{ARIMA}(p,d,q)(P,D,Q)_m$ model, where $p, d, q$ are the regular ARIMA parameters, $P, D, Q$ are seasonal terms for frequency $m$ time series data. See [Hyndman and Athanasopoulos, 2018, 8.7](https://otexts.org/fpp2/arima-r.html) and [8.9](https://otexts.org/fpp2/seasonal-arima.html)."
  )
  
  model <- forecast::auto.arima(ts, lambda = lambda)
  model$model_string <- forecast:::arima.string(model)
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

constant_mean_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "Mean",
    long_name = "Constant mean model / ARIMA(0,0,0)",
    basis_function = "forecast::Arima(c(0,0,0))",
    lambda_heuristic = TRUE,
    notes = "This model always predicts the mean of the input time series, and the prediction interval is similarly estimated using the input time series variance, although with a standard uncertainty correction for the number of observations."
  )
  
  model <- Arima(ts, c(0, 0, 0), lambda = lambda)
  model$model_string <- "Constant mean"
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

ets_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "ETS",
    long_name = "Exponential smoothing state space model",
    basis_function = "forecast::ets()",
    lambda_heuristic = NA,
    notes = "A exponential smoothing state space model of the form ETS(error, trend, season, damped), where the error, trend, and season components can be additive, multiplicative, or null, and where optionally a damped trend can be used. All components are automatically chosen using AIC. This model subsumes simpler classical smoothing methods like Holt and Holt-Winters filtering. See [Hyndman and Athanasopoulos, 2018, 7.5](https://otexts.org/fpp2/ets.html) for a summary."
  )
  
  if (frequency(ts) > 24) {
    spec = "ZZN"
  } else {
    spec = "ZZZ"
  }
  
  model <- forecast::ets(ts, model = spec, lambda = lambda)
  model$model_string <- model$method
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

rw_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "RW",
    long_name = "Random walk / ARIMA(0,1,0)",
    basis_function = "forecast::rwf()",
    lambda_heuristic = TRUE,
    notes = "Simple random walk, equivalent to a ARIMA(0,1,0) model, with $Y_t = Y_{t-1} + Z_t$, where $Z_t$ is normal iid error."
  )
  
  model <- rwf(ts, h = h, lambda = lambda, drift = FALSE, level = 95)
  model$model_string <- "RW with lambda heuristic"
  
  fcast    <- forecast(model)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

rw_drift_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "RW-DRIFT",
    long_name = "Random walk with drift",
    basis_function = "forecast::rwf(drift = TRUE)",
    lambda_heuristic = TRUE,
    notes = "A random walk with drift, i.e. $Y_t=c + Y_{t-1} + Z_t$, where $c$ is the drift."
  )
  
  model <- rwf(ts, h = h, lambda = lambda, drift = TRUE, level = 95)
  model$model_string <- "RW with drift and lambda heuristic"
  
  fcast    <- forecast(model)
  # fix bug
  colnames(fcast$upper) <- colnames(fcast$lower) <- c("95%")
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

arithmetic_rw_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "Arithmetic RW",
    long_name = "Random walk on raw scale",
    basis_function = "forecast::Arima(c(0,1,0), lambda = NULL)",
    lambda_heuristic = FALSE,
    notes = "Arithmetic random walk, i.e. on the raw, untransformed time series. Point predictions always equal the last observed data point."
  )
  
  model <- Arima(ts, c(0, 1, 0), lambda = NULL)
  model$model_string <- "RW on original scale"
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

geometric_rw_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "Geometric RW",
    long_name = "Random walk on log scale",
    basis_function = "forecast::Arima(c(0,1,0), lambda = 0)",
    lambda_heuristic = FALSE,
    notes = "A geometric random walk, i.e. on the log transformed input time series, and thus more appropriate for series with exponential growth. All values in the input time series must be > 0."
  )
  
  if (!sum(ts <= 0, na.rm = TRUE)==0) {
    stop("Series contains values <= 0, model not estimated.")
  }
  model <- Arima(ts, c(0, 1, 0), lambda = 0)
  model$model_string <- "RW on log scale"
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}


rw_seasonal_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "RW-SEAS",
    long_name = "Seasonal random walk",
    basis_function = "forecast::snaive()",
    lambda_heuristic = TRUE,
    notes = "Seasonal random walk with $Y_t = Y_{t-m} + Z_t$, where $m$ is the seasonal frequency."
  )
  
  model <- snaive(ts, h = h, lambda = lambda, level = 95)
  model$model_string <- "Seasonal RW"
  
  fcast    <- forecast(model)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

m4meta_forecast <- function(ts, lambda, h) {
  # requires R packages: customxgboost, M4comp2018, custom tsfeatures, 
  # M4metalearning, M4metaresults
  doc <- list(
    short_name = "M4-Meta",
    long_name = "M4 Meta ensemble model",
    basis_function = "M4metalearning::forecast_meta_m4",
    lambda_heuristic = FALSE,
    notes = "An ensemble model from the M4 competition. The forecast is based on a linear combination of forecasts from 9 time series models, where the weights are calculated by a xgboost model using characteristics of the input time series as features. Essentially the meta model learns to weigh time series models that work well for a given series more. The models for the mean forecast are Auto ARIMA, ETS, NNETAR, TBATS, STLM-AR, RW-DRIFT, THETAF, RW (NAIVE) and RW-SEAS. The 95% prediction interval is computed only from 3 models, THETAF and the RW models. A total of 42 input series characteristics are used to fit the meta-model.\n\nFor more details, see the note at [http://htmlpreview.github.io/?https://github.com/robjhyndman/M4metalearning/blob/master/docs/M4_methodology.html](http://htmlpreview.github.io/?https://github.com/robjhyndman/M4metalearning/blob/master/docs/M4_methodology.html)"
  )

  model <- list(model_string <- "M4 Meta")
  
  # forecast_meta_M4 has a bug that causes it to fail with h = 1;
  # hack fix by using h + 1 and then taking out in results
  raw_fcast <- forecast_meta_M4(model_M4, ts, h + 1)
  raw_fcast <- lapply(raw_fcast, head, h)

  fcast <- list(
    method = "M4 Metalearning",
    model = NULL,
    level = 95,
    mean = ts(raw_fcast$mean),
    upper = ts(matrix(raw_fcast$upper, ncol = 1, dimnames = list(NULL, "95%"))),
    lower = ts(matrix(raw_fcast$lower, ncol = 1, dimnames = list(NULL, "95%"))),
    x = ts,
    series = "ts",
    fitted = NULL,
    residuals = NULL
  )
  class(fcast) <- "forecast"
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}


nnetar_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "NNETAR",
    long_name = "Autoregressive neural net",
    basis_function = "forecast::nnetar()",
    lambda_heuristic = FALSE,
    notes = "ADD"
  )
  
  lambda <- NULL
  
  model <- forecast::nnetar(ts, lambda = lambda)
  model$model_string <- model$method
  
  fcast    <- forecast(model, h = h, level = 95, PI = TRUE, npaths = 1e3)
  # bug in forecast, names is "Series 1" not expected "95%"
  colnames(fcast$upper) <- colnames(fcast$lower) <- "95%"
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

tbats_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "TBATS",
    long_name = "Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components",
    basis_function = "forecast::tbats()",
    lambda_heuristic = FALSE,
    notes = "Specialized for time series with complex seasonal patterns. See [De Livera et al., 2011](https://doi.org/10.1198/jasa.2011.tm09771)."
  )
  
  model <- forecast::tbats(ts)
  model$model_string <- as.character(model)
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

stlm_ar_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "STLM-AR",
    long_name = "Seasonal and Trend Decomposition using Loess with AR modeling of the seasonally adjusted series",
    basis_function = "forecast::stlm(modelfunction = stats::ar)",
    lambda_heuristic = FALSE,
    notes = "ADD"
  )
  
  lambda <- NULL
  
  model <- forecast::stlm(ts, lambda = lambda, modelfunction = stats::ar)
  model$model_string <- "STLM-AR"
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

thetaf_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "THETAF",
    long_name = "Theta forecasting method",
    basis_function = "forecast::thetaf()",
    lambda_heuristic = FALSE,
    notes = "Assimakopoulos and Nikolopoulos (2000) theta forecasting model."
  )
  
  model <- forecast::thetaf(ts, h = h, level = 95)
  model$model_string <- "Theta model"
  
  fcast    <- forecast(model, h = h, level = 95)
  # fix bug in forecast
  colnames(fcast$upper) <- colnames(fcast$lower) <- "95%"
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

# Models on de-seasoned data ----------------------------------------------

# from https://github.com/M4Competition/M4-methods/blob/master/Benchmarks%20and%20Evaluation.R
SeasonalityTest <- function(input, ppy){
  #Used to determine whether a time series is seasonal
  tcrit <- 1.645
  if (length(input)<3*ppy){
    test_seasonal <- FALSE
  }else{
    xacf <- acf(input, plot = FALSE)$acf[-1, 1, 1]
    clim <- tcrit/sqrt(length(input)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal <- ( abs(xacf[ppy]) > clim[ppy] )
    
    if (is.na(test_seasonal)==TRUE){ test_seasonal <- FALSE }
  }
  
  return(test_seasonal)
}

# from https://github.com/M4Competition/M4-methods/blob/master/Benchmarks%20and%20Evaluation.R
# rewritten a bit for clarity
deseason <- function(input, fh) {
  #Estimate seasonaly adjusted time series
  freq <- frequency(input) 
  ST  <- FALSE
  if (freq > 1) { 
    ST <- SeasonalityTest(input, freq) 
  }
  if (ST==TRUE) {
    Dec <- decompose(input, type = "multiplicative")
    des_input <- input/Dec$seasonal
    SIout <- rep_len(tail(Dec$seasonal, freq), length.out = fh)
    #SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-freq+1):length(Dec$seasonal)], fh), fh)
  } else {
    des_input <- input
    SIout <- rep(1, fh)
  }
  list(ts = des_input, si = SIout)
}


 

# f5 <- holt(des_input, h=fh, damped=F)$mean*SIout #Holt
# f6 <- holt(des_input, h=fh, damped=T)$mean*SIout #Damped
# f7 <- Theta.classic(input=des_input, fh=fh)$mean*SIout #Theta
# f8 <- (f4+f5+f6)/3 #Comb

# f3 <- naive(des_input, h=fh)$mean*SIout #Naive2
rw_deseasoned_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "DS-RW",
    long_name = "De-seasoned random walk",
    basis_function = "forecast::rwf(ds_ts)",
    lambda_heuristic = FALSE,
    notes = "This is a random walk on de-seasoned data, with any seasonal components re-added to the RW forecast. M4-f3 benchmark model."
  )
  
  des <- deseason(ts, h)
  
  model <- rwf(des$ts, h = h, lambda = NULL, drift = FALSE, level = 95)
  model$model_string <- "Deseasoned RW (M4-f3)"
  
  fcast       <- forecast(model)
  fcast$mean  <- fcast$mean * des$si
  fcast$upper <- fcast$upper * des$si
  fcast$lower <- fcast$lower * des$si
  
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

# f4 <- ses(des_input, h=fh)$mean*SIout #Ses
ses_deseasoned_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "DS-SES",
    long_name = "De-seasoned simple exponential smoothing / ETS(A,N,N)",
    basis_function = "ets(des_ts, 'ANN')",
    lambda_heuristic = FALSE,
    notes = "Simple exponential smooting / ETS(A,N,N) model on de-seasoned data. The seasonal components are re-added to the model forecast. M4-f4 benchmark model"
  )
  
  des <- deseason(ts, h)
  
  model <- ets(des$ts, "ANN", lambda = NULL, opt.crit = "mse")
  model$model_string <- "Deseasoned ETS(A,N,N) (M4-f4)"
  
  fcast       <- forecast(model, h = h, level = 95)
  fcast$mean  <- fcast$mean * des$si
  fcast$upper <- fcast$upper * des$si
  fcast$lower <- fcast$lower * des$si
  
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}


holt_deseasoned_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "DS-Holt",
    long_name = "De-seasoned Holt's linear trend method / ETS(A,A,N)",
    basis_function = "ets(des_ts, 'AAN')",
    lambda_heuristic = FALSE,
    notes = "Holt's linear trend method / exponential smoothing with linear trend / ETS(A,A,N) model on de-seasoned data. The seasonal components are re-added to the model forecast. M4-f5 benchmark model"
  )
  
  des <- deseason(ts, h)
  
  model <- ets(des$ts, "AAN", lambda = NULL, opt.crit = "mse")
  model$model_string <- "De-seasoned ETS(A,A,N) (M4-f5)"
  
  fcast       <- forecast(model, h = h, level = 95)
  fcast$mean  <- fcast$mean * des$si
  fcast$upper <- fcast$upper * des$si
  fcast$lower <- fcast$lower * des$si
  
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}


damped_deseasoned_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "DS-Holt-damped",
    long_name = "De-seasoned Holt's linear trend method with damped trend / ETS(A,Ad,N)",
    basis_function = "ets(des_ts, 'AAN', damped=TRUE)",
    lambda_heuristic = FALSE,
    notes = "Holt's linear trend method with damped trend / exponential smoothing with damped linear trend / ETS(A,Ad,N) model on de-seasoned data. The seasonal components are re-added to the model forecast. M4-f6 benchmark model"
  )
  
  des <- deseason(ts, h)
  
  model <- ets(des$ts, "AAN", damped = TRUE, lambda = NULL, opt.crit = "mse")
  model$model_string <- "De-seasoned damped ETS(A,Ad,N) (M4-f6)"
  
  fcast       <- forecast(model, h = h, level = 95)
  fcast$mean  <- fcast$mean * des$si
  fcast$upper <- fcast$upper * des$si
  fcast$lower <- fcast$lower * des$si
  
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

m4comb_forecast <- function(ts, lambda = NULL, h) {
  doc <- list(
    short_name = "M4-Comp",
    long_name = "M4 Composite benchmark, average of de-seasoned SES, linear trend, and damped trend smoothing",
    basis_function = "Custom / None",
    lambda_heuristic = FALSE,
    notes = "The M4 benchmark composite model is a simple average of forecasts from three models, all with the de-seasoned data correction: simple exponential smoothing (ETS(A,N,N)), Holt's linear trend method (ETS(A,A,N)), and exponential smoothing with a damped trend (ETS(A,Ad,N)).\n\nThe de-seasoning works by testing data for strong enough seasonality, and in such cases, the seasonal component is removed through classical time series decomposition before models are estimated, and then re-added to the resulting forecasts. "
  )
  
  f4 <- ses_deseasoned_forecast(ts, lambda = NULL, h)$fcast
  f5 <- holt_deseasoned_forecast(ts, lambda = NULL, h)$fcast
  f6 <- damped_deseasoned_forecast(ts, lambda = NULL, h)$fcast
  
  ts_avg <- function(ti, ...) {
    ts(matrix(apply(cbind(...), 1, mean), ncol = 1), 
       start = ti[1], frequency = ti[3], names = "95%")
  }
  if ("mts" %in% class(f4$upper)) stop("mts averaging needs to be implemented")
  
  fcast <- f4
  fcast$method = "M4 Comp"
  tinfo = tsp(f4$mean)
  fcast$mean  <- ts_avg(tinfo, f4$mean, f5$mean, f6$mean)
  fcast$fitted <- ts_avg(tsp(fcast$x), f4$fitted, f5$fitted, f6$fitted)
  fcast$residuals <- fcast$x - fcast$fitted
  fcast$upper <- ts_avg(tinfo, f4$upper, f5$upper, f6$upper)
  fcast$lower <- ts_avg(tinfo, f4$lower, f5$lower, f6$lower)
  colnames(fcast$upper) <- colnames(fcast$lower) <- "95%"
  
  fcast$se <- mean(f4$se, f5$se, f6$se)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  model <- list(model_string = "M4 Composite benchmark (De-seasoned SES + linear trend + damped trend)")
  fcast$model$model_string = model$model_string
  
  list(model = model, fcast = fcast)
}

rnn_forecast <- function(ts, lambda, h) {
  doc <- list(
    short_name = "RNN short_name",
    long_name = "RNN long_name",
    notes = "RNN notes"
  )

  args <- commandArgs(trailingOnly=TRUE)
  request_id   <- args[1]

  rnn_input_fh <- paste0("basil-ts/rnn-request-", request_id, ".json")
  toJSON(list(ts=ts, h = h), pretty=TRUE) %>% writeLines(rnn_input_fh)

  cmd <- paste0("python3 -u rnn/main.py ", rnn_input_fh)
  print(cmd)
  status = system(cmd)
  unlink(rnn_input_fh)

  rnn_output_fh <- paste0("basil-ts/rnn-forecast-", request_id, ".json")
  if ((status != 0) || (!file.exists(rnn_output_fh))) {
    stop("RNN model failed")
  }

  raw_fcast <- fromJSON(rnn_output_fh)
  unlink(rnn_output_fh)

  model <- list(model_string <- "RNN model_string")
  fcast <- list(
    method = "RNN",
    model = NULL,
    level = 95,
    mean = ts(raw_fcast$mean),
    upper = ts(matrix(raw_fcast$upper, ncol = 1, dimnames = list(NULL, "95%"))),
    lower = ts(matrix(raw_fcast$lower, ncol = 1, dimnames = list(NULL, "95%"))),
    x = ts,
    series = "ts",
    fitted = NULL,
    residuals = NULL
  )
  class(fcast) <- "forecast"
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf

  list(model = model, fcast = fcast)
}
