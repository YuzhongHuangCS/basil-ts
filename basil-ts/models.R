#
#   Functions that estimate models and their forecasts
#

model_dictionary <- list(
  "auto ARIMA"    = "auto_arima_forecast",
  "mean"          = "constant_mean_forecast",
  "ETS"           = "ets_forecast",
  "RW"            = "rw_forecast",
  "RW-DRIFT"      = "rw_drift_forecast",
  "RW-SEAS"       = "rw_seasonal_forecast",
  "arithmetic RW" = "arithmetic_rw_forecast",
  "geometric RW"  = "geometric_rw_forecast"
)

get_model <- function(short_name, mdict = model_dictionary) {
  function_name = mdict[[short_name]]
  get(function_name)
}

auto_arima_forecast <- function(ts, lambda, h) {
  model <- forecast::auto.arima(ts, lambda = lambda)
  model$model_string <- forecast:::arima.string(model)
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

constant_mean_forecast <- function(ts, lambda, h) {
  model <- Arima(ts, c(0, 0, 0), lambda = lambda)
  model$model_string <- "constant mean"
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

ets_forecast <- function(ts, lambda, h) {
  if (frequency(ts) > 24) {
    spec = "ZZN"
  } else {
    spec = "ZZZ"
  }
  model <- forecast::ets(ts, model = spec, lambda = lambda)
  model$mdl_string <- model$method
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

rw_forecast <- function(ts, lambda, h) {
  model <- rwf(ts, h = h, lambda = lambda, drift = FALSE, level = 95)
  model$mdl_string <- "RW"
  
  fcast    <- forecast(model)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

rw_drift_forecast <- function(ts, lambda, h) {
  model <- rwf(ts, h = h, lambda = lambda, drift = TRUE, level = 95)
  model$mdl_string <- "RW-DRIFT"
  
  fcast    <- forecast(model)
  # fix bug
  colnames(fcast$upper) <- colnames(fcast$lower) <- c("95%")
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

arithmetic_rw_forecast <- function(ts, lambda, h) {
  model <- Arima(ts, c(0, 1, 0), lambda = NULL)
  model$mdl_string <- "arithmetic RW"
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}

geometric_rw_forecast <- function(ts, lambda, h) {
  if (!sum(ts<=0)==0) {
    stop("Series contains values <= 0, model not estimated.")
  }
  model <- Arima(ts, c(0, 1, 0), lambda = 0)
  model$model_sring <- "geometric RW"
  
  fcast    <- forecast(model, h = h, level = 95)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}


rw_seasonal_forecast <- function(ts, lambda, h) {
  model <- snaive(ts, h = h, lambda = lambda, level = 95)
  model$mdl_string <- "RW-SEAS"
  
  fcast    <- forecast(model)
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}
