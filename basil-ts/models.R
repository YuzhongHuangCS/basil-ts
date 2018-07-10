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
  "geometric RW"  = "geometric_rw_forecast",
  "DS-RW" = "rw_deseasoned_forecast",
  "DS-SES" = "ses_deseasoned_forecast"
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
    SIout <- rep_len(tail(Dec$seasonal, ppy), length.out = fh)
    #SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
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
  des <- deseason(ts, h)
  lambda <- NULL
  
  model <- rwf(des$ts, h = h, lambda = lambda, drift = FALSE, level = 95)
  model$model_string <- "DS-RW (M4-f3)"
  
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
  des <- deseason(ts, h)
  lambda <- NULL
  
  model <- ets(des$ts, "ANN", lambda = lambda, opt.crit = "mse")
  model$model_string <- "DS-SES (M4-f4)"
  
  fcast       <- forecast(model, h = h, level = 95)
  fcast$mean  <- fcast$mean * des$si
  fcast$upper <- fcast$upper * des$si
  fcast$lower <- fcast$lower * des$si
  
  fcast$se <- forecast_se(fcast, tail = TRUE)
  fcast$trunc_lower <- -Inf
  fcast$trunc_upper <- Inf 
  
  list(model = model, fcast = fcast)
}






