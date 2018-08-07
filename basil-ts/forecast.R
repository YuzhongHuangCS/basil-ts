#
#   Forecast helpers
#

# depends on time-period.R
if (!exists("bb_period")) {
  stop("I depend on functions in time-period.R")
}

# Forecast helpers --------------------------------------------------------

#' Calculate time periods per year
determine_ts_frequency <- function(x) {
  x <- aggregate(x[, c("date")], by = list(year = lubridate::year(x$date)), FUN = length)$x
  x <- head(x, length(x)-1) %>% tail(length(.)-1)
  fr <- ifelse(length(x)==0, 1, mean(x))
  # daily data, set to week otherwise models blow up
  if (all(x %in% c(365, 366))) {
    fr <- 7
  }
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

safe_AIC <- function(...) {
  tryCatch(AIC(...),
           error = function(e) {
             NA_real_
           })
}

safe_BIC <- function(...) {
  tryCatch(BIC(...),
           error = function(e) {
             NA_real_
           })
}


#' Enforce value constraints
#' 
#' Enforce value constraints for different types of series, e.g. count
#' 
#' @param x modified forecast object with se, trunc_lower, trunc_upper
enforce_series_type <- function(x, type) {
  if (type=="continuous") {
    x <- x
  } else if (type=="count") {
    nudge <- 1e-5
    tspx <- tsp(x$lower)
    if (any(x$lower < 0)) {
      x$trunc_lower  <- 0
    }
    # Single level forecasts will have lower/upper as 'ts', others will have
    # matrix-like 'mts'; apply will return matrix for mts, but vector for ts;
    # the conversion back to ts does not work correctly for vector unless
    # we coerce all the apply output to be matrix with correct dimensions
    nseries <- ncol(x$lower)
    x$lower <- apply(x$lower, 1, pmax, rev(seq(0, (nseries-1))) * nudge)
    x$lower <- matrix(x$lower, nrow = nseries)
    x$lower <- ts(t(x$lower), frequency = tspx[3], start = tspx[1] )
    x$mean[x$mean < 0]           <- 0 + ncol(x$lower)*nudge
    x$upper <- apply(x$upper, 1, pmax, seq(nseries + 1, length.out = nseries) * nudge)
    x$upper <- matrix(x$upper, nrow = nseries)
    x$upper <- ts(t(x$upper), frequency = tspx[3], start = tspx[1] )
  } 
  x
}

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
  trunc_lower <- fc$trunc_lower
  trunc_upper <- fc$trunc_upper
  se <- fc$se
  
  # BoxCox if lambda was given
  # se is already on transformed scale
  if (!is.null(fc$model$lambda) & is.null(fc$model$constant)) {
    lambda <- fc$model$lambda
    if (lambda==0) cp[cp==-Inf] <- 0
    cp <- BoxCox(cp, lambda)
    mu <- BoxCox(mu, lambda)
    trunc_lower <- ifelse(trunc_lower==-Inf, -Inf, BoxCox(trunc_lower, lambda))
    trunc_upper <- ifelse(trunc_lower==Inf, Inf, BoxCox(trunc_upper, lambda))
  }
  
  # need to sort cutpoints otherwise this is screwed up; reorder at end
  increasing <- all(cp==cummax(cp))
  decreasing <- all(cp==cummin(cp)) & length(cp) > 1
  if (!xor(increasing, decreasing)) {
    stop("Cutpoints are not monotonic")
  }
  if (decreasing) {
    cp <- rev(cp)
  }
  
  cumprob <- truncnorm::ptruncnorm(cp, mean = mu, sd = se, a = trunc_lower, 
                                   b = trunc_upper)
  if (!all(range(cumprob)==c(0, 1))) {
    stop(sprintf("Problem with cumulative probabilities, range 0 to 1. [%s]", paste0(cumprob, collapse = ", ")))
  }
  catp    <- diff(cumprob)
  if (identical(all.equal(sum(catp), 1), FALSE)) {
    stop(sprintf("Forecast probabilities do not sum to 1; [%s]", paste0(catp, collapse = ", ")))
  }
  if (decreasing) catp <- rev(catp)
  catp
}


# Update forecasts with partial outcomes ----------------------------------

#' Calculate SE in forecast object
#' 
#' Calculate implicit SE used for normal density prediction intervals.
#' 
#' @param x A forecast object
#' @param tail For multi-period forecasts, calculate the SE at the head or tail
#'   end of the forecast? 
#'   
#' @details The standard error of the forecast density for multi-period forecasts
#'   expands over time. The tail option will calculate the standard error of the
#'   last forecast, use this for converting to categorical probabilities since the
#'   last forecast is the relevant one for the IFP. Using the head or left-edge 
#'   correspond to the square root of "sigma2" in the object returned by 
#'   Arima() or auto.arima(). 
forecast_se <- function(x, tail = TRUE) {
  if (tail) {
    mu <- tail(as.numeric(x$mean), 1)
    ul <- tail(x$upper[, "95%"], 1)
  } else {
    mu <- head(as.numeric(x$mean), 1)
    ul <- head(x$upper[, "95%"], 1)
  }
  
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


#' Update forecast 
#' 
#' Update forecast with partial outcome information
update_forecast <- function(x, yobs, yn, fcast_date, data_period, fun) {
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
  
  if (fun=="sum") {
    new_pars <- update_norm_sum(x$mean, x$se, yobs, yn, N, lambda)
  } else {
    new_pars <- update_norm_avg(x$mean, x$se, yobs, yn, N, lambda)
  }
  
  # CI re-calculation has to be done on transformed scale
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
  if (fun %in% c("count", "max")) {
    if (yobs > x$lower[, "95%"]) {
      x$lower[, ] <- apply(x$lower, 2, pmax, yobs)
      x$trunc_lower <- yobs
    }
    # theoretically but hopefully not practically, the observed outcome can 
    # push us above the forecast mean or even the forecast upper, in that 
    # case nudge these a bit so the density does not collapse to a point
    nudge = .00001
    if (yobs > x$mean) {
      x$mean <- max(x$mean, yobs * (1 + nudge))
    }
    if (yobs > x$upper[, 1]) {
      x$upper[, ] <- apply(x$upper, 2, pmax, yobs * (1 + 2*nudge))
    }
  }
  x
}

#' Update normal forecast with sum method
#' 
#' Update normal density with partial observed outcomes under assumption that it
#' is a sum of smaller normal densities for each day. 
update_norm_sum <- function(mean, se, yobs, yn, N, lambda = NULL) {
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

#' Update normal forecast with mean method
#' 
#' Update normal density with partial observed outcomes under assumption that it
#' is the average of smaller normal densities for each day.
update_norm_avg <- function(mean, se, yobs, yn, N, lambda = NULL) {
  mean_t <- mean
  n <- yn
  if (is.null(lambda)) {
    mean_star <- weighted.mean(x = c(yobs, mean_t), w = c(n, N-n))
  } else {
    mean_star <- weighted.mean(x = BoxCox(c(yobs, mean_t), lambda), w = c(n, N-n))
  }
  se_star <- sqrt(1 - n/N) * se
  c(mean = mean_star, se = se_star)
}




# Main functions ----------------------------------------------------------

#' Create forecast from a specific TS model
#' 
#' This ties in the time series model and code that converts the TS prediction
#' to the categorical probability forecast. Save for the models themselves, 
#' and bug fixes, there is nothing in here that should change. I.e. there are 
#' no optimizable modeling decisions in here. 
#' 
create_single_forecast <- function(ts, model = "auto ARIMA", parsed_request = NULL) {
  pr <- parsed_request
  
  result <- tryCatch({
    
    model_function <- get_model(model)
    res <- model_function(ts, lambda = pr$lambda, h = pr$h)
    
    mdl   <- res$model
    fcast <- res$fcast
    
    # if (model=="ARIMA") {
    #   mdl <- auto.arima(ts, lambda = pr$lambda)
    #   mdl$model_string <- forecast:::arima.string(mdl)
    # } else if (model=="ETS") {
    #   if (frequency(ts) > 24) {
    #     spec = "ZZN"
    #   } else {
    #     spec = "ZZZ"
    #   }
    #   mdl <- ets(ts, model = spec, lambda = pr$lambda)
    #   mdl$model_string <- mdl$method
    # } else if (model=="RW") {
    #   mdl        <- Arima(ts, c(0, 1, 0), lambda = NULL)
    #   mdl$model_string <- "RW"
    # } else if (model=="geometric RW") {
    #   
    #   if (!sum(ts<=0)==0) {
    #     stop("Series contains values <= 0, model not estimated.")
    #   }
    #   mdl        <- Arima(ts, c(0, 1, 0), lambda = 0)
    #   mdl$model_sring <- "geometric RW"
    #   
    # } else if (model=="mean") {
    #   mdl <- Arima(ts, c(0, 0, 0), lambda = pr$lambda)
    #   mdl$model_string <- "mean"
    # }
    
    #fcast    <- forecast(mdl, h = pr$h, level = c(95))
    #fcast$se <- forecast_se(fcast, tail = TRUE) 
    #fcast$trunc_lower <- -Inf
    #fcast$trunc_upper <- +Inf
    if (pr$partial_outcome) {
      fcast <- update_forecast(fcast, pr$yobs, pr$yn, pr$fcast_date, 
                               pr$data_period, pr$agg_method)
    } 
    
    fcast <- enforce_series_type(fcast, pr$series_type)
    
    # Fit statistics
    if (model=="M4-Meta") {
      rmse = NA
    } else {
      rmse      <- sqrt(mean(residuals(mdl)^2, na.rm = TRUE))
    }
    
    # residuals does not work M4-Meta
    #rmse_mean <- sqrt(mean(residuals(Arima(ts, c(0, 0, 0)))^2))
    #rmse_rwf  <- sqrt(mean(residuals(Arima(ts, c(0, 1, 0), lambda = NULL))^2))
    # wrong metric, should be in part based on CI and cat answer spread
    # hard set to 1 for now; 2018-05-21
    #usable <- as.integer(rmse <= rmse_mean & rmse <= rmse_rwf)
    usable <- 1
    
    fcast_end_date <- tail(pr$fcast_date, 1) + 
      find_days_in_period(max(pr$fcast_date), pr$data_period$period) - 1
    
    # Create data for time series plot
    # we're going to extend the raw forecast on either end for plotting
    resp_ts <- data.frame(
      date = pr$fcast_date,
      as.data.frame(fcast)
    )
    
    if (pr$partial_outcome == FALSE & pr$partial_train != "discarded") {
      lead_point   <- head(resp_ts, 1)
      lead_point$date <- pr$target_tail$date
      lead_point[, 2:ncol(lead_point)] <- pr$target_tail$value
      resp_ts <- rbind(lead_point, resp_ts)
    }
    
    if (fcast_end_date != max(resp_ts$date)) {
      tail_point <- tail(resp_ts, 1)
      tail_point$date <- fcast_end_date
      resp_ts <- rbind(resp_ts, tail_point)
    }
    
    
    result <- list(
      model = model,
      ts_colnames = c("date", names(as.data.frame(fcast))),
      ts = as.matrix(resp_ts),
      to_date = fcast_end_date,
      forecast_is_usable = usable, 
      internal = list(
        mdl_string = mdl$model_string,
        rmse = rmse,
        AIC = safe_AIC(mdl),
        BIC = safe_BIC(mdl)
      ),
      trainN = length(ts),
      est_model = mdl,
      fcast = fcast
    )
    rownames(result$ts) <- NULL
    
    # Get the answer option probabilities 
    catfcast <- category_forecasts(result$fcast, pr$separations$cutpoints)
    
    result$option_probabilities <- catfcast
    result$option_labels <- pr$separations$values
    result$estimated = TRUE
    
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


#' TS process data and make forecasts
#' 
#' Controls several optimizable aspects of modeling that are not related to the
#' models themselves, e.g. how to determine TS frequency, etc. Set up this way
#' so that function arguments can control different components and make 
#' testing easier. 
#' 
create_forecasts <- function(target, parsed_request, quick = FALSE, rnn = FALSE) {
  
  pr <- parsed_request
  
  # Determine periods per year for ts frequency
  fr <- as.integer(determine_ts_frequency(target))
  
  # Cut down training data if needed to speed up model estimation
  if (!rnn) {
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
  model_types <- names(model_dictionary)
  if (quick) model_types <- "Auto ARIMA"
  if (rnn) model_types <- "RNN"
  forecasts   <- lapply(model_types, create_single_forecast, 
                        ts = target_ts, parsed_request = pr)
  names(forecasts) <- model_types
  
  # The estimated model and fcast object are helpers for stuff in this level,
  # can take out now, don't need actually in response.
  forecasts <- lapply(forecasts, clean_model)
  names(forecasts) <- model_types
  
  return(list(forecasts = forecasts, parsed_request = pr))
}





