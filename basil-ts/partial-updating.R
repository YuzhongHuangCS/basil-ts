# from msm https://github.com/chjackson/msm/blob/master/R/utils.R

dtnorm <- function(x, mean = 0, sd = 1, lower = -Inf, upper = Inf) {
  sumsq  <- sd*sd 
  nc <- 1/(pnorm(upper, mean, sd) - pnorm(lower, mean, sd))
  nctmp <- pnorm(upper, x, 0) - pnorm(lower, x, 0)
  nc * nctmp * dnorm(x, mean, sd, 0)
}

dmenorm <- function(x, mean=0, sd=1, lower=-Inf, upper=Inf, sderr=0, meanerr=0, log = FALSE) {
  sumsq <- sd*sd + sderr*sderr
  sigtmp <- sd*sderr / sqrt(sumsq)
  mutmp <- ((x - meanerr)*sd*sd + mean*sderr*sderr) / sumsq
  nc <- 1/(pnorm(upper, mean, sd) - pnorm(lower, mean, sd))
  nctmp <- pnorm(upper, mutmp, sigtmp) - pnorm(lower, mutmp, sigtmp)
  if (log)
    log(nc) + log(nctmp) + log(dnorm(x, meanerr + mean, sqrt(sumsq), 0))
  else
    nc * nctmp * dnorm(x, meanerr + mean, sqrt(sumsq), 0)
}

pmenorm <- function(q, mean=0, sd=1, lower=-Inf, upper=Inf, sderr=0, meanerr=0, lower.tail = TRUE, log.p = FALSE) {
  ret <- numeric(length(q))
  dmenorm2 <- function(x)dmenorm(x, mean=mean, sd=sd, lower=lower, upper=upper, sderr=sderr, meanerr=meanerr)
  for (i in 1:length(q)) {
    ret[i] <- integrate(dmenorm2, -Inf, q[i])$value
  }
  if (!lower.tail) ret <- 1 - ret
  if (log.p) ret <- log(ret)
  ret[upper < lower] <- NaN
  ret
}

#' Update normal forecast
#' 
#' Update normal density with partial observed outcomes under assumption that it
#' is a sum of smaller normal densities. 
#' 
update_agg_norm <- function(mean, se, yobs, yn, N) {
  mean_t <- mean/N
  se_t   <- sqrt(se^2/N)
  n=yn
  mean_star <- sum(yobs) + (N-n)*mean_t
  se_star <- sqrt((N-n)*se_t^2)
  c(mean_star, se_star)
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
  
  new_pars <- update_agg_norm(x$mean, x$se, yobs, yn, N)
  x$mean[]   <- new_pars[1]
  x$se     <- new_pars[2]
  level <- colnames(x$upper) %>% gsub("%", "", .) %>% as.numeric()
  nint <- length(level)
  for (i in 1:nint) {
    qq <- qnorm(0.5 * (1 + level[i] / 100))
    x$lower[, i] <- x$mean - qq * x$se
    x$upper[, i] <- x$mean + qq * x$se
  }
  # Enforce minimum already observed count
  x$lower[, ] <- apply(x$lower, 2, pmax, yobs)
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
  
  # BoxCox is lambda was given
  if (!is.null(fc$model$lambda) & is.null(fc$model$constant)) {
    lambda <- fc$model$lambda
    cp <- BoxCox(cp, lambda)
    mu <- BoxCox(mu, lambda)
  }
  
  se <- forecast_se(fc)
  
  cumprob <- c(0, pnorm(cp, mean = mu, sd = se), 1)
  catp <- diff(cumprob)
  catp
}


#' Calculate SE in forecast object
#' 
#' Calculate implicity SE used for normal density prediction intervals.
forecast_se <- function(x) {
  mu <- tail(as.numeric(x$mean), 1)
  ul <- tail(x$upper[, "95%"], 1)
  
  # BoxCox is lambda was given
  if (!is.null(x$model$lambda) & is.null(x$model$constant)) {
    lambda <- fc$model$lambda
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




