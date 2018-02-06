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






