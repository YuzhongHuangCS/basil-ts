
library("forecast")
library("stringr")

#' Segmentize forecast density
#' 
#' Given answer categories and forecast::forecast object, determine forecast
#' probabilities for each category.
#' 
#' @param fc forecast object
#' @param cp cutpoints
category_forecasts <- function(fc, cp) {
  if (length(fc$mean) > 1) {
    stop("Only works with forecast(..., h = 1) for now")
  }
  # determine forecast density SE
  mu <- as.numeric(fc$mean)
  ul <- fc$upper[1]
  
  # BoxCox is lambda was given
  if (!is.null(fc$model$lambda) & is.null(fc$model$constant)) {
    lambda <- fc$model$lambda
    cp <- BoxCox(cp, lambda)
    mu <- BoxCox(mu, lambda)
    ul <- BoxCox(ul, lambda)
  }
  
  # re-calculate forecast density SE
  level <- 80
  if (!grepl("80", colnames(fc$upper)[1])) {
    stop("Only works if first level is 80%, fix function")
  }
  se <- as.numeric((ul - mu) / qnorm(.5 * (1 + level/100)))
  
  cumprob <- c(0, pnorm(cp, mean = mu, sd = se), 1)
  catp <- diff(cumprob)
  
  # for binary questions, return only "yes" answer
  if (length(catp)==2) {
    catp <- catp[2]
  }
  catp
}


testing <- function() {
  
  # 
  #   Continuous values, categorical questions, with no lambda
  #   ________________________________
  
  url <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/AUS+AUT+BEL+CAN+CHE+CHL+CHN+COL+CRI+CZE+DEU+DNK+ESP+EST+FIN+FRA+GBR+GRC+HUN+IDN+IND+IRL+ISL+ISR+ITA+JPN+KOR+LTU+LUX+LVA+MEX+NLD+NOR+NZL+POL+PRT+RUS+SVK+SVN+SWE+USA+ZAF.STINT.TOT.PC_PA.M/OECD?contentType=csv&detail=code&separator=comma&csv-lang=en&startPeriod=2011"
  stint <- rio::import(url, format = "csv") 
  
  #
  #   #12
  #   _____
  
  df <- filter(stint, LOCATION=="IRL")
  
  target = ts(
    data = df$Value[df$TIME < "2017-08"],
    start = str_split(df$TIME[1], "-")[[1]] %>% as.integer(),
    frequency = 12
  )
  
  mdl1 <- auto.arima(target)
  
  cutpoints <- c(-1.76, -.76, .1, 1.1)
  
  plot(forecast(mdl1))
  for (cc in cutpoints) {
    abline(a = cc, b = 0, lty = 3)
  }
  
  category_forecasts(forecast(mdl1, h = 1), cutpoints)
  right <- c(7.94245434538246e-264, 1.04403941753842e-25, 1, 0, 0)
  
  # 
  #   Count values, categorical questions, with lambda
  #   ________________________________
  
  df <- acled %>% 
    filter(gwcode==625 & str_detect(EVENT_TYPE, "battle")) %>%
    mutate(date = `day<-`(date, 1)) %>%
    group_by(date) %>%
    summarize(battle_deaths = sum(FATALITIES)) %>%
    full_join(., 
              data.frame(date = seq(min(.$date), max(.$date), by = "month")),
              by = "date") %>%
    replace_na(list(battle_deaths = 0)) %>%
    arrange(date)
  
  target <- ts(
    data = df %>% filter(date < "2017-08-01") %>% pull(battle_deaths),
    start = c(year(min(df$date)), month(min(df$date))),
    frequency = 12
  )
  
  mdl1 <- auto.arima(target, lambda = .5)
  
  cutpoints <- c(7, 46, 122, 321)
  
  plot(forecast(mdl1), include = 48)
  for (cc in cutpoints) {
    abline(h = cc, lty = 3)
  }
  abline(a = 0, b = 0, lty = 2)
  
  category_forecasts(forecast(mdl1, h = 1), cutpoints)
  
  # 
  #   Count values, binary questions, with lambda
  #   ________________________________
  
  #
  #   #6
  #   _____
  
  df <- acled %>% 
    filter(gwcode==437 & EVENT_TYPE=="riots/protests") %>%
    mutate(date = `day<-`(date, 1)) %>%
    group_by(date) %>%
    summarize(protests = n()) %>%
    full_join(., 
              data.frame(date = seq(min(.$date), max(.$date), by = "month")),
              by = "date") %>%
    replace_na(list(protests = 0)) %>%
    arrange(date)
  
  target <- ts(
    data = df %>% filter(date < "2017-08-01") %>% pull(protests),
    start = c(year(min(df$date)), month(min(df$date))),
    frequency = 12
  )
  
  mdl1 <- auto.arima(target, lambda = .5)
  
  cutpoints <- c(1)
  
  plot(forecast(mdl1), include = 48)
  for (cc in cutpoints) {
    abline(h = cc, lty = 3)
  }
  abline(a = 0, b = 0, lty = 2)
  
  category_forecasts(forecast(mdl1, h = 1), cutpoints)
  
}

