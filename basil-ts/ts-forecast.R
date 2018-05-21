
suppressPackageStartupMessages({
  library("methods")
  library("forecast")
  library("lubridate")
  library("jsonlite")
  library("stringr")
  library("truncnorm")
})

# Parsing requests --------------------------------------------------------

#' Create period object
#' 
#' To handle weird time periods that are not months or days use a fixed option, which 
#' is any period with a constant day difference in dates
bb_period <- function(x, days = NA) {
  stopifnot(x %in% c("month", "day", "fixed"))
  list(period = x, days = days)
}

#' Number of periods between two periods
#' 
bb_diff_period <- function(d1, d2, pd) {
  d1 <- as.Date(d1)
  d2 <- as.Date(d2)
  days <- as.integer(d2 - d1)
  if (pd$period=="month") {
    out <- round(days / (365.25 / 12))
  } else if (pd$period=="day") {
    out <- days
  } else if (pd$period=="fixed") {
    out <- days / pd$days
  } else {
    stop("Unrecognized period/not implemented")
  }
  attr(out, "period") <- pd
  out
}

#' Sequence of periods
#' 
#' This is like seq.Date, but handles fixed day difference periods (seq.Date only handles weeks like this)
#' 
bb_seq_period <- function(date, length.out, pd) {
  date <- as.Date(date)
  if (pd$period=="month") {
    x <- date %m+% months(0:(length.out - 1))
  } else if (pd$period=="day") {
    x <- date + 0:(length.out-1)
  } else if (pd$period=="halfmonth") {
    if (day(date)==16) {
      mm <- rep(0:ceiling((length.out-1)/2), each = 2)[1:h]
      x <- date %m+% months(mm)
      x <- `day<-`(x, rep_len(c(1, 16), length = h))
    } else {
      mm <- c(0, rep(1:floor(h/2), each = 2))[1:h]
      x <- date %m+% months(mm)
      x <- `day<-`(x, rep_len(c(16, 1), length = h))
    }
  } else if (pd$period=="fixed") {
    x <- date + 0:(length.out-1) * pd$days
  } else {
    stop("Unrecognized period/not implemented")
  }
  x
}

bb_equal_period <- function(pd1, pd2) {
  t1 <- pd1$period==pd2$period
  t2 <- TRUE
  if (pd1$period=="fixed") t2 <- pd1$days==pd2$days
  all(t1, t2)
}

#' Normalize dates to a arbitrary fixed time period
#' 
#' Given an arbitrary fixed time period in days and a reference date, normalize
#' input dates. 
#' 
#' @details 
#' The ref_date argument will be used as the first day of the fixed time periods.
#' For example, to normalize dates to the corresponding Monday of a week, use
#' ref_date = "2018-02-05" (a Monday) and days = 7. To do ISO style weeks, which 
#' use Thursdays as the index date, take this output and afterwards shift to 
#' Thursdays by adding 3. 
#' 
#' @examples
#' feb2018 <- seq(from=as.Date("2018-02-01"), to=as.Date("2018-02-28"), by = "day")
#' norm <- norm_fixed_period(feb2018, days = 7, ref_date = as.Date("2018-02-05"))
#' data.frame(input = feb2018, output = norm, iso_style = norm + 3)
norm_fixed_period <- function(x, days, ref_date) {
  x_int <- as.integer(x)
  x_ref <- as.integer(ref_date)
  shift <- x_ref %% days
  norm_int <- (x_int - shift) %/% days * days + shift
  norm <- as.Date(norm_int, origin = "1970-01-01")
  norm
}


#' Parse last date in input data
#' 
parse_last_date <- function(request, target) {
  last_date <- ifelse(is.null(request$payload$`last-event-date`),
                      max(target$date),
                      request$payload$`last-event-date`)
  last_date <- as.Date(last_date, origin = "1970-01-01")
  if (!is.null(request$payload$`aggregated-data`)) {
    if (request$payload$`aggregated-data`=="month") {
      last_date <- last_date %m+% months(1) - 1
    }
  }
  last_date
}


#' Parse separations for implied cutpoints
#' 
#' @example 
#' seps <- c("<1229.85", "1229.85-1268.42", "1268.42-1301.63", "1301.63-1340.19", ">1340.19")
#' parse_separations(seps)
parse_separations <- function(separations, data_type, ifp_name) {
  # Binary IFPs in request only have "Yes"/"No" as values, need numeric
  # Change the original values to "labels"
  if (all(separations$values %in% c("Yes", "No"))) {
    seps <- c(separations,
              numeric_values = list(binary_seps(ifp_name)))
    binary <- TRUE
  } else {
    seps <- c(separations,
              numeric_values = list(separations$values))
    binary <- FALSE
  }
  
  # some count questions have integer-style separations, e.g. 1271:
  # has [0, 1 - 2, >2]
  # others with higher counts have continuous styles questions, e.g.1514
  # has [<100, 100 - 140, 140 - 170, 170 - 210, >210]
  # for high counts with continuous we want to skip the next section, 
  # but or integer-style low counts we need to adjust the cutpoints to fall
  # between the counts. 'data_type' is not a good way to distinguish these
  # so make own here
  # use the fact that in integer-style questions unique corresponds to length
  # of parsed numbers
  cp_nums <- seps$numeric_values %>%
    str_extract_all("[0-9\\.]+") %>%
    unlist() %>% 
    as.numeric()
  uniN <- length(unique(cp_nums))
  # account for +1 because it could be like [0, 1 - 2, >2]
  integer_style <- ( length(cp_nums) %in% c(uniN, uniN + 1) ) | binary
  
  # shift cutpoints for count questions to simulate discretizing continuous
  # predictions to integer values
  if (data_type=="count" & integer_style) {
    cp_list <- seps$numeric_values %>%
      str_extract_all("[<>]?[0-9\\.]+") %>%
      # if two values, shift outwards, [1, 2] -> [.5, 2.5]
      lapply(., function(x) {
        if (length(x)==2) {
          x <- as.numeric(x) + c(-.5, .5)
        }
        x
      }) %>%
      # [0] -> [-Inf, .5]
      lapply(., function(x) { if (length(x)==1 && x==0) c(-Inf, .5) else x }) %>%
      # [>2] -> [2.5, Inf]
      lapply(., function(x) {
        y <- x
        if (any(str_detect(x, ">"))) {
          n <- as.numeric(str_extract(x, "[-]?[0-9\\.]+"))
          y <- c(Inf, n + .5)
        }
        y
      }) %>%
      # [<2] -> [-Inf, 1.5]
      lapply(., function(x) {
        y <- x
        if (any(str_detect(x, "<"))) {
          n <- as.numeric(str_extract(x, "[-]?[0-9\\.]+"))
          y <- c(-Inf, n - .5)
        }
        y
      }) 
    cutpoints <- cp_list %>% 
      unlist() %>% 
      as.numeric() %>%
      unique()
    
  } else {
    # non-count cutpoints
    cutpoints <- seps$numeric_values %>%
      str_extract_all("[-]?[0-9\\.]+") %>%
      unlist() %>%
      as.numeric() %>%
      unique()
    # Process "<" and ">"
    if (str_detect(seps$numeric_values[1], ">")) cutpoints <- c(Inf, cutpoints)
    if (str_detect(seps$numeric_values[1], "<")) cutpoints <- c(-Inf, cutpoints)
    if (str_detect(tail(seps$numeric_values, 1), ">")) cutpoints <- c(cutpoints, Inf)
    if (str_detect(tail(seps$numeric_values, 1), "<")) cutpoints <- c(cutpoints, -Inf)
  }
  
  increasing <- all(cutpoints==cummax(cutpoints))
  decreasing <- all(cutpoints==cummin(cutpoints)) & length(cutpoints) > 1
  if (!xor(increasing, decreasing)) {
    stop("Cutpoints implied by separations don't seem to be monotonically increasing or decreasing")
  }
  c(cutpoints = list(cutpoints), seps)
}



parse_data_period <- function(x) {
  tdiff <- as.integer(unique(diff(x)))
  if (length(tdiff)==1) {
    # fixed difference in dates
    if (tdiff[1]==1) {
      pd <- bb_period("day")
      days   <- NA
    } else {
      pd <- bb_period("fixed", tdiff[1])
    }
  } else if (all(tdiff < 7)) {
    # why not ==1? because finanical time series skip weekends and holidays...
    pd <- bb_period("day")
  } else if (all(tdiff %in% 13:17)) {
    pd <- bb_period("halfmonth")
  } else if (all(lubridate::day(x)==1)) {
    pd <- bb_period("month")
  } else {
    stop("Unrecognized period")
  }
  list(period = pd)
}

#' Parse date period in question
#' 
parse_question_period <- function(x) {
  # fix form 'in April (Month 04) 2018?'
  x <- str_replace(x, "\\(Month [0-9]{1,2}\\) ", "")
  
  dates <- str_extract_all(x, "([0-9 ]{0,3}[A-Za-z]+[ ]{1}[0-9]{4})")[[1]]
  if (length(dates)==1 & all(str_detect(dates, "[0-9]{1,2}[A-Za-z ]+[0-9]{4}"))) {
    # matches like 27 December 2017
    pd <- bb_period("day")
    dates <- rep(as.Date(dates, format = "%d %B %Y"), 2)
  } else if (length(dates)==1 & all(str_detect(dates, "[A-Za-z]+[ ]{1}[0-9]{4}"))) {
    # matches like December 2017
    pd <- bb_period("month")
    dates  <- as.Date(sprintf("1 %s", dates), format = "%d %B %Y")
    dates  <- c(dates, dates + days_in_month(dates) - 1)
  } else if (length(dates)==2) {
    dates <- as.Date(dates, format = "%d %B %Y")
    # if ( (day(dates[1])==1 & day(dates[2])==15) | (day(dates[1]) %in% c(15, 16) & day(dates[2]) %in% 28:31)) {
    #   period <- "halfmonth"
    #   date   <- dates[1]
    #   if (day(date)==15) day(date) <- 16L
    #} else 
    if (day(dates[1])==1 & day(dates[2])==days_in_month(dates[2])) {
      # first and last date of month
      pd <- bb_period("month")
      date   <- dates[1]
    } else {
      # add +1 to days because in question, dates are inclusive, i.e. 1 to 6 December is a whole week
      pd <- bb_period("fixed", days = as.integer(dates[2]-dates[1] + 1))
      date   <- dates
    }
  } else {
    stop("Unrecognized question period")
  }
  list(period = pd, dates = dates %>% setNames(NULL))
}



guess_series_type <- function(x, question) {
  stopifnot(length(question)==1)
  xvals <- unique(x)
  distinctvals <- length(xvals)
  min0  <- min(xvals)==0
  max1  <- max(xvals)==1
  q_count <- any(str_detect(tolower(question), c("how many", "how much", "acled", "atrocities")))
  q_cont  <- all(str_detect(tolower(question), c("what", "price")))
  q_binary <- str_detect(tolower(question), "any")
  # default
  out <- "continuous"
  if (q_cont) out <- "continuous"
  if (min0 | q_count)      out <- "count"
  if (min0 & max1 & distinctvals==2 & !q_count) out <- "binary"
  out
}

#' Check for comma in seps
validate_seps <- function(seps) {
  # check for ambiguous decimal separators
  if (all(seps %in% c("Yes", "No"))) {
    return(invisible(TRUE))
  } else {
    comma <- any(str_detect(seps, "\\."))
    period <- any(str_detect(seps, ","))
    if (comma & period) {
      msg <- sprintf("Separations contain ambiguous decimal separator, both commas and periods detected\n  Values: [%s]",
                     paste(seps, collapse = "; "))
      stop(msg)
    } 
    # check for mis-parsed seps
    if (sum(str_detect(seps, "^[<>][0-9\\.]+$")) > 2) {
      msg <- sprintf("Separations appear to be mis-parsed, multiple '<X' or '>X'\n  Values: [%s]",
                     paste(seps, collapse = "; "))
      stop(msg)
    }
    # make sure no < or > sign in middle
    if (length(seps) > 2) {
      if (any(str_detect(seps[2:(length(seps)-1)], "[<>]"))) {
        stop(sprintf("Looks like there is '<' or '>' in an element other than the first or last in the separations, this is not going to work. ['%s']", paste0(seps, collapse = "', '")))
      }
    }
    
    # check for '-' without whitespace
    if (any(str_detect(seps, "[0-9]+-[0-9]+"))) {
      stop("Detected separation values with '-' surrounded by numbers, e.g. '1-2', make sure there is white space around it, like '1 - 2'")
    }
  }
  invisible(TRUE)
}

#' Determine separation for binary IFPs
#' 
#' Heuristic for determining separation value for binary IFPs
binary_seps <- function(x) {
  # "Will there be any...?"
  if (all(str_detect(x, c("^Will", "any")))) {
    return(c(">0", "0"))
  }
  # "Will there be more than...?"
  c1 <- str_detect(x, c("^Will", "(more|less) than [0-9]+"))
  c2 <- str_count(x, "(more|less)")  # to eliminate "more than...and less than..."
  if (all(c1) && c2==1) {
    y <- str_extract(x, "than [0-9,\\.]+")
    y <- str_replace(y, "than ", "")
    return(paste0(c(">", "<"), y))
  }
  stop("Unable to identify implied question separations for binary question")
}


# Data helpers ------------------------------------------------------------

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

find_days_in_period <- function(x, period) {
  if (period$period=="fixed") {
    return(period$days)
  } else if (period$period=="month") {
    return(x %>% lubridate::days_in_month() %>% setNames(NULL))
  } else if (period$period=="day") {
    return(1L)
  } else {
    stop("Could not determine days in period, unknown period?")
  }
}

# Update forecasts with partial outcomes ----------------------------------

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


# Forecast helpers --------------------------------------------------------

create_forecast <- function(ts, model = "ARIMA", parsed_request = NULL) {
  pr <- parsed_request
  
  result <- tryCatch({
    if (model=="ARIMA") {
      mdl <- auto.arima(ts, lambda = pr$lambda)
      mdl$model_string <- forecast:::arima.string(mdl)
    } else if (model=="ETS") {
      if (frequency(ts) > 24) {
        spec = "ZZN"
      } else {
        spec = "ZZZ"
      }
      mdl <- ets(ts, model = spec, lambda = pr$lambda)
      mdl$model_string <- mdl$method
    } else if (model=="RW") {
      mdl        <- Arima(ts, c(0, 1, 0), lambda = NULL)
      mdl$model_string <- "RW"
    } else if (model=="geometric RW") {
      
      if (!sum(ts<=0)==0) {
        stop("Series contains values <= 0, model not estimated.")
      }
      mdl        <- Arima(ts, c(0, 1, 0), lambda = 0)
      mdl$model_sring <- "geometric RW"
      
    } else if (model=="mean") {
      mdl <- Arima(ts, c(0, 0, 0), lambda = pr$lambda)
      mdl$model_string <- "mean"
    }
    
    fcast    <- forecast(mdl, h = pr$h, level = c(95))
    fcast$se <- forecast_se(fcast, tail = TRUE) 
    fcast$trunc_lower <- -Inf
    fcast$trunc_upper <- +Inf
    if (pr$partial_outcome) {
      fcast <- update_forecast(fcast, pr$yobs, pr$yn, pr$fcast_date, 
                               pr$data_period, pr$agg_method)
    } 
    
    fcast <- enforce_series_type(fcast, pr$series_type)
    
    # Fit statistics
    rmse      <- sqrt(mean(residuals(mdl)^2))
    rmse_mean <- sqrt(mean(residuals(Arima(ts, c(0, 0, 0)))^2))
    rmse_rwf  <- sqrt(mean(residuals(Arima(ts, c(0, 1, 0), lambda = NULL))^2))
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
    
    lead_point   <- head(resp_ts, 1)
    lead_point$date <- pr$target_tail$date
    lead_point[, 2:ncol(lead_point)] <- pr$target_tail$value
    resp_ts <- rbind(lead_point, resp_ts)
    
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
        AIC = AIC(mdl),
        BIC = BIC(mdl)
      ),
      trainN = length(ts),
      est_model = mdl,
      fcast = fcast
    )
    rownames(result$ts) <- NULL
    
    # Get the answer option probabilities 
    catfcast <- category_forecasts(result$fcast, pr$separations$cutpoints)
    
    # for binary IFPs, category_forecast will return P for "no"/"yes" options
    # if the question is "any" or "more", then we want the second option only??
    if (pr$binary_ifp) {
      pos <- ifelse(str_detect(pr$ifp_name, "(any|more)"), 1L, 2L)
      catfcast         <- catfcast[pos]
    }
    result$option_probabilities <- catfcast
    result$option_labels <- pr$separations$values
    
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
  if (decreasing) catp <- rev(catp)
  catp
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
  #fh = "tests/io/andy_input_1145.json"
  
  if (length(args) > 0) {
    # normal use via Rscript
    request_id   <- args[1]
    backcast     <- ifelse(args[2]=="True", TRUE, backcast)
    drop_after   <- as.Date(args[3])
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
  pr$last_date       <- parse_last_date(request, target)
  pr$question_period <- parse_question_period(pr$ifp_name)
  pr$data_period     <- parse_data_period(target$date)
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
    if (drop_after==as.Date("9999-12-31")) drop_after <- pr$question_period$dates[1] + 1
    # if drop after was user supplied, make sure it does not exceed question end date
    drop_after    <- min(c(drop_after, pr$question_period$dates[2] - 1))
    target        <- target[target$date < (drop_after + 1), ]
    pr$last_date  <- max(target$date)
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
    # update internal data
    target      <- aggregate_data(target, pr$question_period, pr$agg_method)
    pr$data_period <- parse_data_period(target$date)
    pr$was_data_aggregated <- TRUE
  }
  
  # Check that data are aggregated correctly and dates are aligned
  validate_data(target, pr$data_period, pr$question_period, pr$ifp_name)
  
  # Check for partial outcome info
  pr$partial_outcome <- FALSE
  pr$partial_train   <- "no"
  # retain original target tail for plotting
  pr$target_tail <- tail(target, 1)
  pr$yobs <- NA
  pr$yn <- NA
  if (pr$data_period$period$period!="day") {
    
    gt_train_end      <- pr$last_date >= max(target$date)
    gt_question_start <- pr$last_date >= pr$question_period$dates[1]
    
    days_in_period <- find_days_in_period(max(target$date), pr$data_period$period)
    days_avail <- (pr$last_date - max(target$date)) %>% `+`(1) %>% as.integer()
    
    if (gt_train_end & !gt_question_start) {
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
        pr$target_tail <- tail(target, 1)
      }
      
    } else if (gt_train_end & gt_question_start) {
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
  model_types <- c("ARIMA", "ETS", "RW", "geometric RW", "mean")
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
  internal_info$forecast_created_at <- lubridate::now()
  internal_info$rmse_mean <- rmse_mean
  internal_info$rmse_rwf  <- rmse_rwf
  internal_info$backcast  <- backcast
  
  # Put ARIMA forecast at top-level; also copied in forecasts below
  # in the future maybe this will be selected by AIC/BIC/whatever
  response                     <- forecasts[["ARIMA"]]
  response[["parsed_request"]] <- internal_info
  response[["forecasts"]]      <- forecasts
  
  if (test) {
    return(invisible(response))
  } else {
    out_fh <- paste0("basil-ts/forecast-", request_id, ".json")
    toJSON(response, "columns", POSIXt = "ISO8601", pretty = TRUE) %>% writeLines(out_fh)
    return(invisible(NULL))
  }
}

r_basil_ts()
