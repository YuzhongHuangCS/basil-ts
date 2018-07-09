#
#   Time period functions
#

suppressPackageStartupMessages({
  library("methods")
  library("lubridate")
  library("stringr")
})

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