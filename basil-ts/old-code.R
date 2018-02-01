# Old code not used anymore, but keep in case used down the road

# Aggregation-related code ------------------------------------------------
#
#   Not used since 1/30/18 API change, but keep in case it is needed later
#

halfmonth <- function(x) {
  stopifnot(is.Date(x))
  paste(substr(x, 6, 7), ifelse(substr(x, 9, 10) < 16, "01", "16"), sep = "-")
}

lbl <- paste(sprintf("%02d", rep(1:12, 1, each = 2)), c("01", "16"), sep = "-")
N_DAYS_IN_HALFMONTHS <- as.Date(c(sprintf("2017-%s", lbl), "2018-01-01")) %>% diff() %>% as.integer()
names(N_DAYS_IN_HALFMONTHS) <- lbl

days_in_halfmonth <- function(x) {
  halfmonth_x <- halfmonth(x)
  n_days <- N_DAYS_IN_HALFMONTHS[halfmonth_x]
  n_days[halfmonth_x == "02-16" & leap_year(x)] <- 14L
  n_days
}

days_in_period <- function(x, period) {
  if (period=="month") {
    days_in_month(x)
  } else if (period=="halfmonth") {
    days_in_halfmonth(x)
  } else {
    stop("Unrecognized period")
  }
}

#' Normalize date for different kinds of periods
#' 
cast_date <- function(x, period) {
  stopifnot(is.Date(x))
  if (period %in% c("day", "fixed")) {
    out <- x
  } else if (period=="week") {
    stop("not implemented")
  } else if (period=="halfmonth") {
    out <- `day<-`(x, ifelse(day(x) < 16, 1, 16))
  } else if (period=="month") {
    out <- `day<-`(x, 1)
  } else {
    stop("Unrecognized period")
  }
  out
}

aggregate <- function() {
  # Aggregate
  target$normdate <- cast_date(target$date, question_period$period)
  
  # Aggregate
  # TODO fill in missing time periods when aggregating; month and halfmonth data only, i think
  partial    <- target[target$date >= question_period$date[1], ]
  target_agg <- target[target$date < question_period$date[1], ]
  # check for partial training data
  if (question_period$period %in% c("month", "halfmonth") & data_period$period=="day" & series_type=="count") {
    target_agg$rows <- 1
    target_agg <- aggregate(target_agg[, c("value", "rows")], by = list(target_agg$normdate), FUN = sum) %>%
      setNames(c("normdate", "value", "rows"))
    target_agg$days  <- days_in_period(target_agg$normdate, question_period$period)
    # don't extrapolate if less than half period; drop in that case
    target_agg <- target_agg[target_agg$rows / target_agg$days > .5, ]
    # extrapolate for partials with more than x
    target_agg$value <- as.integer(target_agg$days / target_agg$rows * target_agg$value)
  } else {
    target_agg <- aggregate(target_agg[, c("value")], by = list(target_agg$normdate), FUN = sum) %>%
      setNames(c("normdate", "value"))
  }
}


# 1/30/18 API update: the payload not includes parsed separations
parse_raw_options <- function(x) {
  cutpoints <- x %>% 
    str_extract_all(., "[-0-9\\.,]+") %>%
    unlist() %>%
    str_replace_all(., ",", "") %>%
    as.numeric() %>%
    unique()
  if (length(cutpoints)==0) cutpoints <- 1
  
  list(cutpoints = cutpoints, options = x)
}
