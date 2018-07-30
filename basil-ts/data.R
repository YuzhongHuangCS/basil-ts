
# Data helpers ------------------------------------------------------------

#' Identify whether aggregated data based on IFP question title
id_aggregated_data <- function(ifp_name) {
  title <- tolower(ifp_name)
  patterns <- c("(acled|icews|earthquakes|sea ice|hacking|boko haram|how many united nations security)")
  aggdata <- str_detect(title, patterns)
  aggdata
}

#' Heuristic for determining how data should be aggregated over time
#' 
#' 
determine_aggregation_method <- function(series_type, ifp_name) {
  qmax <- str_detect(ifp_name, "maximum")
  qmin <- str_detect(ifp_name, "minimum")
  agg <- "mean"
  if (qmax) {
    return("max")
  }
  if (qmin) {
    return("min")
  }
  if (series_type=="count") {
    return("sum")
  }
  if (series_type=="continuous") {
    return("mean")
  } 
  return(agg)
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


# Main function, process_data() -------------------------------------------



#' Process input data
#' 
#' Take care of partial data handling
#' 
#' @param input_data A list with components target, data_updated_to, and data_period
#' 
process_data <- function(input_data, parsed_request) {
  
  pr <- parsed_request
  pr$data_updated_to <- input_data$data_updated_to
  target <- input_data$target
  
  # Do aggregation if neccessary
  pr$was_data_aggregated <- FALSE
  #were_dates_shifted  <- FALSE
  if (pr$data_period$period$period=="day" & pr$question_period$period$period=="fixed") {
    target      <- aggregate_data(target, pr$question_period, pr$agg_method)
    pr$data_period <- parse_data_period(target$date)
    pr$was_data_aggregated <- TRUE
    pr$aggregated_data <- TRUE
    # for backcasting, pr$data_updated_to is still correct because we had daily data and it was set to max(target)
  }
  
  # Check that data are aggregated correctly and dates are aligned
  validate_data(target, pr$data_period, pr$question_period, pr$ifp_name)
  
  # Record some characteristics of aggregated input data
  pr$orig_N <- nrow(target)
  pr$data_has_0 <- any(target$value==0, na.rm = TRUE)
  
  pr$partial_outcome <- FALSE
  pr$partial_train   <- ""
  # retain original target tail for plotting
  pr$target_tail <- tail(target, 1)
  pr$yobs <- NA
  pr$yn <- NA
  
  if (pr$aggregated_data==TRUE & pr$data_period$period$period!="day") {
    
    gt_train_end      <- pr$data_updated_to >= max(target$date)
    gt_question_start <- pr$data_updated_to >= pr$question_period$dates[1]
    
    days_in_period <- find_days_in_period(max(target$date), pr$data_period$period)
    days_avail <- (pr$data_updated_to - max(target$date)) %>% `+`(1) %>% as.integer()
    
    actually_partial <- days_avail < days_in_period
    
    what_to_do <- "nothing"
    if (gt_train_end & !gt_question_start & actually_partial) {
      what_to_do <- "drop or extrapolate"
    }
    if (gt_train_end & gt_question_start) {
      what_to_do <- "update forecast"
    }
    
    if (what_to_do=="nothing") {
      pr$partial_train <- "no"  
    }
    
    if (what_to_do=="drop or extrapolate") {
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
        #pr$target_tail <- tail(target, 1)  # connect to discarded data anyways, not previous point
      }
    }
    
    if (what_to_do=="update forecast") {
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
  
  return(list(target = target, parsed_request = pr))
}
