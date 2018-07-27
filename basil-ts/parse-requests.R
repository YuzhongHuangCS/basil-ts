#
#   Functions for parsing requests
#

suppressPackageStartupMessages({
  library("methods")
  library("lubridate")
  library("jsonlite")
  library("stringr")
})


# depends on time-period.R
if (!exists("bb_period")) {
  stop("I depend on functions in time-period.R")
}


#' Check input file has correct format matching API spec
#' 
validate_input_file_format <- function(x) {
  # if (x$payload$`aggregated-data`==TRUE & is.null(x$payload$`last-event-date`)) {
  #   stop("The input file is missing the `last-event-date` field under `payload`. Add it and try again.")
  # }
  if (length(x$payload$`last-event-date`) > 1) {
    stop("Check 'last-event-date' in the input file, it seems to be too long.")
  }
  if (!is.null(x$payload$`last-event-date`)) {
    data_updated_to <- as.Date(as.POSIXct(x$payload$`last-event-date`))
    if (data_updated_to > lubridate::today()) stop("last-event-date in request must be wrong, it is after today.")
  }
  
  invisible(TRUE)
}



#' Parse last date in input data
#' 
parse_data_updated_to <- function(last_event_date, aggregated_data, data_period, target) {
  
  if (is.na(aggregated_data)) stop("aggregated_data argument cannot be NA")
  
  # If aggregated data take input LED at face value
  # otherwise infer, no matter what the passed value is (might be wrong...)
  #
  # WARNING: I'm resetting last_event_date every time aggregated_data is FALSE
  #
  #
  if (aggregated_data==TRUE) {
    
    if (is.na(last_event_date)) stop("last_event_date cannot be missing for aggregated data.")
    
    data_updated_to <- last_event_date
    
    # make sure it doesn't exceed max date implied by data
    implied_date_range <- c(
      max(target$date),
      bb_seq_period(max(target$date), 2, data_period$period)[2] - 1
    )
    if (data_updated_to < implied_date_range[1] | data_updated_to > implied_date_range[2]) {
      stop("last_event_date seems to be wrong, outside the date range implied by last data point in historical data")
    }
    
  } else {
    
    data_updated_to <- bb_seq_period(max(target$date), 2, data_period$period)[2] - 1
    
  }
  
  as.Date(data_updated_to)
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
  # fix for 1919, "more than one" -> "more than 1"
  x <- str_replace(x, "one", "1")
  c1 <- str_detect(x, c("^Will", "(more|less) than ([0-9]+)"))
  # to eliminate "more than...and less than..."
  if (str_detect(x, "more") & str_detect(x, "less")) {
    stop("Both 'more' and 'less' detected, something is wrong. Is this a binary question at all?")
  }
  # less is not implemented
  if (str_detect(x, "less")) {
    stop("Parsing for 'less than' type binary questions is not implemented yet, tell Andy the time has come.")
  }
  if (all(c1)) {
    y <- str_extract(x, "than [0-9,\\.]+")
    y <- str_replace(y, "than ", "")
    return(c(sprintf(">%s", y), sprintf("0 - %s", y)))
  }
  stop("Unable to identify implied question separations for binary question")
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
      # [0 - 1] -> [-Inf, 1.5]
      lapply(., function(x) {
        if (length(x)==2 && x[1]==0) {
          x <- c(-Inf, as.numeric(x[2]) + .5)
        }
        x
      }) %>%
      # if two values, shift outwards, [1, 2] -> [.5, 2.5]
      lapply(., function(x) {
        if (length(x)==2 && x[1]!=-Inf) {
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
  
  # Check if binary cutpoints are in right order
  # binary_seps() will be default put in "Yes"/"No" order, e.g. [Inf, .5, -Inf]
  # if there is ever a question starting with "No" as first value, reverse
  if (binary & tolower(separations$values[1])=="no") {
    cutpoints <- rev(cutpoints)
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
      days   <- 1
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



# Main function, parse_request() ------------------------------------------

parse_request <- function(request) {
  
  target <- data.frame(
    date  = as.Date(request$payload$historical_data$ts[, 1]),
    value = as.numeric(request$payload$historical_data$ts[, 2])
  )
  
  # Initialize list parsed request data
  pr <- list()
  pr$ifp_name        <- request$ifp$name
  pr$binary_ifp      <- request$ifp$`binary?`
  pr$question_period <- parse_question_period(pr$ifp_name)
  pr$data_period     <- parse_data_period(target$date)
  pr$aggregated_data <- id_aggregated_data(pr$ifp_name)
  if (!is.null(request$payload$`data-updated-to`)) {
    req_data_updated_to <- request$payload$`data-updated-to`
  } else {
    req_data_updated_to <- request$payload$`last-event-date`
  }
  pr$orig_data_updated_to  <- as.Date(req_data_updated_to)
  pr$data_updated_to       <- parse_data_updated_to(req_data_updated_to, 
                                                    pr$aggregated_data, pr$data_period,
                                                    target)
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
  
  return(list(target = target, parsed_request = pr))
}

