#
#   Create example requests from backcasting IFPs
#   Andreas Beger
#   23 January 2018
#   updated 30 January 2018 for new API spec
#   updated 8 February 2018, redo #68
#

oldwd = getwd()
setwd("~/Work/SAGE-ward-share/basil-ts/tests/ready-eval-examples")
source("../../basil-ts/ts-forecast.R")

library("jsonlite")
library("readxl")
library("lubridate")
library("dplyr")
library("tidyr")
library("rio")
library("readr")
library("stringr")

source("utils.R")

#
#   #65: oil prices, continuous, daily
#   _______________________________________

df <- rio::import("https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls", 
                     sheet = 2, skip = 2) %>%
  dplyr::mutate(Date = as.Date(Date)) %>%
  setNames(c("date", "value"))

rr <- make_request(65)

# data right up to question
rr$ts <- filter(df, date < "2017-11-10") %>% as.matrix()
rr %>% toJSON(dataframe = "values", pretty = TRUE) %>% writeLines("../requests/ifp65a.json")

# data ends before question period
rr$ts <- filter(df, date < "2017-11-09") %>% as.matrix()
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp65b.json")


#
#   #12: interest-rates, continuous, monthly
#   ______________________

url <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/AUS+AUT+BEL+CAN+CHE+CHL+CHN+COL+CRI+CZE+DEU+DNK+ESP+EST+FIN+FRA+GBR+GRC+HUN+IDN+IND+IRL+ISL+ISR+ITA+JPN+KOR+LTU+LUX+LVA+MEX+NLD+NOR+NZL+POL+PRT+RUS+SVK+SVN+SWE+USA+ZAF.STINT.TOT.PC_PA.M/OECD?contentType=csv&detail=code&separator=comma&csv-lang=en&startPeriod=2011"
stint <- rio::import(url, format = "csv") 

rr <- make_request(12)

df <- filter(stint, LOCATION=="IRL") %>%
  mutate(date = as.Date(sprintf("%s-01", TIME)), value = Value) %>%
  select(date, value)

# data right up to question
rr$ts <- filter(df, date < "2017-08-01")  %>% as.matrix()
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp12a.json")

# data ends before question period
rr$ts <- filter(df, date < "2017-07-01")  %>% as.matrix()
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp12b.json")


#
#   #5: count, monthly
#   ______________________________________

acled <- read_rds("data/acled.rds")

df <- acled %>% 
  filter(gwcode==625 & str_detect(EVENT_TYPE, "battle")) %>%
  group_by(date) %>%
  summarize(value = sum(FATALITIES)) %>%
  full_join(., 
            data.frame(date = seq(min(.$date), max(.$date), by = "day")),
            by = "date") %>%
  replace_na(list(value = 0)) %>%
  mutate(date2 = date, date = `day<-`(date, 1)) %>%
  arrange(date2)

rr <- make_request(5)

# data right up to question
dd <- "2017-07-31"
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp5a.json")

# data ends before question
dd <- "2017-06-30"
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp5b.json")

# data ends before question, but partial month in (less than half)
dd <- "2017-07-10"
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp5c.json")

# data ends before question, but partial month in
dd <- "2017-07-16"
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp5d.json")

# partial info for outcome in question period
dd <- "2017-08-10"
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp5e.json")


#
#   #6: count, daily, binary month question
#   _______________________________________

# data ends right before question

# data ends before question

#
#   #68: earthquakes, 15 day question
#   ____________

df <- readRDS("data/earthquakes.rds") %>%
  dplyr::filter(mag >= 5) %>%
  dplyr::mutate(date = as.Date(time)) %>%
  dplyr::group_by(date) %>% 
  summarize(value = n()) %>% 
  full_join(., 
            data.frame(date = seq(min(.$date), max(.$date), by = "day")),
            by = "date") %>%
  replace_na(list(value = 0)) %>%
  mutate(norm_date = norm_fixed_period(date, 15, as.Date("2017-12-01"))) %>%
  arrange(date) %>%
  filter(date < as.Date("2017-12-15"))

rr <- make_request(68)

# data ends right before question
dd <- as.Date("2017-11-30")
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(norm_date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp68a.json")

# data ends before question
dd <- as.Date("2017-11-16")
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(norm_date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp68b.json")

# partial training data, but less than half
dd <- as.Date("2017-11-20")
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(norm_date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp68c.json")

# partial training data, but more than half
dd <- as.Date("2017-11-27")
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(norm_date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp68d.json")

# partial outcome info
dd <- as.Date("2017-12-01")
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(norm_date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp68e.json")

# partial outcome info
dd <- as.Date("2017-12-10")
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(norm_date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp68f.json")

# partial outcome info
dd <- as.Date("2017-12-14")
rr$payload$historical_data$ts <- filter(df, date <= dd) %>% group_by(norm_date) %>% summarize(value = sum(value)) %>% as.matrix()
rr$payload$`last-event-date` <- dd
rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% writeLines("../requests/ifp68g.json")



# End
setwd(oldwd)
