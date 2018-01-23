#
#   Create example requests from backcasting IFPs
#   Andreas Beger
#   23 January 2018
#

setwd("/media/andybega/DATA/Work/SAGE-ward-share/basil-ts/test/ready-eval-examples")

library("jsonlite")
library("readxl")
library("lubridate")
library("dplyr")
library("tidyr")
library("rio")
library("readr")

ifps <- read_excel("test-ifps.xlsx")

make_request <- function(question, ifp_list = ifps) {
  ifp <- ifp_list %>% filter(`discover question id`==question)
  
  out <- list(
    ts = data.frame(NULL),
    metadata = list(
      title = unique(ifp$`question name`),
      startingAt = unique(ifp$`question starts`),
      endingAt = unique(ifp$`question ends at`),
      options = data.frame(name = ifp$`answer name`)
    )
  )
  out
}

#
#   #65: oil prices, continuous, daily
#   _______________________________________

df <- rio::import("https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls", 
                     sheet = 2, skip = 2) %>%
  dplyr::mutate(Date = as.Date(Date)) %>%
  setNames(c("date", "value"))

rr <- make_request(65)

# data right up to question
rr$ts <- filter(df, date < "2017-11-10") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp65a.json")

# data ends before question period
rr$ts <- filter(df, date < "2017-11-09")
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp65b.json")


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
rr$ts <- filter(df, date < "2017-08-01") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp12a.json")

# data ends before question period
rr$ts <- filter(df, date < "2017-07-01") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp12b.json")


#
#   #5: count, daily, question is monthly
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
  arrange(date)

rr <- make_request(5)

# data right up to question
rr$ts <- filter(df, date < "2017-08-01") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp5a.json")

# data ends before question
rr$ts <- filter(df, date < "2017-07-01") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp5b.json")

# data ends before question, but partial month in
rr$ts <- filter(df, date < "2017-08-15") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp5c.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-08-10") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp5d.json")


#
#   #6: count, daily, binary month question
#   _______________________________________

# data ends right before question

# data ends before question

#
#   #68: earthquakes, half-month question
#   ____________

df <- readRDS("data/earthquakes.rds") %>%
  dplyr::filter(mag >= 5) %>%
  dplyr::mutate(date = as.Date(time)) %>%
  dplyr::group_by(date) %>% 
  summarize(value = n()) %>% 
  arrange(date) 

rr <- make_request(68)

# data ends right before question
rr$ts <- filter(df, date < "2017-12-01") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68a.json")

# data ends before question
rr$ts <- filter(df, date < "2017-11-16") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68b.json")

# data ends before question, but partial month in
rr$ts <- filter(df, date < "2017-11-21") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68c.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-02") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68d.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-03") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68e.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-04") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68f.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-05") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68g.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-06") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68h.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-07") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68i.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-08") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68j.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-09") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68k.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-10") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68l.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-11") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68m.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-12") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68n.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-13") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68o.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-14") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68p.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-15") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68q.json")

# partial info for outcome in question period
rr$ts <- filter(df, date < "2017-12-16") 
rr %>% toJSON(dataframe = "values") %>% writeLines("../requests/ifp68r.json")

