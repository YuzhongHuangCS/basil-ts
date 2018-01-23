#
#   Do some univariate time series forecasts for IFPs from the readiness eval
#   Andreas Beger
#   2018-01-17
#

library("lubridate")
library("dplyr")
library("tidyr")
library("readr")
library("forecast")
library("rio")
library("dplyr")
library("glue")
library("yaml")
library("stringr")
library("lubridate")
library("states")
library("futile.logger")
library("stringr")
library("R6")
library("readxl")
library("stringr")

source("notes/2018-01b-machine-forecasts/category_forecasts.R")

ifps <- read_excel("notes/2018-01b-machine-forecasts/test-ifps.xlsx")

parse_answers <- function(question_id, ifp_list = ifps) {
  ifp <- ifp_list %>%
    filter(`discover question id`==question_id) 
  cutpoints <- ifp$`answer name` %>% 
    str_extract_all(., "[-0-9\\.]+") %>%
    unlist() %>%
    as.numeric() %>%
    unique()
  if (length(cutpoints)==0) cutpoints <- 1
  
  answer_ids <- pull(ifp, `discover answer id`)
  list(cutpoints = cutpoints, answer_ids = answer_ids)
}


results <- R6Class(
  "results",
  public = list(
    data = data.frame(NULL),
    add = function(id, pfcast, oo, aid, cfcast) {
      nans <- length(aid)
      newifp <- data.frame(discover_question_id = rep(id, nans), 
                           point_forecast = rep(pfcast, nans), 
                           observed_outcome = rep(oo, nans), 
                           discover_answer_id = aid, 
                           forecast_probability = cfcast)
      if (id %in% self$data$discover_question_id) {
        warning("Updating existing forecast")
        self$data[self$data$discover_question_id==id, ] <- newifp
      } else {
        self$data <- rbind(self$data, newifp) %>%
          arrange(discover_question_id)
      }
      
      invisible(self)
    }
  ))

forecasts <- results$new()


# Earthquakes -------------------------------------------------------------


if (!file.exists("notes/2018-01b-machine-forecasts/earthquakes.rds")) {
  # https://earthquake.usgs.gov/earthquakes/search/
  
  cs <- cols(
    time = col_datetime(format = ""),
    latitude = col_double(),
    longitude = col_double(),
    depth = col_double(),
    mag = col_double(),
    magType = col_character(),
    nst = col_integer(),
    gap = col_double(),
    dmin = col_double(),
    rms = col_double(),
    net = col_character(),
    id = col_character(),
    updated = col_datetime(format = ""),
    place = col_character(),
    type = col_character(),
    horizontalError = col_double(),
    depthError = col_double(),
    magError = col_double(),
    magNst = col_integer(),
    status = col_character(),
    locationSource = col_character(),
    magSource = col_character()
  )
  
  eq1 <- read_csv("https://earthquake.usgs.gov/fdsnws/event/1/query.csv?starttime=2010-01-01%2000:00:00&endtime=2018-01-16%2023:59:59&minmagnitude=5&orderby=time",
                  col_types = cs)
  eq2 <- read_csv("https://earthquake.usgs.gov/fdsnws/event/1/query.csv?starttime=2001-01-01%2000:00:00&endtime=2010-01-01%0000:00:00&minmagnitude=5&orderby=time",
                  col_types = cs)
  eq <- bind_rows(eq1, eq2)
  
  write_rds(eq, path = "notes/2018-01b-machine-forecasts/earthquakes.rds",
            compress = "gz")
  
} else {
  eq <- readRDS("notes/2018-01b-machine-forecasts/earthquakes.rds")
}

#
#   #68
#   _____

qid <- 68
for_date <- "2017-12-01"

bimonthly <- eq %>%
  dplyr::mutate(date = as.Date(time), 
                date = `day<-`(date, ifelse(day(date) < 16, 1, 16))) %>%
  dplyr::group_by(date) %>% 
  summarize(earthquakes = n()) %>% 
  arrange(date) %>%
  filter(date <= for_date)

target <- ts(data = bimonthly$earthquakes[-nrow(bimonthly)], start = c(2001, 1), frequency = 24)

plot(target)
plot(stl(target, s.window = "periodic"))
mdl1 <- auto.arima(target)
plot(residuals(mdl1))
Acf(residuals(mdl1))
Pacf(residuals(mdl1))

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 12), include = 12*5)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = bimonthly$earthquakes[bimonthly$date==for_date],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

#
#   #40
#   _____

qid <- 40
for_date <- "2017-10-01"

monthly <- eq %>%
  dplyr::mutate(date = as.Date(time), 
                date = `day<-`(date, 1)) %>%
  dplyr::group_by(date) %>% 
  summarize(earthquakes = n()) %>% 
  arrange(date)

target <- ts(data = filter(monthly, date < for_date) %>% pull(earthquakes), 
             start = c(year(min(monthly$date)), month(min(monthly$date))), 
             frequency = 12)
mdl1 <- auto.arima(target, lambda = .5)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 20), include = 100)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = monthly$earthquakes[monthly$date==for_date],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)


# ACLED -------------------------------------------------------------------

if (!file.exists("notes/2018-01b-machine-forecasts/acled.rds")) {
  source("basil/basil/data-modules/acled/acled.R")
  acled <- get_acled("raw")
  write_rds(acled, "notes/2018-01b-machine-forecasts/acled.rds",
            compress = "gz")
} else {
  acled <- read_rds("notes/2018-01b-machine-forecasts/acled.rds")
}


#
#   Questions about riots/protets
#   _____________________________

#
#   #6
#   _____

qid <- 6
for_date <- "2017-08-01"

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

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 48)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$protests[df$date=="2017-08-01"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

#
#   #18
#   _____

qid <- 18
for_date <- "2017-09-01"

df <- acled %>% 
  filter(gwcode==433 & EVENT_TYPE=="riots/protests") %>%
  mutate(date = `day<-`(date, 1)) %>%
  group_by(date) %>%
  summarize(protests = n()) %>%
  full_join(., 
            data.frame(date = seq(min(.$date), max(.$date), by = "month")),
            by = "date") %>%
  replace_na(list(protests = 0)) %>%
  arrange(date)

target <- ts(
  data = df %>% filter(date < "2017-09-01") %>% pull(protests),
  start = c(year(min(df$date)), month(min(df$date))),
  frequency = 12
)

mdl1 <- auto.arima(target, lambda = .5)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 48)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$protests[df$date=="2017-09-01"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

#
#   #32
#   _____

qid <- 32
for_date <- "2017-10-01"

df <- acled %>% 
  filter(gwcode==541 & EVENT_TYPE=="riots/protests") %>%
  mutate(date = `day<-`(date, 1)) %>%
  group_by(date) %>%
  summarize(protests = n()) %>%
  full_join(., 
            data.frame(date = seq(min(.$date), max(.$date), by = "month")),
            by = "date") %>%
  replace_na(list(protests = 0)) %>%
  arrange(date)

target <- ts(
  data = df %>% filter(date < "2017-10-01") %>% pull(protests),
  start = c(year(min(df$date)), month(min(df$date))),
  frequency = 12
)

mdl1 <- auto.arima(target, lambda = .5)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 48)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$protests[df$date=="2017-10-01"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

#
#   #49 - bi-monthly
#   ________________

qid <- 49
for_date <- "2017-10-15"

dates <- data.frame(
  date = c(seq(min(acled$date), max(acled$date), by = "month"),
           min(acled$date) %>% `day<-`(., 15) %>% seq(., max(acled$date), by = "month"))
) %>%
  arrange(date)

df <- acled %>% 
  filter(gwcode==451 & EVENT_TYPE=="riots/protests") %>%
  mutate(date = `day<-`(date, ifelse(day(date) < 15, 1, 15))) %>%
  group_by(date) %>%
  summarize(protests = n()) %>%
  full_join(., dates, by = "date") %>%
  replace_na(list(protests = 0)) %>%
  arrange(date)

target <- ts(
  data = df %>% filter(date < "2017-10-15") %>% pull(protests),
  start = c(year(min(df$date)), month(min(df$date))),
  frequency = 24
)

mdl1 <- auto.arima(target, lambda = .5)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 48)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$protests[df$date=="2017-10-15"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

#
#   Questions about battle deaths
#   _____________________________

# 
#   #5
#   _____

qid <- 5
for_date <- "2017-08-01"

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

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 48)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$battle_deaths[df$date=="2017-08-01"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

# 
#   #24
#   _____

qid <- 24
for_date <- "2017-09-01"

df <- acled %>% 
  filter(gwcode==615 & str_detect(EVENT_TYPE, "battle")) %>%
  mutate(date = `day<-`(date, 1)) %>%
  group_by(date) %>%
  summarize(battle_deaths = sum(FATALITIES)) %>%
  full_join(., 
            data.frame(date = seq(min(.$date), max(.$date), by = "month")),
            by = "date") %>%
  replace_na(list(battle_deaths = 0)) %>%
  arrange(date)

target <- ts(
  data = df %>% filter(date < "2017-09-01") %>% pull(battle_deaths),
  start = c(year(min(df$date)), month(min(df$date))),
  frequency = 12
)

mdl1 <- auto.arima(target, lambda = .5)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 48)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$battle_deaths[df$date=="2017-09-01"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

# 
#   #23
#   _____

qid <- 23
for_date <- "2017-09-01"

df <- acled %>% 
  filter(gwcode==501 & str_detect(EVENT_TYPE, "battle")) %>%
  mutate(date = `day<-`(date, 1)) %>%
  group_by(date) %>%
  summarize(battle_deaths = sum(FATALITIES)) %>%
  full_join(., 
            data.frame(date = seq(min(.$date), max(.$date), by = "month")),
            by = "date") %>%
  replace_na(list(battle_deaths = 0)) %>%
  arrange(date)

target <- ts(
  data = df %>% filter(date < "2017-09-01") %>% pull(battle_deaths),
  start = c(year(min(df$date)), month(min(df$date))),
  frequency = 12
)

mdl1 <- auto.arima(target, lambda = .5)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 48)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$battle_deaths[df$date=="2017-09-01"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

# 
#   #47
#   _____

qid <- 47
for_date <- "2017-10-01"

df <- acled %>% 
  filter(gwcode==490 & str_detect(EVENT_TYPE, "battle")) %>%
  mutate(date = `day<-`(date, 1)) %>%
  group_by(date) %>%
  summarize(battle_deaths = sum(FATALITIES)) %>%
  full_join(., 
            data.frame(date = seq(min(.$date), max(.$date), by = "month")),
            by = "date") %>%
  replace_na(list(battle_deaths = 0)) %>%
  arrange(date)

target <- ts(
  data = df %>% filter(date < "2017-10-01") %>% pull(battle_deaths),
  start = c(year(min(df$date)), month(min(df$date))),
  frequency = 12
)

mdl1 <- auto.arima(target, lambda = .5)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 48)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$battle_deaths[df$date=="2017-10-01"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)


#
#   Questions about civilian fatalities
#   ___________________________________

#
#   #35
#   _____

qid <- 35
for_date <- "2017-10-01"

df <- acled %>% 
  filter(gwcode==540 & EVENT_TYPE %in% c("violence against civilians", "remote violence")) %>%
  mutate(date = `day<-`(date, 1)) %>%
  group_by(date) %>%
  summarize(civ_deaths = sum(FATALITIES)) %>%
  full_join(., 
            data.frame(date = seq(min(.$date), max(acled$date), by = "month")),
            by = "date") %>%
  replace_na(list(civ_deaths = 0)) %>%
  arrange(date)

target <- ts(
  data = df %>% filter(date < "2017-10-01") %>% pull(civ_deaths),
  start = c(year(min(df$date)), month(min(df$date))),
  frequency = 12
)

mdl1 <- auto.arima(target, lambda = .5)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 48)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$civ_deaths[df$date=="2017-10-01"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

#
#   #36
#   ______

qid <- 36
for_date <- "2017-10-01"

df <- acled %>% 
  filter(gwcode==552 & EVENT_TYPE %in% c("violence against civilians", "remote violence")) %>%
  mutate(date = `day<-`(date, 1)) %>%
  group_by(date) %>%
  summarize(civ_deaths = sum(FATALITIES)) %>%
  full_join(., 
            data.frame(date = seq(min(.$date), max(acled$date), by = "month")),
            by = "date") %>%
  replace_na(list(civ_deaths = 0)) %>%
  arrange(date)

target <- ts(
  data = df %>% filter(date < "2017-10-01") %>% pull(civ_deaths),
  start = c(year(min(df$date)), month(min(df$date))),
  frequency = 12
)

mdl1 <- auto.arima(target, lambda = .5)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 48)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$civ_deaths[df$date=="2017-10-01"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)


#
#   #48 - bi-monthly
#   ________________

qid <- 48
for_date <- "2017-10-15"

dates <- data.frame(
  date = c(seq(min(acled$date), max(acled$date), by = "month"),
           min(acled$date) %>% `day<-`(., 15) %>% seq(., max(acled$date), by = "month"))
) %>%
  arrange(date)

df <- acled %>% 
  filter(gwcode==616 & EVENT_TYPE %in% c("violence against civilians", "remote violence")) %>%
  mutate(date = `day<-`(date, ifelse(day(date) < 15, 1, 15))) %>%
  group_by(date) %>%
  summarize(civ_deaths = sum(FATALITIES)) %>%
  full_join(., 
            data.frame(date = seq(min(.$date), max(acled$date), by = "month")),
            by = "date") %>%
  replace_na(list(civ_deaths = 0)) %>%
  arrange(date)

target <- ts(
  data = df %>% filter(date < "2017-10-15") %>% pull(civ_deaths),
  start = c(year(min(df$date)), month(min(df$date))),
  frequency = 24
)

mdl1 <- auto.arima(target, lambda = .5)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 48)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$civ_deaths[df$date=="2017-10-15"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)



# Oil prices --------------------------------------------------------------

# brent <- rio::import("https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls", 
#                      sheet = 2, skip = 2) %>%
#   dplyr::full_join(., 
#                    data.frame(Date = seq(min(.$Date), max(.$Date), by = "day")),
#                    by = "Date") %>%
#   dplyr::mutate(Date = as.Date(Date)) %>%
#   arrange(Date)

brent <- rio::import("https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls", 
                     sheet = 2, skip = 2) %>%
  dplyr::mutate(Date = as.Date(Date)) %>%
  arrange(Date) 

#
#   #65
#   _____

qid <- 65
for_date <- "2017-11-10"

target <- ts(brent[brent$Date < "2017-11-10", 2])

mdl1 <- auto.arima(target)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 100)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = brent[brent$Date=="2017-11-10", 2],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

#
#   #122
#   _____

qid <- 122
for_date <- "2017-12-01"

target <- ts(brent[brent$Date < "2017-12-01", 2])

mdl1 <- auto.arima(target)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 100)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = brent[brent$Date=="2017-12-01", 2],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

#
#   #143
#   _____

qid <- 143
for_date <- "2017-12-13"

target <- ts(brent[brent$Date < "2017-12-13", 2])

mdl1 <- auto.arima(target)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 100)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = brent[brent$Date=="2017-12-13", 2],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

#
#   #149
#   _____

qid <- 149
for_date <- "2017-12-20"

target <- msts(brent[brent$Date < "2017-12-20", 2], seasonal.periods = c(7, 365.25))
target <- ts(brent[brent$Date < "2017-12-20", 2])

mdl1 <- auto.arima(target)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 10), include = 100)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = brent[brent$Date=="2017-12-20", 2],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)


# FAO food prices ---------------------------------------------------------

url <- "http://www.fao.org/fileadmin/templates/worldfood/Reports_and_docs/Food_price_indices_data.csv"
fpi <- rio::import(url, skip = 2, header = TRUE)
fpi <- fpi %>%
  mutate(date = as.Date(sprintf("15/%s", Date), format = "%d/%m/%Y")) %>%
  select(which(!sapply(., function(x) sum(is.na(x)))==nrow(.))) %>%
  select(-Date)

#
#   #55
#   ____

qid <- 55
for_date <- "2017-10-15"

target <- ts(
  data = fpi$`Sugar Price Index`[fpi$date < "2017-10-15"],
  start = c(1990, 1),
  frequency = 12
)

mdl1 <- auto.arima(target)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 12), include = 12*10)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = fpi$`Sugar Price Index`[fpi$date=="2017-10-15"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)


# Interest rates, short-term --------------------------------------------------

url <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/AUS+AUT+BEL+CAN+CHE+CHL+CHN+COL+CRI+CZE+DEU+DNK+ESP+EST+FIN+FRA+GBR+GRC+HUN+IDN+IND+IRL+ISL+ISR+ITA+JPN+KOR+LTU+LUX+LVA+MEX+NLD+NOR+NZL+POL+PRT+RUS+SVK+SVN+SWE+USA+ZAF.STINT.TOT.PC_PA.M/OECD?contentType=csv&detail=code&separator=comma&csv-lang=en&startPeriod=2011"
stint <- rio::import(url, format = "csv") 
  

#
#   #12
#   _____

qid <- 12
for_date <- "2017-08"

df <- filter(stint, LOCATION=="IRL")

target = ts(
  data = df$Value[df$TIME < "2017-08"],
  start = str_split(df$TIME[1], "-")[[1]] %>% as.integer(),
  frequency = 12
)

mdl1 <- auto.arima(target)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 12), include = 12*10)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$Value[df$TIME=="2017-08"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

#
#   #41
#   _____

qid <- 41
for_date <- "2017-10"

df <- filter(stint, LOCATION=="AUS")

target = ts(
  data = df$Value[df$TIME < "2017-10"],
  start = str_split(df$TIME[1], "-")[[1]] %>% as.integer(),
  frequency = 12
)

mdl1 <- auto.arima(target)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 12), include = 12*10)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$Value[df$TIME=="2017-10"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

# Interest rates, long-term --------------------------------------------------

url <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.LTINT.TOT.PC_PA.M/OECD?contentType=csv&detail=code&separator=comma&csv-lang=en&startPeriod=2007"
ltint <- rio::import(url, format = "csv") 

#
#   #15
#   ____

qid <- 15
for_date <- "2017-08"

df <- filter(ltint, LOCATION=="COL")

target = ts(
  data = df$Value[df$TIME < "2017-08"],
  start = str_split(df$TIME[1], "-")[[1]] %>% as.integer(),
  frequency = 12
)

mdl1 <- auto.arima(target)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 12), include = 12*10)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$Value[df$TIME=="2017-08"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

#
#   #20
#   ____

qid <- 20
for_date <- "2017-08"

df <- filter(ltint, LOCATION=="ZAF")

target = ts(
  data = df$Value[df$TIME < "2017-08"],
  start = str_split(df$TIME[1], "-")[[1]] %>% as.integer(),
  frequency = 12
)

mdl1 <- auto.arima(target)

cutpoints <- parse_answers(qid)$cutpoints
fcast <- forecast(mdl1, h = 1)
answers <- parse_answers(qid)$answer_ids
plot(forecast(mdl1, h = 12), include = 12*10)
for (cc in cutpoints) {
  abline(h = cc, lty = 3)
}

forecasts$add(
  id     = qid,
  pfcast = fcast$mean,
  oo     = df$Value[df$TIME=="2017-08"],
  aid    = answers,
  cfcast = category_forecasts(fcast, cutpoints)
)

# Export results ----------------------------------------------------------

write_csv(forecasts$data, path = "notes/2018-01b-machine-forecasts/ph-ts-forecasts.csv")

