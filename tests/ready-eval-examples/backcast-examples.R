#
#   Backcasting forecast update examples for Mark
#   Andreas Beger
#   8 February 2018
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

library("tidyverse")

source("utils.R")


# IFP 68 earthquakes ------------------------------------------------------

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
dates <- seq(as.Date("2017-11-01"), as.Date("2017-12-14"), by = "day")
out <- data.frame(NULL)
for (dd in dates) {
  cat(as.character(dd), "\n")
  rr$payload$historical_data$ts <- filter(df, date <= dd) %>% 
    group_by(norm_date) %>% summarize(value = sum(value)) %>% as.matrix()
  rr$payload$`last-event-date` <- dd
  res <- rr %>% toJSON(dataframe = "rows", pretty = TRUE) %>% r_basil_ts() 
  out_i <- data.frame(forecast_with_data_to = dd,
                      discover_question_id = rr$ifp$discover_question_id,
                      discover_answer_id = rr$ifp$discover_answer_id,
                      answer_name = as.character(rr$ifp$options[, 1]),
                      answer_p = res$option_probabilities,
                      forecast_is_usable = res$forecast_is_usable,
                      fcast_raw_mean = res$ts[, "mean"] %>% tail(1),
                      stringsAsFactors = FALSE)
  out <- rbind(out, out_i, stringsAsFactors=FALSE)
}
out$forecast_with_data_to <- as.Date(out$forecast_with_data_to, origin = "1970-01-01")

lvls <- rev(as.character(rr$ifp$options[, "name"]))
out$answer_name <- factor(out$answer_name, levels = lvls)

ggplot(out, aes(x = forecast_with_data_to, y = answer_p, fill = answer_name2)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_brewer(type = "seq") +
  annotate("rect", xmin = as.Date("2017-12-01")-.5, xmax = as.Date("2017-12-15")+.5,
           ymin = 0, ymax = 1, fill = NA, color = "gray50", size = .5,
           linetype = 2) +
  annotate("text", x = as.Date("2017-12-01"), y = .99, label = "Question period",
           hjust = 0, vjust = 1, color = "gray50") +
  ggtitle(str_wrap(rr$ifp$name, 80)) +
  labs(x = "Forecast with data to (inclusive)") +
  labs(y = "Probability")

ggsave("ifp68.png", height = 4, width = 8)

out %>% write_csv(., path = "backcasts.csv")

#
#   Done
#   ______

setwd(oldwd)