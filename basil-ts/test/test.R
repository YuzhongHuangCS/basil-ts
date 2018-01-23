
source("basil-ts/basil-ts/ts-forecast.R")

df <- rbind(
  c("August 2017", "month"),
  c("between 15 October 2017 and 31 October 2017", "half-month"),
  # week
  c("27 December 2017", "day")
  # custom for unusual periods? just # of days?
) %>% as.data.frame() %>% setNames(c("string", "answer"))

lapply(df$string, parse_question_period)

make_dates(as.Date("2017-01-01"), 5, "day")
make_dates(as.Date("2017-01-01"), 5, "week")
make_dates(as.Date("2017-01-01"), 5, "half-month")
make_dates(as.Date("2017-01-16"), 5, "half-month")
make_dates(as.Date("2017-01-01"), 5, "month")


x <- as.Date(c("2017-01-01", "2017-01-15", "2017-01-16", "2017-02-01"))
cast_date(x, "day")
cast_date(x, "week")
cast_date(x, "half-month")
cast_date(x, "month")

main("basil-ts/basil-ts/request.json")$options
