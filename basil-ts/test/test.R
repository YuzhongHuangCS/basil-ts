


df <- rbind(
  c("August 2017", "month"),
  c("between 15 October 2017 and 31 October 2017", "half-month"),
  # week
  c("27 December 2017", "day")
  # custom for unusual periods? just # of days?
) %>% as.data.frame() %>% setNames(c("string", "answer"))
