
if (!require("testthat")) {
  msg <- paste(sep = "\n",
               "R unit tests require 'testthat' package, install with:",
               "  bash: install2.r testthat",
               "  R: install.packages('testthat')")
  stop(msg)
}

library("testthat")
source("basil-ts/ts-forecast.R")

test_dir("basil-ts/tests/testthat")
