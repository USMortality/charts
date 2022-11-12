path <- strsplit(commandArgs(trailingOnly = FALSE)[4], "--file=")[[1]][2]
path <- ifelse(is.na(path), ".", dirname(path))
source(paste(path, "lib/aws.r", sep = "/"))

pacman::p_load(
  jsonlite, clock, scales, dplyr, fabletools, fable, httr, tsibble, stringr,
  ggplot2
)
