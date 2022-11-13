path <- strsplit(commandArgs(trailingOnly = FALSE)[4], "--file=")[[1]][2]
path <- ifelse(is.na(path), ".", dirname(path))
source(paste(path, "_deps.r", sep = "/"))
source(paste(path, "lib/common.r", sep = "/"))

chart <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(size = 3) +
  labs(
    title = "Cars",
    subtitle = "Source: R mtcars test data set",
    x = "Horse Power",
    y = "MPG"
  ) +
  twitter_theme() +
  watermark(mtcars$hp, mtcars$mpg)

save_chart(chart, "test")
