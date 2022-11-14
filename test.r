source("lib/common.r")

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
