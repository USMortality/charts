source("lib/common.r")

data <- as_tibble(
  read.csv("./data_static/krankenhaus-hitze-licht-hitzetage.csv",
    sep = ";",
    skip = 0
  )
)

ts <- data |>
  select(Jahr, Krankenhausbehandlungen) |>
  as_tsibble(index = Jahr)

ggplot(ts, aes(x = Jahr, y = Krankenhausbehandlungen)) +
  labs(
    title = "Krankenhausbehandlungen [Deutschland]",
    subtitle = "Quelle: destatis.de",
    x = "Jahr",
    y = "Krankenhausbehandlungen"
  ) +
  geom_smooth(method = lm, color = "blue", fill = "blue", level = 0.95) +
  geom_col(fill = "blue") +
  geom_hline(yintercept = 0) +
  twitter_theme() +
  watermark() +
  stat_cor(label.y = 2500, label.x = 2018) +
  stat_regline_equation(label.y = 2300, label.x = 2018)

ts <- data |>
  select(Jahr, Hitzetage) |>
  as_tsibble(index = Jahr)

ggplot(ts, aes(x = Jahr, y = Hitzetage)) +
  labs(
    title = "Hitzetage [Deutschland]",
    subtitle = "Quelle: destatis.de",
    x = "Jahr",
    y = "Hitzetage"
  ) +
  geom_smooth(method = lm, color = "red", fill = "red", level = 0.95) +
  geom_col(fill = "red") +
  geom_hline(yintercept = 0) +
  twitter_theme() +
  watermark() +
  stat_cor(label.y = 20, label.x = 2010) +
  stat_regline_equation(label.y = 18, label.x = 2010)
