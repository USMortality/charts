source("lib/common.r")

df_r99 <- data.frame(x = seq(as.Date("2012/1/1"), as.Date("2021/1/1"), "years"), y = c(
  0.022,
  0.024,
  0.023,
  0.026,
  0.027,
  0.029,
  0.028,
  0.032,
  0.028,
  0.027
))

df_r96 <- data.frame(x = seq(as.Date("2012/1/1"), as.Date("2021/1/1"), "years"), y = c(
  0.024,
  0.025,
  0.026,
  0.027,
  0.025,
  0.024,
  0.027,
  0.025,
  0.021,
  0.019
))


df <- df_r96 %>% as_tsibble(index = x)
ggplot(df, aes(x = x, y = y)) +
  labs(
    title = "Rohe Prävalenz für ausgewählte Diagnosekodierungen [R96]",
    subtitle = "Quelle: zi.de",
    y = "Angaben in %",
    x = "Jahr"
  ) +
  geom_smooth(
    data = df %>% filter(x <= as.Date("2019/1/1")),
    method = "lm_right",
    fullrange = TRUE,
    se = TRUE,
    level = .99,
    linetype = "dashed",
  ) +
  geom_line(color = "#5383EC", linewidth = 1) +
  scale_y_continuous(labels = comma_format(decimal.mark = ",")) +
  twitter_theme() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
