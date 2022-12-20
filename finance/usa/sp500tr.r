source("lib/common.r")

sdate <- as.Date("1900-01-01")
edate <- as.Date("2022-12-31")

sp500 <- getSymbols("^SP500TR", from = sdate, to = edate, auto.assign = F)
df <- as.data.frame(sp500)
df$date <- row.names(df)

df <- df %>%
  select(date, SP500TR.Close) %>%
  mutate(close = SP500TR.Close) %>%
  select(date, close) %>%
  mutate(date = as.Date(date))

save_csv(df, "finance/usa/sp500tr")

chart <-
  ggplot(as_tsibble(df, index = date), aes(x = date, y = close)) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(trans = "log2") +
  labs(
    title = "SP500 Total Return (Log Scale)",
    subtitle = "Source: ^SP500TR",
    x = "Date",
    y = "Points"
  ) +
  geom_line(color = "#5383EC", linewidth = 1) +
  twitter_theme() +
  watermark(ts$date, ts$close) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "black",
    linetype = "dashed"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))

save_chart(chart, "finance/usa/sp500tr")
