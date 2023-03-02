source("lib/common.r")

data <- read.csv("./data/covid19_usa_live_births.csv")

df <- as_tibble(data) %>%
  filter(State == "UNITED STATES") %>%
  filter(Indicator == "Number of Live Births") %>%
  filter(Period == "Monthly") %>%
  mutate(yearmonth = yearmonth(paste0(Year, "-", Month))) %>%
  select(yearmonth, Data.Value) %>%
  setNames(c("yearmonth", "births"))

save_csv(df, "population/usa/live_births")

# Make Chart
chart <-
  ggplot(as_tsibble(df, index = yearmonth), aes(x = yearmonth, y = births)) +
  labs(
    title = "Live Births [USA]",
    subtitle = "Source: cdc.gov",
    x = "Month of Year",
    y = "Live Births"
  ) +
  geom_line(color = "#5383EC", linewidth = 1.5) +
  geom_hline(yintercept = 0) +
  twitter_theme() +
  watermark(df$yearmonth, df$value_p) +
  scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3))

save_chart(chart, "population/usa/live_births")
