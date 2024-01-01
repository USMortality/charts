source("lib/common.r")

data0 <- read_csv(
  "./data_static/Natality_2016_2022_expanded.csv",
  col_types = "ciicii"
)
data <- read_csv("./data/covid19_usa_live_births.csv", col_types = "ciccci")

df0 <- data0 |>
  select(3, 5, 6) |>
  setNames(c("year", "month", "births")) |>
  mutate(yearmonth = make_yearmonth(year, month)) |>
  select(yearmonth, births)

df <- data |>
  filter(State == "UNITED STATES") |>
  filter(Indicator == "Number of Live Births") |>
  filter(Period == "Monthly") |>
  mutate(yearmonth = yearmonth(paste0(Year, "-", Month))) |>
  select(yearmonth, `Data Value`) |>
  setNames(c("yearmonth", "births")) |>
  rbind(df0) |>
  arrange(yearmonth) |>
  distinct(yearmonth, .keep_all = TRUE) |>
  filter(!is.na(yearmonth))

save_csv(df, "population/usa/live_births")

# Make Chart
ts <- df |> as_tsibble(index = yearmonth)

fc_start_month <- make_yearmonth(year = 2020, month = 3)
fc_months <- max(ts$yearmonth) - fc_start_month

fit <- ts |>
  filter(yearmonth < fc_start_month) |>
  model(TSLM(births ~ season() + trend()))
fc <- forecast(fit, h = paste0(as.numeric(fc_months), " months"))
chart <- fc |> autoplot(ts, level = 95) +
  labs(
    title = "Actual & Forecasts of Monthly Live Births [USA]",
    subtitle = paste0(
      "Source: CDC/Wonder",
      "Illness; Training Period: 2016/1 - 2020/2; ",
      "95% PI"
    ),
    x = "Month of Year",
    y = "Live Births"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  watermark(min(ts$yearmonth), 400) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y Jan") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
  coord_cartesian(ylim = c(min(ts$births), max(ts$births)))

save_chart(chart, "population/usa/live_births")
