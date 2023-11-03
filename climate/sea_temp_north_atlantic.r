source("lib/common.r")

#  Load Data
req <- GET(
  paste0(
    "https://climatereanalyzer.org/clim/sst_daily/json/",
    "oisst2.1_natlan1_sst_day.json"
  )
)
stop_for_status(req)
data <- content(req, "text") |>
  fromJSON()

fun <- function(a) {
  start_date <- paste0(a$date[1], "-01-01")
  end_date <- paste0(a$date[1], "-12-31")
  dates <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  if (length(dates) == 365) dates <- c(dates, NA)
  a$date <- dates
  a
}

df <- data |>
  as_tibble() |>
  mutate(name = suppress_warnings(
    as.integer(name),
    "NAs introduced by coercion"
  )) |>
  filter(!is.na(name)) |>
  unnest(cols = c(data)) |>
  mutate(date = name) |>
  nest(data = !c(name)) |>
  mutate(data = lapply(data, fun)) |>
  unnest(cols = c(data)) |>
  select(date, data) |>
  setNames(c("date", "temp"))

# Daily
df |>
  filter(!is.na(temp)) |>
  as_tsibble(index = date) |>
  autoplot()

# Monthly
ts <- df |>
  mutate(date = yearmonth(date)) |>
  group_by(date) |>
  summarise(temp = mean(temp)) |>
  filter(!is.na(temp)) |>
  as_tsibble(index = date)
chart <- ts |>
  filter(date <= make_yearmonth(year = 2019, month = 12)) |>
  model(TSLM(temp ~ trend() + season())) |>
  forecast(h = 4 * 12) |>
  autoplot(ts) +
  labs(
    y = "Temperature (C)",
    x = "Month",
    title = "North Atlantic Sea Surface Temperature",
    subtitle = paste(
      "Forecast based on <2020 linear trend.",
      "Source: climatereanalyzer.org",
      sep = " | "
    )
  ) +
  twitter_theme() +
  watermark()

save_chart(chart, "climate/sea_temp_north_atlantic_monthly")

# Yearly
ts <- df |>
  mutate(date = year(date)) |>
  group_by(date) |>
  summarise(temp = mean(temp)) |>
  filter(!is.na(temp)) |>
  as_tsibble(index = date)

chart <- ts |>
  filter(date <= 2019) |>
  model(TSLM(temp ~ trend())) |>
  forecast(h = 4) |>
  autoplot(ts) +
  labs(
    y = "Temperature (C)",
    x = "Year",
    title = "North Atlantic Sea Surface Temperature",
    subtitle = paste(
      "Forecast based on <2020 linear trend.",
      "Source: climatereanalyzer.org",
      sep = " | "
    )
  ) +
  twitter_theme() +
  watermark()

save_chart(chart, "climate/sea_temp_north_atlantic_yearly")
