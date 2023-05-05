source("lib/common.r")

#  Load Data
req <- POST("https://api.bls.gov/publicAPI/v2/timeseries/data/",
  add_headers("Content-Type" = "application/json"),
  body = paste0(
    '{"seriesid": ["LNS14000000"], "startyear":"',
    year(Sys.Date()) - 9,
    '", "endyear":"',
    year(Sys.Date()),
    '"}'
  )
)
stop_for_status(req)
data <- content(req, "text") |> fromJSON()

# Transform
df <- as_tibble(data$Results$series$data[[1]]) |>
  mutate(year = as.integer(year)) |>
  mutate(value_p = as.double(value) / 100) |>
  mutate(yearmonth = yearmonth(paste0(year, "-", right(period, 2)))) |>
  select(yearmonth, value_p)

save_csv(df, "economy/usa/unemployment")

# Make Chart
chart <-
  ggplot(as_tsibble(df, index = yearmonth), aes(x = yearmonth, y = value_p)) +
  labs(
    title = "Unemployment Rate [USA]",
    subtitle = "Source: bls.gov",
    x = "Month of Year",
    y = "Unemployment Rate"
  ) +
  geom_line(color = "#5383EC", linewidth = 1.5) +
  geom_hline(yintercept = 0) +
  twitter_theme() +
  watermark(df$yearmonth, df$value_p) +
  scale_y_continuous(labels = scales::percent)

save_chart(chart, "economy/usa/unemployment")
