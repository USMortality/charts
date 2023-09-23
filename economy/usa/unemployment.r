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

ts <- as_tsibble(df, index = yearmonth)
# Make Chart
chart <-
  ggplot(ts, aes(x = yearmonth, y = value_p)) +
  labs(
    title = "Unemployment Rate [USA]",
    subtitle = "Source: bls.gov",
    x = "Month of Year",
    y = "Unemployment Rate"
  ) +
  geom_line(color = "#5383EC", linewidth = 1.5) +
  geom_hline(yintercept = 0) +
  twitter_theme() +
  watermark() +
  scale_y_continuous(labels = scales::percent) +
  ggrepel::geom_label_repel(
    data = tail(ts, n = 1) |> mutate(str = paste0(
      yearmonth, ": ",
      sprintf("%0.1f%%", value_p * 100)
    )),
    aes(label = str),
    nudge_y = 0.1,
    segment.color = "grey50",
  )
save_chart(chart, "economy/usa/unemployment")
