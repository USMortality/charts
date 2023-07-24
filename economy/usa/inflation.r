source("lib/common.r")
library(ggrepel)

getData <- function(from, to) {
  req <- POST("https://api.bls.gov/publicAPI/v2/timeseries/data/",
    add_headers("Content-Type" = "application/json"),
    body = paste0(
      '{"seriesid": ["CUUR0000SA0"], "startyear":"',
      from,
      '", "endyear":"',
      to,
      '"}'
    )
  )
  stop_for_status(req)
  data <- content(req, "text") |>
    fromJSON()

  as_tibble(data$Results$series$data[[1]]) |>
    mutate(year = as.integer(year)) |>
    mutate(period = as.integer(right(period, 2))) |>
    mutate(value = as.double(value)) |>
    mutate(yearmonth = yearmonth(paste0(year, "-", period))) |>
    mutate(value_ref = lead(value, 12)) |>
    mutate(value_p = value / value_ref - 1) |>
    filter(!is.na(value_ref)) |>
    select(yearmonth, value_p)
}

currentYear <- year(Sys.Date())
#  Load Data
df <- rbind(
  getData(currentYear - 9, currentYear),
  getData(currentYear - 19, currentYear - 10),
  getData(currentYear - 29, currentYear - 20),
  getData(currentYear - 39, currentYear - 30),
  getData(currentYear - 49, currentYear - 40),
  getData(currentYear - 59, currentYear - 50),
  getData(currentYear - 69, currentYear - 60),
  getData(currentYear - 79, currentYear - 70),
  getData(currentYear - 89, currentYear - 80),
  getData(currentYear - 99, currentYear - 90)
)

save_csv(df, "economy/usa/inflation")

# Make Chart
# chart <-
ggplot(as_tsibble(df, index = yearmonth), aes(x = yearmonth, y = value_p)) +
  labs(
    title = "Inflation Rate [USA]",
    subtitle = "Source: bls.gov",
    x = "Month of Year",
    y = "12M Rate of Increase"
  ) +
  geom_line(color = "#5383EC", linewidth = 1.5) +
  geom_hline(yintercept = 0) +
  geom_hline(aes(yintercept = 0.02), color = "#58A65C", linetype = "dashed") +
  twitter_theme() +
  watermark() +
  scale_y_continuous(labels = scales::percent) +
  geom_label_repel(
    data = head(df, n = 1) |> mutate(str = paste0(
      yearmonth,
      ": ",
      sprintf("%0.1f%%", value_p * 100)
    )),
    aes(label = str),
    nudge_y = 0.1,
    segment.color = "grey50",
  )

save_chart(chart, "economy/usa/inflation")
