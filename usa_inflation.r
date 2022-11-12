path <- strsplit(commandArgs(trailingOnly = FALSE)[4], "--file=")[[1]][2]
path <- ifelse(is.na(path), ".", dirname(path))
source(paste(path, "_deps.r", sep = "/"))

#  Load Data
req <- POST("https://api.bls.gov/publicAPI/v2/timeseries/data/",
  add_headers("Content-Type" = "application/json"),
  body = '{"seriesid": ["CUUR0000SA0"], "startyear":"2013", "endyear":"2022"}'
)
stop_for_status(req)
data <- content(req, "text") %>% fromJSON()

# Transform
df <- as_tibble(data$Results$series$data[[1]]) %>%
  mutate(year = as.integer(year)) %>%
  mutate(period = as.integer(str_sub(period, start = -2))) %>%
  mutate(value = as.double(value)) %>%
  mutate(yearmonth = yearmonth(paste0(year, "-", period))) %>%
  mutate(value_ref = lead(value, 12)) %>%
  mutate(value_p = value / value_ref - 1) %>%
  filter(!is.na(value_ref)) %>%
  select(yearmonth, value_p)

save_csv(df, "usa_inflation")

# Make Chart
chart <-
  ggplot(as_tsibble(df, index = yearmonth), aes(x = yearmonth, y = value_p)) +
  labs(
    title = "Inflation Rate [USA]",
    subtitle = "Source: bls.gov",
    x = "Month of Year",
    y = "12M Rate of Increase"
  ) +
  geom_line(color = "#5383EC", size = 1.5) +
  geom_hline(yintercept = 0) +
  geom_hline(aes(yintercept = 0.02), color = "#58A65C", linetype = "dashed") +
  twitter_theme() +
  watermark(df$yearmonth, df$value_p) +
  scale_y_continuous(labels = scales::percent)

save_chart(chart, "usa_inflation")
