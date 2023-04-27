source("lib/common.r")

data1 <- as_tibble(read.csv("./data_static/usa_deaths_causes_2014_2019.csv"))
data2 <- as_tibble(read.csv("./data/usa_deaths_causes_2020_n.csv"))
population <- read_remote("population/usa/six_age_bands.csv")

a <- data1 |>
  filter(Jurisdiction.of.Occurrence == "United States") |>
  select(Week.Ending.Date, Malignant.neoplasms..C00.C97.) |>
  mutate(date = lubridate::mdy(Week.Ending.Date))

b <- data2 |>
  filter(Jurisdiction.of.Occurrence == "United States") |>
  select(Week.Ending.Date, Malignant.neoplasms..C00.C97.) |>
  mutate(date = lubridate::ymd(Week.Ending.Date))

us_population <- population |>
  filter(jurisdiction == "United States") |>
  filter(age_group == "all")

data <- rbind(a, b) |>
  mutate(date = date(date)) |>
  mutate(year = year(date)) |>
  mutate(deaths = as.integer(gsub(",", "", Malignant.neoplasms..C00.C97.))) |>
  head(-4) |> # Exclude last 4 weeks, b/c of reporting delay.
  left_join(us_population, by = "year") |>
  getDailyFromWeekly(c("deaths")) |>
  mutate(mortality = deaths / population * 100000) |>
  select(date, year, deaths, mortality) |>
  as_tsibble(index = date)

save_csv(data, paste("mortality", "usa", "cancer", sep = "/"))

# Weekly
w_data <- data |>
  index_by(yearweek(date)) |>
  summarise(sum(mortality)) |>
  head(-1) |>
  tail(-1) |>
  setNames(c("date", "mortality"))
chart1 <-
  ggplot(w_data, aes(x = date, y = mortality)) +
  labs(
    title = paste0("Weekly Cancer Mortality [USA]"),
    subtitle = "Source: CDC",
    y = "Mortality (Deaths/100k)",
    x = "Week of Year"
  ) +
  geom_line(color = "#5383EC", linewidth = 0.5) +
  twitter_theme() +
  scale_x_yearweek(date_breaks = "1 year", date_labels = "%Y") +
  watermark(df$yearmonth, df$value_p) +
  geom_smooth(span = 52 / count(w_data), level = FALSE, color = "#000000")

# STL
chart2 <-
  w_data |>
  model(STL(mortality)) |>
  components() |>
  autoplot() +
  labs(
    title = "Weekly Cancer Mortality - STL Decomposition [USA]",
    subtitle = "Source: CDC"
  ) + twitter_theme() +
  watermark(df$date, df$value_p) +
  scale_x_yearweek(date_breaks = "1 year", date_labels = "%Y")

# Monthly
m_data <- data |>
  index_by(yearmonth(date)) |>
  summarise(sum(mortality)) |>
  ungroup() |>
  head(-1) |>
  setNames(c("date", "mortality"))

chart3 <-
  m_data |>
  ggplot(aes(x = date, y = mortality)) +
  labs(
    title = paste0("Monthly Cancer Mortality [USA]"),
    subtitle = "Source: CDC",
    y = "Mortality (Deaths/100k)",
    x = "Month of Year"
  ) +
  geom_point(color = "#bbbbbb") +
  twitter_theme() +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
  watermark(df$yearmonth, df$value_p) +
  geom_smooth(
    data = m_data |> filter(year(date) %in% seq(2015, 2020)),
    method = "lm_right",
    fullrange = TRUE,
    col = "#249C31"
  ) +
  geom_smooth(
    span = (24 / count(m_data)),
    level = FALSE,
    color = "#000000"
  )

# Quarterly
q_data <- data |>
  index_by(yearquarter(date)) |>
  summarise(sum(mortality)) |>
  setNames(c("date", "mortality"))

chart4 <- q_data |>
  setNames(c("date", "mortality")) |>
  head(-1) |>
  tail(-1) |>
  ggplot(aes(x = date, y = mortality)) +
  labs(
    title = paste0("Quarterly Cancer Mortality [USA]"),
    subtitle = "Source: CDC",
    y = "Mortality (Deaths/100k)",
    x = "Quarter of Year"
  ) +
  geom_point(color = "#5383EC") +
  twitter_theme() +
  scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y") +
  watermark(df$yearmonth, df$value_p) +
  geom_smooth(
    data = q_data |> filter(year(date) %in% seq(2015, 2020)),
    method = "lm_right",
    fullrange = TRUE,
    col = "#249C31"
  ) +
  geom_smooth(
    span = (4 * 4 / count(q_data)),
    level = FALSE,
    color = "#000000"
  )

save_chart(chart1, paste("mortality", "usa", "cancer", "weekly", sep = "/"))
save_chart(chart2, paste("mortality", "usa", "cancer", "stl", sep = "/"))
save_chart(chart3, paste("mortality", "usa", "cancer", "monthly", sep = "/"))
save_chart(chart4, paste("mortality", "usa", "cancer", "quarterly", sep = "/"))

save_collage(
  chart1, chart2, chart3, chart4,
  path = paste("mortality", "usa", "cancer", sep = "/")
)
