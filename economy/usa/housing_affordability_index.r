source("lib/common.r")
options(warn = 1)

# 30-Year Fixed Rate Mortgage Average in the United States (MORTGAGE30US)
mortgage_rate <- read.csv(
  paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=639&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=MORTGAGE30US&scale=left&cosd=1970-01-12&coed=",
    Sys.Date(),
    "&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Weekly%2C%20Ending%20Thursday&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
    Sys.Date(),
    "&revision_date=",
    Sys.Date(),
    "&nd=1971-04-02"
  )
) |>
  setNames(c("date", "rate")) |>
  as_tibble() |>
  mutate(
    date = as.Date(date),
    rate = rate / 100
  ) |>
  mutate(
    date = make_yearweek(year = year(date), week = week(date))
  ) |>
  group_by(date) |>
  summarise(rate = mean(rate))

chart <-
  ggplot(mortgage_rate, aes(x = date, y = rate)) +
  labs(
    title = "30-Year Fixed Rate Mortgage Average [USA]",
    subtitle = "Data Source: St. Louis Fed",
    x = "Week of Year", y = ""
  ) +
  geom_line(color = "#5383EC", linewidth = 1.5) +
  twitter_theme() +
  watermark() +
  expand_limits(x = 0, y = 0) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "black",
    linetype = "dashed"
  ) +
  scale_x_yearweek(date_breaks = "5 year", date_labels = "%Y") +
  ggrepel::geom_label_repel(
    data = tail(mortgage_rate, n = 1) |> mutate(str = paste0(
      date, ": ", sprintf("%0.1f%%", rate * 100)
    )),
    aes(label = str),
    nudge_y = 0.1,
    segment.color = "grey50",
  )

save_chart(chart, "economy/usa/30y_fixed_mortgage")

# Median Home Prices
median_home_price <- read.csv(
  paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1138&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=MSPNHSUS&scale=left&cosd=1963-01-01&coed=",
    Sys.Date(),
    "&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
    Sys.Date(),
    "&revision_date=",
    Sys.Date(),
    "&nd=1963-01-01"
  )
) |>
  setNames(c("date", "price")) |>
  mutate(date = as.Date(date)) |>
  as_tsibble(index = date) |>
  fill_gaps() |>
  mutate(price = zoo::na.approx(price)) |>
  as_tibble() |>
  mutate(
    date = make_yearweek(year = year(date), week = week(date))
  ) |>
  group_by(date) |>
  summarise(price = mean(price))

chart <-
  ggplot(median_home_price, aes(x = date, y = price)) +
  labs(
    title = "Median Home Prices [USA]",
    subtitle = "Data Source: St. Louis Fed",
    x = "Week of Year", y = ""
  ) +
  geom_line(color = "#5383EC", linewidth = 1.5) +
  twitter_theme() +
  watermark() +
  scale_y_continuous(
    trans = "log2",
    labels = scales::dollar_format()
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "black",
    linetype = "dashed"
  ) +
  scale_x_yearweek(date_breaks = "5 year", date_labels = "%Y") +
  ggrepel::geom_label_repel(
    data = tail(median_home_price, n = 1) |> mutate(str = paste0(
      date, ": ", as_usd(price)
    )),
    aes(label = str),
    nudge_y = 0.1,
    segment.color = "grey50",
  )

save_chart(chart, "economy/usa/median_home_price")

# Median Family Income in the United States (MEFAINUSA646N)
median_income <- read.csv(
  paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1138&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=MEFAINUSA646N&scale=left&cosd=1953-01-01&coed=",
    Sys.Date(),
    "2021-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
    Sys.Date(),
    "&revision_date=",
    Sys.Date(),
    "&nd=1963-01-01"
  )
) |>
  setNames(c("date", "income")) |>
  as_tibble() |>
  mutate(date = as.Date(date)) |>
  mutate(date = year(date))

ts <- median_income |> as_tsibble(index = date)
fc <- tail(ts, n = 5) |>
  model(TSLM(income ~ trend())) |>
  forecast(h = 3)

chart <-
  ggplot(ts, aes(x = date, y = income)) +
  labs(
    title = "Median Family Income [USA]",
    subtitle = "Data Source: St. Louis Fed",
    x = "Week of Year", y = ""
  ) +
  geom_line(color = "#5383EC", linewidth = 1.5) +
  twitter_theme() +
  watermark() +
  scale_y_continuous(
    trans = "log2",
    labels = scales::dollar_format()
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "black",
    linetype = "dashed"
  ) +
  scale_x_continuous(breaks = seq(min(ts$date), max(ts$date), by = 6)) +
  ggrepel::geom_label_repel(
    data = tail(ts, n = 1) |> mutate(str = paste0(
      date, ": ", as_usd(income)
    )),
    aes(label = str),
    nudge_y = 0.1,
    segment.color = "grey50",
  )

save_chart(chart, "economy/usa/median_family_income")

median_income_fc <- tibble(date = fc$date, income = fc$.mean)
median_income <- rbind(median_income, median_income_fc) |>
  distinct(date, .keep_all = TRUE)

median_income <- median_income |>
  mutate(
    date = as.Date(paste0(date, "-01-01"))
  ) |>
  as_tsibble(index = date) |>
  fill_gaps() |>
  mutate(income = zoo::na.approx(income)) |>
  as_tibble() |>
  mutate(
    date = make_yearweek(year = year(date), week = week(date))
  ) |>
  group_by(date) |>
  summarise(income = mean(income))

# Final input df
df <-
  median_income |>
  inner_join(median_home_price, by = c("date")) |>
  inner_join(mortgage_rate, by = c("date"))

# Calculate index
pmt <- function(rate, nper, pv, fv = 0, type = 0) {
  pmt <- ifelse(rate != 0,
    (rate * (fv + pv * (1 + rate)^nper)) /
      ((1 + rate * type) * (1 - (1 + rate)^nper)),
    (-1 * (fv + pv) / nper)
  )

  return(pmt)
}

ts <- df |>
  mutate(monthly_pi = -pmt(rate / 12, 30 * 12, price * .8)) |>
  mutate(percent_income = (monthly_pi * 12) / income) |>
  mutate(q_income = monthly_pi * 4 * 12) |>
  mutate(a_index = income / q_income) |>
  as_tsibble(index = date)

chart <-
  ggplot(ts, aes(x = date, y = a_index)) +
  labs(
    title = "Housing Affordability Index [USA]",
    subtitle = "Data Source: St. Louis Fed | Methodology: NAR",
    x = "Week of Year", y = ""
  ) +
  geom_line(color = "#5383EC", linewidth = 1.5) +
  twitter_theme() +
  watermark() +
  scale_y_continuous(labels = scales::percent, limits = c(0.25, 1.75)) +
  scale_x_yearweek(date_breaks = "5 year", date_labels = "%Y") +
  ggrepel::geom_label_repel(
    data = tail(ts, n = 1) |> mutate(str = paste0(
      date, ": ", sprintf("%0.1f%%", a_index * 100)
    )),
    aes(label = str),
    nudge_y = 0.1,
    segment.color = "grey50",
  )

save_chart(chart, "economy/usa/housing_affordability_index")

rent_raw <- read.csv(
  paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1138&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CUUR0000SEHA&scale=left&cosd=1914-12-01&coed=",
    Sys.Date(),
    "&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=",
    Sys.Date(),
    "&revision_date=",
    Sys.Date(),
    "&nd=1971-04-02"
  )
)

ts <- rent_raw |>
  setNames(c("date", "rent")) |>
  mutate(date = as.Date(date)) |>
  as_tsibble(index = date) |>
  fill_gaps() |>
  mutate(rent = zoo::na.approx(rent)) |>
  as_tibble() |>
  mutate(
    date = make_yearweek(year = year(date), week = week(date))
  ) |>
  group_by(date) |>
  summarise(rent = mean(rent, na.rm = TRUE)) |>
  as_tsibble(index = date)

chart <-
  ggplot(ts, aes(x = date, y = rent)) +
  labs(
    title = "CPI: Rent of Primary Residence City [USA]",
    subtitle = "Index 1982-1984=100 | Data Source: St. Louis Fed",
    x = "Month of Year", y = "Index"
  ) +
  geom_line(color = "#5383EC", linewidth = 1.5) +
  twitter_theme() +
  watermark() +
  scale_y_continuous(
    trans = "log2"
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "black",
    linetype = "dashed"
  ) +
  scale_x_yearweek(date_breaks = "10 year", date_labels = "%Y") +
  ggrepel::geom_label_repel(
    data = tail(ts, n = 1) |> mutate(str = paste0(
      date, ": ", round(rent)
    )),
    aes(label = str),
    nudge_y = 0.1,
    segment.color = "grey50",
  )

save_chart(chart, "economy/usa/rent_index")
