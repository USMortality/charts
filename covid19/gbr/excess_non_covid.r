source("lib/common.r")

source("covid19/gbr/deaths_by_vaxx.r")

make_chart <- function(data, title) {
  ggplot(data, aes(x = date, y = deaths, group = type, fill = type)) +
    labs(
      title = title,
      subtitle = "Source: ons.gov.uk",
      x = "Month of Year",
      y = "Excess Deaths"
    ) +
    geom_bar(
      stat = "identity",
      position = "stack"
    ) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
    theme(legend.position = "top")
}

data <- read_excel(
  "./data/uk_acm.xlsx",
  sheet = "1",
  range = "A6:H99"
)

df <- data |>
  select(1, 6, 7, 8) |>
  setNames(c("week", "deaths_2022", "deaths_2021", "baseline")) %>%
  pivot_longer(
    cols = starts_with("deaths_"), names_prefix = "deaths_",
    names_to = "year", values_to = "deaths"
  ) |>
  mutate(date = make_yearweek(
    year = as.numeric(year), week = as.numeric(week)
  ), .before = "baseline") |>
  select(-week, -year) |>
  mutate(excess = deaths - baseline) |>
  filter(!is.na(date)) |>
  arrange(date) |>
  getDailyFromWeekly("excess")

quarterly_excess <- df |>
  select(1, 4) |>
  mutate(date = yearquarter(date)) |>
  group_by(date) |>
  summarise(deaths = sum(excess, na.rm = TRUE)) |>
  mutate(deaths = ifelse(deaths > 0, deaths, 0)) |>
  filter(year(date) < 2023)
quarterly_excess$type <- "_excess"

quarterly_covid <- covid |>
  mutate(date = yearquarter(date)) |>
  group_by(date, vaccination_status) |>
  summarise(deaths = sum(covid_deaths, na.rm = TRUE))

# Unvaccinated
quarterly_covid_unvaxx <- quarterly_covid |>
  filter(vaccination_status == "Unvaccinated") |>
  select(1, 3)
quarterly_covid_unvaxx$type <- "unvaxx_covid19"

make_chart(
  rbind(quarterly_excess, quarterly_covid_unvaxx) |>
    filter(date > make_yearquarter(year = 2021, quarter = 1)),
  "Quarterly All-Cause Excess & Unvaxx COVID-19 Deaths [UK]"
)

# Vaccinated
quarterly_covid_unvaxx <- quarterly_covid |>
  filter(vaccination_status == "Ever vaccinated") |>
  select(1, 3)
quarterly_covid_unvaxx$type <- "vaxx_covid19"

make_chart(
  rbind(quarterly_excess, quarterly_covid_unvaxx) |>
    filter(date > make_yearquarter(year = 2021, quarter = 1)),
  "Quarterly All-Cause Excess & Vaccination COVID-19 Deaths [UK]"
)

# All
quarterly_covid_all <- quarterly_covid |>
  group_by(date) |>
  summarise(deaths = sum(deaths))
quarterly_covid_all$type <- "covid19"

make_chart(
  rbind(quarterly_excess, quarterly_covid_all) |>
    filter(date > make_yearquarter(year = 2021, quarter = 1)),
  "Quarterly All-Cause Excess & COVID-19 Deaths [UK]"
)

# Combined
colnames(quarterly_covid)[2] <- "type"
quarterly_covid$place <- "COVID-19"
quarterly_excess$type <- "Excess"
quarterly_excess$place <- "Excess"
quarterly_excess <- quarterly_excess |> relocate(type, .before = deaths)

dx <- rbind(quarterly_excess, quarterly_covid) |>
  filter(date >= make_yearquarter(year = 2021, quarter = 2))

ggplot(dx, aes(x = place, y = deaths, group = type, fill = type)) +
  labs(
    title = "Quarterly All-Cause Excess & COVID-19 Deaths by Vaccination Status [UK]",
    subtitle = "Source: ons.gov.uk",
    x = "Month of Year",
    y = "Excess Deaths"
  ) +
  geom_bar(stat = "identity", width = 1, position = "stack") +
  twitter_theme() +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
  theme(legend.position = "top") +
  facet_grid(. ~ date) +
  scale_fill_manual(
    values = c(
      "#55BAC2",
      "#DC7D74",
      "#000000"
    ),
    breaks = c("Unvaccinated", "Ever vaccinated", "Excess")
  )
