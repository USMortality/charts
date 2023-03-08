source("lib/common.r")

data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") |>
  as_tibble(data)

data_deu <- data |>
  filter(iso_code == "DEU") |>
  select(
    date,
    new_deaths_smoothed,
    people_vaccinated_per_hundred
  ) |>
  setNames(c("date", "deaths", "vaccinated")) |>
  mutate(
    date = date(date),
    vaccinated = vaccinated / 100
  ) |>
  as_tsibble(index = date) |>
  mutate(
    unvaccinated = 1 - vaccinated,
    deaths_unvaccinated = deaths * unvaccinated,
    deaths_vaccinated = deaths * vaccinated
  ) |>
  select(-2) |>
  filter(!is.na(vaccinated))

ggplot(
  data_deu |>
    select(1, 4, 5) |>
    pivot_longer(
      cols = starts_with("deaths_"),
      names_to = "vaccination_status",
      names_prefix = "deaths_",
      values_to = "deaths",
      values_drop_na = TRUE
    ),
  aes(x = date, y = deaths, fill = vaccination_status)
) +
  labs(
    title = "COVID-19 Deaths by proportional Vaccination Status [Germany]",
    subtitle = "HYPOTHETICAL | Source: OurWorldInData.org",
    y = "COVID-19 Deaths",
    x = "Week of Year"
  ) +
  geom_area() +
  twitter_theme() +
  watermark(df$date, df$cmr) +
  # scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
  theme(legend.position = "top") +
  scale_x_yearweek(date_breaks = "2 month") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))

plot_chart <- function(data, delay) {
  ggplot(data, aes(x = date, y = ve)) +
    labs(
      title = "COVID-19 VE/death by EVEN Vaccination Status [Germany]",
      subtitle = paste(delay, "HYPOTHETICAL | Source: OurWorldInData.org", sep = " | "),
      y = "COVID-19 Vaccine Efficacy (Death)",
      x = "Month of Year"
    ) +
    geom_line() +
    twitter_theme() +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 1)
    ) +
    watermark(df$date, df$cmr) +
    # scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
    theme(legend.position = "top") +
    scale_x_yearweek(date_breaks = "2 month") +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
}

ve_data <- data_deu |>
  mutate(ve = 1 - (
    deaths_vaccinated / vaccinated) / (deaths_unvaccinated / unvaccinated
  )) |>
  select(date, ve)
plot_chart(ve_data, "daily")

ve_data_all_time <- data_deu |>
  mutate(ve = 1 - (
    cumsum(deaths_vaccinated) / vaccinated) /
    (cumsum(deaths_unvaccinated) / unvaccinated
    )) |>
  select(date, ve)
plot_chart(ve_data_all_time, "cumulative all time")

ve_data_4w <- data_deu |>
  mutate(ve = 1 - (
    SMA(deaths_vaccinated, n = 30) / vaccinated) /
    (SMA(deaths_unvaccinated, n = 30) / unvaccinated
    )) |>
  select(date, ve)
plot_chart(ve_data_4w, "last cum. 30 days")
