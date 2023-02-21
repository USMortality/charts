source("lib/common.r")

deaths_weekly <- read.csv("https://s3.mortality.watch/data/mortality/world_weekly.csv") |>
  as_tibble() |>
  filter(jurisdiction == "Switzerland") |>
  mutate(
    date = make_yearweek(
      year = as.integer(left(date, 4)), week = as.integer(right(date, 2))
    )
  ) |>
  select(date, deaths_excess)

data <- read.csv("./data_static/COVID19Death_vaccpersons_AKL10_w.csv") |>
  as_tibble() |>
  mutate(date = make_yearweek(
    year = as.integer(left(date, 4)),
    week = as.integer(right(date, 2))
  ))

df <- data |>
  filter(
    # date == make_yearweek(2021, 50),
    altersklasse_covid19 == "all"
  ) |>
  select(date, vaccination_status, entries) |>
  setNames(c(
    "date",
    "vaccination_status",
    "deaths"
  )) |>
  mutate(
    vaccination_status = case_when(
      vaccination_status == "not_vaccinated" ~ "unvaccinated",
      vaccination_status == "unknown" ~ "unknown",
      .default = "vaccinated"
    )
  ) |>
  group_by(date, vaccination_status) |>
  summarise(deaths = sum(deaths, na.rm = TRUE)) |>
  ungroup()

excess_deaths <- deaths_weekly |>
  filter(date >= min(df$date), date <= max(df$date))

df2 <- df |>
  pivot_wider(
    names_from = vaccination_status,
    values_from = deaths
  ) |>
  mutate(covid = unvaccinated + vaccinated + unknown) |>
  inner_join(excess_deaths, by = c("date")) |>
  mutate(deaths_excess = ifelse(deaths_excess < 0, 0, deaths_excess)) |>
  mutate(non_covid_excess = deaths_excess - covid) |>
  mutate(non_covid_excess = ifelse(
    non_covid_excess < 0, 0, non_covid_excess
  )) |>
  select(date, unvaccinated, vaccinated, unknown, non_covid_excess) |>
  setNames(c(
    "date",
    "COVID-19 (Unvaccinated)",
    "COVID-19 (Vaccinated)",
    "COVID-19 (Unknown)",
    "Non COVID-19 Excess"
  )) |>
  pivot_longer(cols = !date, names_to = "death_type", values_to = "deaths")

make_chart <- function(df, st) {
  ggplot(
    df,
    aes(
      x = date,
      y = deaths,
      fill = death_type
    )
  ) +
    labs(
      title = "Weekly COVID-19 Deaths by Vaccination Status [Switzerland]",
      subtitle = st,
      x = "Week of Year",
      y = "COVID-19 Deaths"
    ) +
    geom_area() +
    twitter_theme() +
    scale_x_yearweek(date_breaks = "4 weeks", date_labels = "%Y/%W") +
    scale_fill_manual(
      name = "",
      values = c(
        "#A3A02C",
        "#A33F1C",
        "#404E64",
        "#41F06A"
      )
    ) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)
    ) +
    ylim(c(0, 450))
}

make_chart(
  df2 |> filter(death_type != "Non COVID-19 Excess"),
  "Source: www.covid19.admin.ch"
)

df3 <- df2 |>
  getDailyFromWeekly("deaths") |>
  mutate(quarter = yearquarter(date)) |>
  select(quarter, death_type, deaths) |>
  group_by(quarter, death_type) |>
  summarise(deaths = round(sum(deaths, na.rm = TRUE)))

ggplot(
  df3,
  aes(
    x = quarter
  )
) +
  labs(
    title = "Weekly COVID-19 Deaths by Vaccination Status [Switzerland]",
    x = "Week of Year",
    y = "COVID-19 Deaths"
  ) +
  geom_bar(stat = "deaths") +
  twitter_theme() +
  scale_x_yearweek(date_breaks = "4 weeks", date_labels = "%Y/%W") +
  scale_fill_manual(
    name = "",
    values = c(
      "#A3A02C",
      "#A33F1C",
      "#404E64",
      "#41F06A"
    )
  ) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)
  ) +
  ylim(c(0, 450))
