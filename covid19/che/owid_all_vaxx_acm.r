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
  filter(altersklasse_covid19 == "all") |>
  select(date, vaccination_status, entries) |>
  setNames(c(
    "date",
    "vaccination_status",
    "deaths"
  )) |>
  mutate(
    vaccination_status = case_when(
      vaccination_status == "not_vaccinated" ~ "3_unvaccinated",
      vaccination_status == "unknown" ~ "1_unknown",
      vaccination_status %in% c(
        "fully_vaccinated",
        "partially_vaccinated"
      ) ~ "2_vaccinated"
    )
  ) |>
  group_by(date, vaccination_status) |>
  filter(!is.na(vaccination_status)) |>
  summarise(deaths = sum(deaths, na.rm = TRUE)) |>
  ungroup()

make_chart <- function(df, st) {
  ggplot(
    df,
    aes(
      x = date,
      y = deaths,
      fill = vaccination_status
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
    # scale_y_continuous(limits = c(0, 200)) +
    scale_fill_manual(
      name = "",
      values = c(
        # "#a3401cc3",
        # "#BA5A16",
        "#A3A02C",
        # "#404E64",
        "#A33F1C",
        "#404E64"
      )
    ) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)
    )
}

# Actual reported
make_chart(df, "Source: www.covid19.admin.ch")
make_chart(
  df |> filter(date >= make_yearweek(year = 2022, week = 1)),
  "Source: www.covid19.admin.ch"
)

excess_deaths <- deaths_weekly |>
  filter(date >= min(df$date), date <= max(df$date)) |>
  mutate(deaths_excess = ifelse(deaths_excess > 0, deaths_excess, 0))

# df2 <- df |>
#   pivot_wider(
#     names_from = vaccination_status,
#     values_from = deaths
#   ) |>
#   mutate(covid = `3_unvaccinated` + `2_vaccinated` + `1_unknown`) |>
#   inner_join(excess_deaths, by = c("date")) |>
#   mutate(non_covid_excess = deaths_excess - covid) |>
#   mutate(
#     non_covid_excess = ifelse(non_covid_excess > 0, non_covid_excess, 0)
#   ) |>
#   select(
#     date, `3_unvaccinated`, `2_vaccinated`, `1_unknown`, deaths_excess, non_covid_excess
#   ) |>
#   setNames(c(
#     "date",
#     "COVID-19 (Unvaccinated)",
#     "COVID-19 (Vaccinated)",
#     "COVID-19 (Unknown)",
#     "Excess",
#     "Non COVID-19 Excess"
#   )) |>
#   pivot_longer(cols = !date, names_to = "death_type", values_to = "deaths")

# # Bar Chart
# df3 <- df2 |>
#   filter(death_type != "Non COVID-19 Excess") |>
#   getDailyFromWeekly("deaths") |>
#   mutate(quarter = yearquarter(date)) |>
#   select(quarter, death_type, deaths) |>
#   group_by(quarter, death_type) |>
#   summarise(deaths = round(sum(deaths, na.rm = TRUE)))

# ggplot(
#   df3 |> filter(death_type != "Excess"),
#   aes(
#     x = quarter,
#     y = deaths,
#     fill = death_type
#   )
# ) +
#   labs(
#     title = "Quarterly COVID-19 Deaths by Vaccination Status [Switzerland]",
#     subtitle = "Source: www.covid19.admin.ch",
#     x = "Quarter of Year",
#     y = "COVID-19 Deaths"
#   ) +
#   geom_bar(stat = "identity") +
#   twitter_theme() +
#   scale_fill_manual(
#     name = "",
#     values = c(
#       "#A3A02C",
#       "#A33F1C",
#       "#404E64",
#       "#41F06A"
#     )
#   )

df4 <- df3 |> mutate(type = ifelse(death_type == "Excess", " ", ""))

ggplot(df4, aes(x = type, y = deaths, group = death_type, fill = death_type)) +
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
  facet_grid(. ~ quarter) +
  scale_fill_manual(
    values = c(
      "#A3A02C",
      "#A33F1C",
      "#404E64",
      "#000000"
    )
    # breaks = c("Unvaccinated", "Ever vaccinated", "Excess")
  )
