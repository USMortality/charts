source("lib/common.r")

data <- read.csv("./data_static/COVID19Death_vaccpersons_AKL10_w.csv") %>%
  select(date, altersklasse_covid19, vaccination_status, entries, pop) %>%
  setNames(c(
    "date",
    "age_group",
    "vaccination_status",
    "deaths",
    "population"
  )) %>%
  as_tibble() %>%
  mutate(
    # Translate years
    vaccination_status = case_when(
      vaccination_status %in% c(
        "fully_vaccinated",
        "partially_vaccinated",
        "fully_vaccinated_first_booster",
        "fully_vaccinated_no_booster"
      ) ~ "vaccinated",
      vaccination_status %in% c("not_vaccinated") ~ "unvaccinated",
      vaccination_status %in% c("unknown") ~ "unknown"
    )
  ) %>%
  group_by(date, age_group, vaccination_status) %>%
  summarise(deaths = sum(deaths), population = sum(population)) %>%
  ungroup() %>%
  mutate(date = make_yearweek(
    year = as.integer(left(date, 4)),
    week = as.integer(right(date, 2))
  )) %>%
  nest(data = !age_group)

df <- data$data[[14]]

# Split Unknown deaths by vaccination ratio, and create interval
a <- df %>%
  select(-population) %>%
  pivot_wider(
    names_from = vaccination_status,
    values_from = c(
      deaths,
      #  population
    )
  ) %>%
  # mutate(ratio_vaccinated = population_vaccinated / (population_vaccinated + population_unvaccinated)) %>%
  mutate(
    vaccinated_lower = vaccinated,
    # vaccinated_mid = vaccinated + round(unknown * ratio_vaccinated),
    vaccinated_upper = vaccinated + unknown,
    unvaccinated_lower = unvaccinated,
    # unvaccinated_mid = unvaccinated + round(unknown * (1 - ratio_vaccinated)),
    unvaccinated_upper = unvaccinated + unknown
  ) %>%
  select(
    -unvaccinated, -vaccinated, -unknown
    # -deaths_vaccinated, -deaths_unvaccinated, -ratio_vaccinated
  ) %>%
  pivot_longer(
    cols = c(starts_with("vaccinated"), starts_with("unvaccinated")),
    names_to = c("vaccination_status", "range"),
    names_pattern = "(.*)_(.*)",
    values_to = "deaths",
  ) %>%
  pivot_wider(
    names_from = range,
    values_from = deaths,
    names_prefix = "deaths_"
  )

ggplot(
  a,
  aes(x = date, y= deaths_lower, group = vaccination_status, color = vaccination_status)
) +
  labs(
    title = "Weekly COVID-19 Deaths by Vaccination Status [Switzerland]",
    subtitle = "Source: www.covid19.admin.ch",
    x = "Week of Year",
    y = "COVID-19 Deaths"
  ) +
  geom_line(linewidth = 1) +
  # geom_ribbon(
  #   aes(
  #     ymin = deaths_lower
  #     ymax = deaths_upper
  #   ), # shadowing cnf intervals
  #   fill = "#33333333"
  # ) +
  twitter_theme() +
  watermark(df$yearmonth, df$value_p) +
  scale_x_yearweek(date_breaks = "1 year", date_labels = "%Y")

# mutate(
#   rr_vaxx = deaths_vaccinated / population_vaccinated,
#   rr_unvaxx = deaths_unvaccinated / population_unvaccinated,
#   ve = (1 - (rr_vaxx / rr_unvaxx))
# ) %>%
#   select(date, ve) %>%
#   autoplot(.var = ve)
