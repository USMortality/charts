source("lib/common.r")

source("covid19/gbr/deaths_by_vaxx.r")
deaths <- df |>
  filter(type == "All causes") |>
  select(-type)

# Vaxx Rates
data <- read.csv(
  "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv"
)

vaxxed <- data |>
  as_tibble() |>
  select(4, 5, cumVaccinationFirstDoseUptakeByVaccinationDatePercentage) |>
  set_names(c("date", "age_group", "vaxxed")) |>
  mutate(
    date = ymd(date),
    age_group = str_replace(age_group, "_", "-"),
    vaxxed = vaxxed / 100
  ) |>
  mutate(
    # Translate years
    age_group = case_when(
      age_group %in% c("18-24", "25-29", "30-34", "35-39") ~ "18-39",
      age_group %in% c("40-44", "45-49") ~ "40-49",
      age_group %in% c("50-54", "55-59") ~ "50-59",
      age_group %in% c("60-64", "65-69") ~ "60-69",
      age_group %in% c("70-74", "75-79") ~ "70-79",
      age_group %in% c("80-84", "85-89") ~ "80-89",
      age_group %in% c("90+") ~ "90+"
    )
  ) |>
  filter(!is.na(age_group)) |>
  mutate(date = yearmonth(date)) |>
  group_by(date, age_group) |>
  summarise(vaxxed = mean(vaxxed))

# Deaths
ve <- deaths |>
  inner_join(vaxxed, by = c("date", "age_group")) |>
  mutate(population = ifelse(
    vaxx_status == "Unvaccinated",
    1 - vaxxed,
    vaxxed
  )) |>
  select(-vaxxed) |>
  pivot_wider(
    names_from = vaxx_status,
    values_from = c(deaths, population)
  ) |>
  set_names(c(
    "date",
    "age_group",
    "deaths_unvaxx",
    "deaths_vaxx",
    "population_unvaxx",
    "population_vaxx"
  )) |>
  mutate(
    ve = 1 - (deaths_vaxx / population_vaxx) /
      (deaths_unvaxx / population_unvaxx)
  ) |>
  select(1, 2, 7)

ggplot(
  ve,
  aes(x = date, y = ve, group = age_group, color = age_group)
) +
  labs(
    title = "COVID-19 Vaccine Efficacy (Any Death) [England]",
    subtitle = "Source: ons.gov.uk",
    x = "Month of Year",
    y = "Vaccine Efficacy"
  ) +
  geom_line(linewidth = 1) +
  twitter_theme() +
  geom_hline(yintercept = 0) +
  watermark(df$yearmonth, df$value_p) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-1, 1)
  ) +
  theme(legend.position = "top")


options(vsc.dev.args = list(width = 1920, height = 1080, res = 72 * sf))
ggplot(
  ve,
  aes(x = date, y = ve, group = age_group, color = age_group)
) +
  labs(
    title = "COVID-19 Vaccine Efficacy (Any Death) [England]",
    subtitle = "Source: ons.gov.uk",
    x = "Month of Year",
    y = "Vaccine Efficacy"
  ) +
  geom_line(linewidth = 1) +
  twitter_theme() +
  geom_hline(yintercept = 0) +
  watermark(df$yearmonth, df$value_p) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-1, 1)
  ) +
  facet_wrap(vars(age_group), scales = "free") +
  theme(
    panel.spacing = unit(0.3, "in"),
    legend.position = "top"
  )
