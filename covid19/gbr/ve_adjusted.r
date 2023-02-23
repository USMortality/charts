source("lib/common.r")

source("covid19/gbr/deaths_by_vaxx.r")
deaths <- df |>
  filter(type == "All causes") |>
  select(-type)

# Vaxx Rates
data <- read.csv(
  "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=vaccinationsAgeDemographics&format=csv"
) |>
  as_tibble() |>
  select(4, 5, cumPeopleVaccinatedFirstDoseByVaccinationDate, cumVaccinationFirstDoseUptakeByVaccinationDatePercentage) |>
  set_names(c("date", "age_group", "vaxxed", "vaxxed_pct"))

vaxxed <- data |>
  mutate(
    date = ymd(date),
    age_group = str_replace(age_group, "_", "-")
  ) |>
  mutate(
    # Translate years
    age_group = case_when(
      age_group %in% c("05-11", "12-15", "16-17") ~ "0-17",
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
  summarise(
    vaxxed_lower = as.integer(min(vaxxed)),
    vaxxed_upper = as.integer(max(vaxxed)),
    vaxxed = as.integer(mean(vaxxed))
  ) |>
  inner_join(population, by = "age_group")

save_csv(ve, "test", upload = FALSE)

# Deaths
mr <- deaths |>
  inner_join(vaxxed, by = c("date", "age_group")) |>
  mutate(
    population = ifelse(
      vaxx_status == "Unvaccinated",
      population - vaxxed,
      vaxxed
    ),
    # population_lower = ifelse(
    #   vaxx_status == "Unvaccinated",
    #   population - vaxxed_lower,
    #   vaxxed_lower
    # ),
    # population_upper = ifelse(
    #   vaxx_status == "Unvaccinated",
    #   population - vaxxed_upper,
    #   vaxxed_upper
    # )
  ) |>
  select(-vaxxed, -vaxxed_lower, -vaxxed_upper) |>
  pivot_wider(
    names_from = vaxx_status,
    values_from = c(deaths, population,
    #  population_lower, population_upper
     )
  ) |>
  set_names(c(
    "date",
    "age_group",
    "deaths_unvaxx",
    "deaths_vaxx",
    "population_unvaxx",
    "population_vaxx"
    # "population_unvaxx_lower",
    # "population_vaxx_lower",
    # "population_unvaxx_upper",
    # "population_vaxx_upper"
  )) |>
  mutate(
    mr_vaxx = deaths_vaxx / population_vaxx * 100000,
    mr_unvaxx = deaths_unvaxx / population_unvaxx * 100000,
  ) |>
  select(date, age_group, mr_vaxx, mr_unvaxx)

mr

#  |>
# mutate(
#   ve = 1 - (deaths_vaxx / population_vaxx) /
#     (deaths_unvaxx / population_unvaxx),
#   ve_lower = 1 - (deaths_vaxx / population_vaxx_lower) /
#     (deaths_unvaxx / population_unvaxx_lower),
#   ve_upper = 1 - (deaths_vaxx / population_vaxx_upper) /
#     (deaths_unvaxx / population_unvaxx_upper)
# ) |>
# select(date, age_group, ve, ve_lower, ve_upper)

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
  geom_ribbon(aes(
    ymin = ve_lower,
    ymax = ve_upper,
    fill = age_group
  ), alpha = .3, linetype = 0) +
  twitter_theme() +
  geom_hline(yintercept = 0) +
  watermark(df$yearmonth, df$value_p) +
  scale_x_yearmonth(date_breaks = "1 months", date_labels = "%Y/%m") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-1, 1)
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
    legend.position = "top"
  )

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
  geom_ribbon(aes(
    ymin = ve_lower,
    ymax = ve_upper,
    fill = age_group
  ), alpha = .3, linetype = 0) +
  twitter_theme() +
  geom_hline(yintercept = 0) +
  watermark(df$yearmonth, df$value_p) +
  scale_x_yearmonth(date_breaks = "2 months", date_labels = "%Y/%m") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-1, 1)
  ) +
  facet_wrap(vars(age_group), scales = "free") +
  theme(
    panel.spacing = unit(0.3, "in"),
    legend.position = "top"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
    legend.position = "top"
  )
