source("lib/common.r")

df <- read.csv("./data_static/COVID19Death_vaccpersons_AKL10_w.csv") |>
  select(date, altersklasse_covid19, vaccination_status, entries, pop) |>
  setNames(c(
    "date",
    "age_group",
    "vaccination_status",
    "deaths",
    "population"
  )) |>
  as_tibble() |>
  mutate(
    vaccination_status = case_when(
      vaccination_status %in% c(
        "fully_vaccinated",
        "fully_vaccinated_no_booster"
      ) ~ "fully_vaccinated",
      vaccination_status %in% c(
        "fully_vaccinated_first_booster"
      ) ~ "fully_vaccinated_with_booster",
      vaccination_status %in% c("not_vaccinated") ~ "unvaccinated"
    )
  ) |>
  group_by(date, age_group, vaccination_status) |>
  summarise(deaths = sum(deaths), population = sum(population)) |>
  ungroup() |>
  mutate(date = make_yearweek(
    year = as.integer(left(date, 4)),
    week = as.integer(right(date, 2))
  ))

df1 <- df |>
  filter(!age_group %in% c(
    "12 - 15",
    "16 - 64",
    "5 - 11",
    "65+",
    "Unbekannt"
  )) |>
  filter(!is.na(vaccination_status)) |>
  mutate(death_rate = deaths / population * 100000)

# Age adjusted rate
std_pop <- as_tibble(
  read.csv("https://s3.mortality.watch/data/population/who_std_pop_9.csv")
)

df2 <- df1 |>
  select(-deaths, -population) |>
  mutate(age_group = gsub(" ", "", age_group)) |>
  inner_join(std_pop, by = c("age_group")) |>
  mutate(asmr = death_rate * percentage) |>
  group_by(date, vaccination_status) |>
  summarise(asmr = sum(asmr, na.rm = TRUE)) |>
  ungroup()

ggplot(
  df2,
  aes(
    x = date,
    y = asmr,
    group = vaccination_status,
    color = vaccination_status
  )
) +
  labs(
    title = "Weekly COVID-19 Death Rate by Vaccination Status [Switzerland]",
    subtitle = "Age-Adjusted | Source: www.covid19.admin.ch",
    x = "Week of Year",
    y = "COVID-19 ASMR"
  ) +
  geom_line(linewidth = 1) +
  twitter_theme() +
  scale_color_manual(values = c(
    "#404E64",
    "#39827E",
    "#A33F1C"
  )) +
  theme(legend.position = "top")
