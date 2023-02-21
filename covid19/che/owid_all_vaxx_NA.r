source("lib/common.r")

df <- read.csv("./data_static/COVID19Death_vaccpersons_AKL10_w.csv") %>%
  select(date, altersklasse_covid19, vaccination_status, entries, pop) %>%
  setNames(c(
    "date",
    "age_group",
    "vaccination_status",
    "deaths",
    "population"
  )) %>%
  as_tibble()

df2a <- df %>%
  mutate(
    vaccination_status = case_when(
      vaccination_status == "not_vaccinated" ~ "unvaccinated_lower",
      .default = "vaccinated_upper"
    )
  ) %>%
  group_by(date, age_group, vaccination_status) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), population = sum(population, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = make_yearweek(
    year = as.integer(left(date, 4)),
    week = as.integer(right(date, 2))
  )) %>%
  filter(!age_group %in% c(
    "12 - 15",
    "16 - 64",
    "5 - 11",
    "65+",
    "Unbekannt"
  )) %>%
  filter(!is.na(vaccination_status)) %>%
  mutate(death_rate = deaths / population * 100000)

df2b <- df %>%
  mutate(
    vaccination_status = case_when(
      vaccination_status %in% c(
        "fully_vaccinated",
        "fully_vaccinated_no_booster",
        "fully_vaccinated_first_booster",
        "partially_vaccinated"
      ) ~ "vaccinated_lower",
      vaccination_status %in% c("not_vaccinated", "unknown") ~ "unvaccinated_upper"
    )
  ) %>%
  group_by(date, age_group, vaccination_status) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), population = sum(population, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = make_yearweek(
    year = as.integer(left(date, 4)),
    week = as.integer(right(date, 2))
  )) %>%
  filter(!age_group %in% c(
    "12 - 15",
    "16 - 64",
    "5 - 11",
    "65+",
    "Unbekannt"
  )) %>%
  filter(!is.na(vaccination_status)) %>%
  mutate(death_rate = deaths / population * 100000)

# Age adjusted rate
std_pop <- as_tibble(
  read.csv("https://s3.mortality.watch/data/population/who_std_pop_9.csv")
)

df2 <- rbind(df2a, df2b) %>%
  select(-deaths, -population) %>%
  mutate(age_group = gsub(" ", "", age_group)) %>%
  inner_join(std_pop, by = c("age_group")) %>%
  mutate(asmr = death_rate * percentage) %>%
  group_by(date, vaccination_status) %>%
  summarise(asmr = sum(asmr, na.rm = TRUE)) %>%
  ungroup()

df3 <- df2 |>
  separate("vaccination_status", c("vaccination_status", "range")) |>
  pivot_wider(names_from = range, values_from = asmr)

ggplot(
  df3,
  aes(
    x = date,
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
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ),
    fill = "#33333333"
  ) +
  twitter_theme() +
  theme(legend.position = "top")
