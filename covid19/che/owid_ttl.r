
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
  ))
