source("lib/common.r")

# Load Data
deaths <- as_tibble(read.csv("./data/mortality_org.csv", skip = 2))

deaths$CountryCode[deaths$CountryCode == "DEUTNP"] <- "DEU"
deaths$CountryCode[deaths$CountryCode == "FRATNP"] <- "FRA"
deaths$CountryCode[deaths$CountryCode == "NZL_NP"] <- "NZL"
deaths$CountryCode[deaths$CountryCode == "GBR_NP"] <- "GBR"

df <- deaths |>
  filter(Sex == "b") |>
  mutate(
    P0_14 = D0_14 / R0_14 * 52,
    P15_64 = D15_64 / R15_64 * 52,
    P65_74 = D65_74 / R65_74 * 52,
    P75_84 = D75_84 / R75_84 * 52,
    P85p = D85p / R85p * 52,
    PTotal = DTotal / RTotal * 52,
  ) |>
  mutate(date = make_yearweek(
    year = Year, week = Week
  ))

population <- df |>
  select(CountryCode, date, P0_14, P15_64, P65_74, P75_84, P85p, PTotal) |>
  setNames(
    c("iso3c", "year", "0-14", "15-64", "65-74", "75-84", "85+", "all")
  ) |>
  pivot_longer(
    cols = 3:8,
    names_to = "age_group",
    values_to = "population"
  ) |>
  mutate(year = year(year)) |>
  group_by(iso3c, year, age_group) |>
  summarize(population = mean(population, na.rm = TRUE)) |>
  ungroup() |>
  nest(data = c("year", "population")) |>
  mutate(data = lapply(data, forecast_population)) |>
  unnest(cols = "data") |>
  mutate(date = date(sprintf("%d-01-01", year)), .after = year) |>
  group_by(iso3c) |>
  nest() |>
  mutate(
    data = map(data, ~ . |>
      group_by(age_group) |>
      nest() |>
      mutate(data = lapply(data, interpolate_population)) |>
      unnest(cols = "data"))
  ) |>
  unnest(cols = "data") |>
  select(-year, -is_projection)

mortality_org <- df |>
  select(CountryCode, date, D0_14, D15_64, D65_74, D75_84, D85p, DTotal) |>
  setNames(
    c("iso3c", "date", "0-14", "15-64", "65-74", "75-84", "85+", "all")
  ) |>
  pivot_longer(
    cols = 3:8,
    names_to = "age_group",
    values_to = "deaths"
  ) |>
  getDailyFromWeekly(c("deaths")) |>
  inner_join(population, by = c("iso3c", "date", "age_group")) |>
  arrange(iso3c, date, age_group) |>
  distinct(iso3c, date, age_group, .keep_all = TRUE)
