source("lib/common.r")

de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv"))
df <- read_remote("deaths/deu/deaths.csv") |>
  inner_join(de_states, by = "jurisdiction")

# All Ages
df_all <- df |>
  filter(age_group == "Insgesamt") |>
  mutate(
    date = make_yearweek(year = year, week = week),
    age_group = "all"
  ) |>
  getDailyFromWeekly(c("deaths")) |>
  select(iso3c, date, age_group, deaths) |>
  arrange(iso3c, date)

# By age group, national
df_age_d <- df |>
  filter(jurisdiction == "Deutschland" & age_group != "Insgesamt") |>
  mutate(
    date = make_yearweek(year = year, week = week),
    age_group = case_when(
      age_group %in% c("0-30") ~ "0-29",
      age_group %in% c("30-35") ~ "30-44",
      age_group %in% c("35-40") ~ "30-44",
      age_group %in% c("40-45") ~ "30-44",
      age_group %in% c("45-50") ~ "45-64",
      age_group %in% c("50-55") ~ "45-64",
      age_group %in% c("55-60") ~ "45-64",
      age_group %in% c("60-65") ~ "45-64",
      age_group %in% c("65-70") ~ "65-74",
      age_group %in% c("70-75") ~ "65-74",
      age_group %in% c("75-80") ~ "75-84",
      age_group %in% c("80-85") ~ "75-84",
      age_group %in% c("85-90") ~ "85+",
      age_group %in% c("90-95") ~ "85+",
      age_group %in% c("95 u. mehr") ~ "85+"
    )
  ) |>
  group_by(iso3c, date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup() |>
  getDailyFromWeekly(c("deaths"))

# By age group, states
df_age_states <- df |>
  filter(jurisdiction != "Deutschland" & age_group != "Insgesamt") |>
  mutate(
    date = make_yearweek(year = year, week = week),
    age_group = case_when(
      age_group %in% c("0-65") ~ "0-64",
      age_group %in% c("65-75") ~ "65-74",
      age_group %in% c("75-85") ~ "75-84",
      age_group %in% c("85 u. mehr") ~ "85+"
    )
  ) |>
  group_by(iso3c, date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup() |>
  getDailyFromWeekly(c("deaths"))

# Population
source("population/deu/deu.r")

population <- de_population |>
  mutate(date = date(sprintf("%d-01-01", year)), .after = iso3c) |>
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
  select(iso3c, date, age_group, population)

deu_mortality_states <- rbind(df_all, df_age_d, df_age_states) |>
  inner_join(population, by = c("iso3c", "date", "age_group")) |>
  unique() |>
  arrange(iso3c, date, age_group)
