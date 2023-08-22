source("lib/common.r")

data1 <- as_tibble(read.csv("./data/usa_states_excess_weekly.csv"))
data2 <- as_tibble(read.csv("./data/usa_states_age_weekly.csv"))
us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

df1 <- data1 |>
  filter(Type == "Predicted (weighted)", Outcome == "All causes") |>
  select("State", "Week.Ending.Date", "Year", "Observed.Number") |>
  setNames(c("jurisdiction", "date", "year", "deaths")) |>
  mutate(
    date = yearweek(date),
    age_group = "all",
    deaths = as.integer(str_replace(deaths, ",", ""))
  ) |>
  select(-"year")

# Combine NY/NYC
ny <- df1 |>
  filter(jurisdiction %in% c("New York", "New York City")) |>
  group_by(date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()
ny$jurisdiction <- "New York"
df1 <- rbind(df1 |> filter(jurisdiction != "New York"), ny)

df2 <- data2 |>
  filter(Type == "Predicted (weighted)") |>
  select(Jurisdiction, Year, Week, Age.Group, Number.of.Deaths) |>
  setNames(c("jurisdiction", "year", "week", "age_group", "deaths")) |>
  mutate(
    date = make_yearweek(year = year, week = week),
    # Create categories
    age_group = case_when(
      age_group == "Under 25 years" ~ "0-24",
      age_group == "25-44 years" ~ "25-44",
      age_group == "45-64 years" ~ "45-64",
      age_group == "65-74 years" ~ "65-74",
      age_group == "75-84 years" ~ "75-84",
      age_group == "85 years and older" ~ "85+"
    )
  ) |>
  select(-"year", -"week")

# Combine NY/NYC
ny <- df2 |>
  filter(jurisdiction %in% c("New York", "New York City")) |>
  group_by(date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()
ny$jurisdiction <- "New York"
df2 <- rbind(df2 |> filter(jurisdiction != "New York"), ny)

df3 <- df2 |> filter(jurisdiction == "United States", age_group == "85+")
df3$age_group <- "NS"
df3$deaths <- NA

df <- rbind(df1, df2, df3)

result <- df |>
  left_join(us_states_iso3c, by = "jurisdiction") |>
  select(-"jurisdiction") |>
  relocate(iso3c, date, age_group, deaths) |>
  arrange(iso3c, date, age_group) |>
  complete(iso3c, date, age_group) |>
  group_by(iso3c, date) |>
  group_modify(~ impute_single_na(.x)) |>
  ungroup()

save_csv(
  result |> mutate(year = isoyear(date), week = isoweek(date)),
  "deaths/usa/age_weekly_2015-n",
  upload = TRUE
)
