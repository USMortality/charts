source("lib/common.r")

us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

# Download data
data0 <- as_tibble(read.csv("./data_static/population_usa_2000-2010.csv"))
data1 <- as_tibble(read.csv("./data_static/population_usa_2010-2020.csv"))
data2 <- as_tibble(read.csv("./data_static/population_usa_2020-2022.csv"))

# Transform data
a <- data0 |>
  filter(SEX == 0) |>
  select(
    NAME, AGE,
    POPESTIMATE2000, POPESTIMATE2001, POPESTIMATE2002,
    POPESTIMATE2003, POPESTIMATE2004, POPESTIMATE2005,
    POPESTIMATE2006, POPESTIMATE2007, POPESTIMATE2008,
    POPESTIMATE2009
  ) |>
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "year", values_to = "population"
  ) |>
  transform(year = right(year, 4)) |>
  setNames(c("jurisdiction", "age", "year", "population"))

b <- data1 |>
  filter(SEX == 0) |>
  select(
    NAME, AGE,
    POPEST2010_CIV, POPEST2011_CIV, POPEST2012_CIV, POPEST2013_CIV,
    POPEST2014_CIV, POPEST2015_CIV, POPEST2016_CIV, POPEST2017_CIV,
    POPEST2018_CIV, POPEST2019_CIV
  ) |>
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "year", values_to = "population"
  ) |>
  transform(year = str_sub(year, 7, 10)) |>
  setNames(c("jurisdiction", "age", "year", "population"))

c <- data2 |>
  filter(SEX == 0) |>
  select(
    NAME, AGE, POPEST2020_CIV, POPEST2021_CIV
  ) |>
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "year", values_to = "population"
  ) |>
  transform(year = str_sub(year, 7, 10)) |>
  setNames(c("jurisdiction", "age", "year", "population"))

# Create 5y age bands
population_grouped <- bind_rows(list(a, b, c)) |>
  mutate(
    # Create categories
    age_group = case_when(
      age == "999" ~ "all",
      age >= 0 & age <= 4 ~ "0-4",
      age >= 5 & age <= 9 ~ "5-9",
      age >= 10 & age <= 14 ~ "10-14",
      age >= 15 & age <= 19 ~ "15-19",
      age >= 20 & age <= 24 ~ "20-24",
      age >= 25 & age <= 29 ~ "25-29",
      age >= 30 & age <= 34 ~ "30-34",
      age >= 35 & age <= 39 ~ "35-39",
      age >= 40 & age <= 44 ~ "40-44",
      age >= 45 & age <= 49 ~ "45-49",
      age >= 50 & age <= 54 ~ "50-54",
      age >= 55 & age <= 59 ~ "55-59",
      age >= 60 & age <= 64 ~ "60-64",
      age >= 65 & age <= 69 ~ "65-69",
      age >= 70 & age <= 74 ~ "70-74",
      age >= 75 & age <= 79 ~ "75-79",
      age >= 80 & age <= 84 ~ "80-84",
      age >= 85 ~ "85+"
    )
  ) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population)) |>
  mutate(year = as.integer(year)) |>
  ungroup()

population_grouped <- population_grouped |>
  inner_join(us_states_iso3c, by = c("jurisdiction"))

population_grouped_forecasted <- population_grouped |>
  nest(data = c("year", "population")) |>
  mutate(data = lapply(data, forecast_population)) |>
  unnest(cols = "data") |>
  filter(!is.na(jurisdiction)) |>
  relocate(iso3c, jurisdiction, age_group)

save_csv(population_grouped_forecasted, "population/usa/5y")

# Create 10y age bands
population_grouped <- bind_rows(list(a, b, c)) |>
  mutate(
    # Create categories
    age_group = case_when(
      age == "999" ~ "all",
      age >= 0 & age <= 9 ~ "0-9",
      age >= 10 & age <= 19 ~ "10-19",
      age >= 20 & age <= 29 ~ "20-29",
      age >= 30 & age <= 39 ~ "30-39",
      age >= 40 & age <= 49 ~ "40-49",
      age >= 50 & age <= 59 ~ "50-59",
      age >= 60 & age <= 69 ~ "60-69",
      age >= 70 & age <= 79 ~ "70-79",
      age >= 80 ~ "80+"
    )
  ) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population)) |>
  mutate(year = as.integer(year)) |>
  ungroup()

population_grouped <- population_grouped |>
  inner_join(us_states_iso3c, by = c("jurisdiction"))

population_grouped_forecasted <- population_grouped |>
  nest(data = c("year", "population")) |>
  mutate(data = lapply(data, forecast_population)) |>
  unnest(cols = "data") |>
  filter(!is.na(jurisdiction)) |>
  relocate(iso3c, jurisdiction, age_group)

save_csv(population_grouped_forecasted, "population/usa/10y")

# Create 20y age bands
population_grouped <- bind_rows(list(a, b, c)) |>
  mutate(
    # Create categories
    age_group = case_when(
      age == "999" ~ "all",
      age >= 0 & age <= 19 ~ "0-19",
      age >= 20 & age <= 39 ~ "20-39",
      age >= 40 & age <= 59 ~ "40-59",
      age >= 60 & age <= 79 ~ "60-79",
      age >= 80 ~ "80+"
    )
  ) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population)) |>
  mutate(year = as.integer(year)) |>
  ungroup()

population_grouped <- population_grouped |>
  inner_join(us_states_iso3c, by = c("jurisdiction"))

population_grouped_forecasted <- population_grouped |>
  nest(data = c("year", "population")) |>
  mutate(data = lapply(data, forecast_population)) |>
  unnest(cols = "data") |>
  filter(!is.na(jurisdiction)) |>
  relocate(iso3c, jurisdiction, age_group)

save_csv(population_grouped_forecasted, "population/usa/20y")
