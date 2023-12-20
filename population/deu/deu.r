source("lib/common.r")

de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv"))

# Genesis 12411-0005: Bevölkerung: Deutschland, Stichtag, Altersjahre
pop_raw <- as_tibble(
  head(
    read.csv("./data_static/12411-0005_$F.csv",
      sep = ";",
      skip = 6,
      colClasses = c("character"),
      fileEncoding = "latin1"
    ),
    -4
  )
)

# Parse raw data
pop <- pop_raw |>
  pivot_longer(
    cols = 2:ncol(pop_raw),
    names_to = "year",
    values_to = "population"
  ) |>
  setNames(c("age_group", "year", "population")) |>
  rowwise() |>
  mutate(
    age_group = case_when(
      age_group == "unter 1 Jahr" ~ "0",
      age_group == "85 Jahre und mehr" ~ "85+",
      age_group == "Alter unbekannt" ~ "NS",
      age_group == "Insgesamt" ~ "all",
      .default = str_split(age_group, "-")[[1]][1]
    ),
    year = as.integer(right(year, 4)),
    population = as.integer(population)
  )

pop$jurisdiction <- "Deutschland"
pop <- pop |> relocate(jurisdiction, year, age_group, population)

# Genesis 12411-0005: Bevölkerung: Deutschland, Stichtag, Altersjahre
pop_raw <- as_tibble(
  head(
    read.csv("./data_static/12411-0012.csv",
      sep = ";",
      skip = 5,
      colClasses = c("character"),
      fileEncoding = "latin1"
    ),
    -4
  )
)

# Parse raw data
pop_states <- pop_raw |>
  pivot_longer(
    cols = 3:ncol(pop_raw),
    names_to = "jurisdiction",
    values_to = "deaths"
  ) |>
  setNames(c("year", "age_group", "jurisdiction", "population")) |>
  rowwise() |>
  mutate(
    jurisdiction = str_replace_all(jurisdiction, "\\.", "-"),
    age_group = case_when(
      age_group == "unter 1 Jahr" ~ "0",
      age_group == "90 Jahre und mehr" ~ "90+",
      age_group == "Alter unbekannt" ~ "NS",
      age_group == "Insgesamt" ~ "all",
      .default = str_split(age_group, "-")[[1]][1]
    ),
    year = as.integer(right(year, 4)),
    population = suppress_warnings(
      as.integer(population),
      "NAs introduced by coercion"
    )
  ) |>
  relocate(jurisdiction, year, age_group, population)

rm(pop_raw)

pop2 <- pop |>
  mutate(
    age_group = case_when(
      age_group %in% 0:29 ~ "0-29",
      age_group %in% 30:34 ~ "30-34",
      age_group %in% 35:39 ~ "35-39",
      age_group %in% 40:44 ~ "40-44",
      age_group %in% 45:49 ~ "45-49",
      age_group %in% 50:54 ~ "50-54",
      age_group %in% 55:59 ~ "55-59",
      age_group %in% 60:64 ~ "60-64",
      age_group %in% 65:69 ~ "65-69",
      age_group %in% 70:74 ~ "70-74",
      age_group %in% 75:79 ~ "75-79",
      age_group %in% 80:84 ~ "80-84",
      age_group == "85+" ~ "85+",
      age_group == "all" ~ "all"
    )
  ) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population)) |>
  ungroup()

# Additional larger groups
pop3 <- pop |>
  mutate(
    age_group = case_when(
      age_group %in% 30:44 ~ "30-44",
      age_group %in% 45:64 ~ "45-64",
      age_group %in% 65:74 ~ "65-74",
      age_group %in% 75:84 ~ "75-84"
    )
  ) |>
  filter(!is.na(age_group)) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population)) |>
  ungroup()

rm(pop)

pop_states2 <- pop_states |>
  mutate(
    age_group = case_when(
      age_group %in% 0:64 ~ "0-64",
      age_group %in% 65:74 ~ "65-74",
      age_group %in% 75:84 ~ "75-84",
      age_group %in% 85:89 ~ "85+",
      age_group == "90+" ~ "85+",
      age_group == "all" ~ "all",
      .default = age_group
    )
  ) |>
  group_by(jurisdiction, year, age_group) |>
  summarise(population = sum(population)) |>
  ungroup()

rm(pop_states)

# Format: iso3c, jurisdiction, year, population, is_projection
de_population <- rbind(pop2, pop3, pop_states2) |>
  filter(!is.na(population)) |>
  inner_join(de_states, by = "jurisdiction") |>
  select(iso3c, jurisdiction, year, age_group, population) |>
  group_by(iso3c, jurisdiction, year, age_group) |>
  summarise(population = sum(population)) |>
  ungroup() |>
  nest(data = c("year", "population")) |>
  mutate(data = lapply(data, forecast_population)) |>
  unnest(cols = "data")

stopifnot(length(unique(de_population$iso3c)) == 17)

rm(de_states, pop2, pop_states2)

# source("./population/deu/deu.r")
