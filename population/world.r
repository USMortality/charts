source("lib/common.r")
source("population/deu/deu.r")

world_population_raw <- read_excel(
  "./data_static/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
  sheet = "Estimates",
  range = cell_cols(6:12)
)

us_population <- read_remote("population/usa/six_age_bands.csv") |>
  filter(jurisdiction != "United States") |>
  mutate(jurisdiction = paste0("USA - ", jurisdiction)) |>
  filter(age_group == "all") |>
  select(-any_of("age_group"))

countries <- as_tibble(read.csv("./data_static/countries.csv")) |>
  select(iso3, name) |>
  setNames(c("iso3c", "name"))

# Population
world_population <- world_population_raw |>
  select(1, 6, 7) |>
  setNames(c("iso3c", "year", "population")) |>
  inner_join(countries, by = c("iso3c")) |>
  filter(!iso3c %in% c("ISO3 Alpha-code", NA)) |>
  mutate(year = as.integer(year), population = as.integer(population)) |>
  filter(!is.na(population)) |>
  mutate(population = as.integer(population * 1000))

population <- world_population |>
  nest(data = c("year", "population")) |>
  mutate(data = lapply(data, forecast_population)) |>
  unnest(cols = "data") |>
  setNames(c("iso3c", "jurisdiction", "year", "population", "is_projection"))

population <- rbind(
  population,
  us_population,
  de_population |> filter(age_group == "all") |>
    select(-any_of("age_group"))
) |> distinct(iso3c, year, .keep_all = TRUE)

save_csv(population, "population/world")
