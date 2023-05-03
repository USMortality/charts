de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv"))

de_population <- as_tibble(read.csv(
  "data_static/population_deu_2000-n.csv"
))

# Format: iso3c, jurisdiction, year, population, is_projection
de_population <- de_population |>
  inner_join(de_states, by = "jurisdiction") |>
  select(iso3c, jurisdiction, year, age_group, population) |>
  group_by(iso3c, jurisdiction, year, age_group) |>
  summarise(population = sum(population)) |>
  ungroup() |>
  nest(data = c("year", "population")) |>
  mutate(data = lapply(data, forecast_population)) |>
  unnest(cols = "data")
