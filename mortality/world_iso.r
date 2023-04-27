source("lib/common.r")

population <- read_remote("population/world.csv")

country_codes <- population |>
  select(iso3c, jurisdiction) |>
  unique()

de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv")) |>
  mutate(jurisdiction = paste0("DEU - ", jurisdiction))
usa_states <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv")) |>
  mutate(jurisdiction = paste0("USA - ", jurisdiction))

iso3c_jurisdiction <- rbind(de_states, usa_states, country_codes) |>
  distinct(iso3c, .keep_all = TRUE) |>
  arrange(iso3c)
