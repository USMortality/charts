de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv"))

de_population <- as_tibble(read.csv("data_static/population_deu_2000-n.csv")) %>%
  filter(jurisdiction != "Deutschland", age_group == "all") %>%
  filter(age_group == "all") %>%
  select(-age_group)

# Format: iso3c, jurisdiction, year, population, is_projection
de_population <- de_population %>%
  inner_join(de_states, by = "jurisdiction") %>%
  mutate(jurisdiction = paste0("DEU - ", jurisdiction)) %>%
  select(iso3c, jurisdiction, year, population)

de_population$is_projection <- FALSE
