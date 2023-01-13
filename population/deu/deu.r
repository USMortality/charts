de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv"))

de_population <- as_tibble(read.csv(
  "data_static/population_deu_2000-n.csv"
)) %>%
  filter(jurisdiction != "Deutschland", age_group == "all") %>%
  select(-age_group)

# Format: iso3c, jurisdiction, year, population, is_projection
de_population <- de_population %>%
  inner_join(de_states, by = "jurisdiction") %>%
  mutate(jurisdiction = paste0("DEU - ", jurisdiction)) %>%
  select(iso3c, jurisdiction, year, population)

de_population$is_projection <- FALSE

de_population_age <- as_tibble(read.csv(
  "data_static/population_deu_2000-n.csv"
)) %>% filter(jurisdiction != "Deutschland", age_group != "all")

# Format: iso3c, jurisdiction, year, population, is_projection
de_population_age <- de_population_age %>%
  inner_join(de_states, by = "jurisdiction") %>%
  mutate(jurisdiction = paste0("DEU - ", jurisdiction)) %>%
  select(iso3c, jurisdiction, year, age_group, population) %>%
  group_by(iso3c, jurisdiction, year, age_group) %>%
  summarise(population = sum(population))

de_population_age$is_projection <- FALSE
