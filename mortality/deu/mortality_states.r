source("lib/common.r")

df <- read_remote("mortality/deu/deaths.csv")
de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv"))

# Format: iso3c, date, deaths
dd_de <- df %>%
  inner_join(de_states, by = "jurisdiction") %>%
  filter(age_group == "Insgesamt") %>%
  mutate(
    date = make_yearweek(year = year, week = week),
    jurisdiction = paste0("DEU - ", jurisdiction)
  ) %>%
  getDailyFromWeekly("deaths") %>%
  select(iso3c, date, deaths) %>%
  arrange(iso3c, date)

# ASMR
std_pop <- read_remote("population/who_std_pop_3.csv")
source("population/deu/deu.r")

dd_asmr_de_states <- df %>%
  filter(age_group != "Insgesamt") %>%
  mutate(age_group = case_when(
    age_group %in% c("0-65") ~ "0-64",
    age_group %in% c("65-75") ~ "65-74",
    age_group %in% c("75-85") ~ "75-84",
    age_group %in% c("85 u. mehr") ~ "85+"
  )) %>%
  inner_join(de_population_age, by = c("iso3c", "year", "age_group")) %>%
  mutate(mortality = deaths / population * 100000) %>%
  inner_join(std_pop, by = "age_group") %>%
  mutate(asmr = mortality * percentage) %>%
  select(iso3c, year, week, asmr) %>%
  setNames(c("iso3c", "year", "week", "asmr")) %>%
  group_by(iso3c, year, week) %>%
  summarise(asmr = sum(asmr)) %>%
  ungroup() %>%
  mutate(date = make_yearweek(year = year, week = week)) %>%
  getDailyFromWeekly("asmr") %>%
  select(iso3c, date, asmr)
