source("lib/common.r")

de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv"))

df1 <- read_excel(
  "./data_static/sonderauswertung-sterbefaelle-endgueltige-daten.xlsx",
  sheet = "BL_2000_2015_KW_AG_Ins",
  range = "B9:BE9999"
)

df2 <- read_excel(
  "./data/sonderauswertung-sterbefaelle.xlsx",
  sheet = "BL_2016_2023_KW_AG_Ins",
  range = "B9:BE9999"
)

df1 <- df1 %>%
  mutate(across(.cols = 4:ncol(df1), .fns = as.numeric)) %>%
  pivot_longer(cols = 4:ncol(df1), names_to = "week", values_to = "deaths") %>%
  setNames(c("year", "jurisdiction", "age_group", "week", "deaths"))

df2 <- df2 %>%
  mutate(across(.cols = 4:ncol(df2), .fns = as.numeric)) %>%
  pivot_longer(cols = 4:ncol(df2), names_to = "week", values_to = "deaths") %>%
  setNames(c("year", "jurisdiction", "age_group", "week", "deaths"))

df <- rbind(df1, df2) %>%
  mutate(
    year = as.integer(year),
    week = as.integer(week),
    deaths = as.integer(deaths)
  ) %>%
  inner_join(de_states, by = "jurisdiction") %>%
  filter(!is.na(deaths))

# Format: iso3c, date, deaths
dd_de <- df %>%
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
