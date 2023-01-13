source("lib/common.r")

de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv"))

df1 <- read_excel(
  "./data_static/sonderauswertung-sterbefaelle-endgueltige-daten.xlsx",
  sheet = "BL_2000_2015_KW_AG_Ins",
  range = "B9:BE1289"
)

df2 <- read_excel(
  "./data/sonderauswertung-sterbefaelle.xlsx",
  sheet = "BL_2016_2022_KW_AG_Ins",
  range = "B9:BE569"
)

df1 <- df1 %>%
  mutate(`53` = as.numeric(`53`)) %>%
  pivot_longer(cols = 4:ncol(df1), names_to = "week", values_to = "deaths") %>%
  setNames(c("year", "jurisdiction", "age_group", "week", "deaths"))

df2 <- df2 %>%
  mutate(`50` = as.numeric(`50`)) %>%
  mutate(`51` = as.numeric(`51`)) %>%
  mutate(`52` = as.numeric(`52`)) %>%
  mutate(`53` = as.numeric(`53`)) %>%
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
  select(iso3c, date, deaths)

dd_de %>%
  filter(iso3c == "DE-BW")

save_csv(dd_de, "test", upload = TRUE)
