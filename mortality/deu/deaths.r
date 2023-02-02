source("lib/common.r")

# National
df1 <- read_excel(
  "./data_static/sonderauswertung-sterbefaelle-endgueltige-daten.xlsx",
  sheet = "D_2000_2015_KW_AG_Ins",
  range = "B9:BE9999"
)
df2 <- read_excel(
  "./data/sonderauswertung-sterbefaelle.xlsx",
  sheet = "D_2016_2023_KW_AG_Ins",
  range = "B9:BE9999"
)

result1 <- rbind(df1, df2) %>%
  mutate(across(.cols = 3:ncol(df1), .fns = as.numeric)) %>%
  pivot_longer(cols = 3:ncol(df1), names_to = "week", values_to = "deaths") %>%
  setNames(c("year", "age_group", "week", "deaths")) %>%
  mutate(jurisdiction = "Deutschland", .after = "year")

# States
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
result2 <- rbind(df1, df2) %>%
  mutate(across(.cols = 4:ncol(df1), .fns = as.numeric)) %>%
  pivot_longer(cols = 4:ncol(df1), names_to = "week", values_to = "deaths") %>%
  setNames(c("year", "jurisdiction", "age_group", "week", "deaths"))

df <- rbind(result1, result2) %>%
  mutate(
    year = as.integer(year),
    week = as.integer(week),
    deaths = as.integer(deaths)
  ) %>%
  filter(!is.na(deaths)) %>%
  arrange(year, jurisdiction, age_group, week)

save_csv(df, "mortality/deu/deaths")
