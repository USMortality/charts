source("lib/common.r")

# National
df <- read_excel(
  "./data/sonderauswertung-sterbefaelle.xlsx",
  sheet = "D_2016_2023_KW_AG_Ins",
  range = "B9:BE569"
)

df1 <- df %>%
  mutate(across(.cols = 3:ncol(df), .fns = as.numeric)) %>%
  pivot_longer(cols = 3:ncol(df), names_to = "week", values_to = "deaths") %>%
  setNames(c("year", "age_group", "week", "deaths")) %>%
  mutate(jurisdiction = "Deutschland", .after = "year")

# States
df <- read_excel(
  "./data/sonderauswertung-sterbefaelle.xlsx",
  sheet = "BL_2016_2023_KW_AG_Ins",
  range = "B9:BE569"
)

df2 <- df %>%
  mutate(across(.cols = 4:ncol(df), .fns = as.numeric)) %>%
  pivot_longer(cols = 4:ncol(df), names_to = "week", values_to = "deaths") %>%
  setNames(c("year", "jurisdiction", "age_group", "week", "deaths"))

df <- rbind(df1, df2) %>%
  mutate(
    year = as.integer(year),
    week = as.integer(week),
    deaths = as.integer(deaths)
  ) %>%
  filter(!is.na(deaths))

save_csv(df, "mortality/deu/deaths")
