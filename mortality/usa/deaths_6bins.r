source("lib/common.r")

data <- read_remote("deaths/usa/yearly_5y_complete.csv")

df <- data |>
  mutate(
    age_group = case_when(
      age_group %in% c("0-4", "5-9", "10-14", "15-19", "20-24") ~ "0-24",
      age_group %in% c("25-29", "30-34", "35-39", "40-44") ~ "25-44",
      age_group %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
      age_group %in% c("65-69", "70-74") ~ "65-74",
      age_group %in% c("75-79", "70-74", "75-79", "80-84") ~ "75-84",
      age_group %in% c("85-89", "90-94", "95+") ~ "85+",
      age_group == "NS" ~ "NS",
      age_group == "all" ~ "all"
    )
  ) |>
  group_by(iso3c, date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()

save_csv(df, "deaths/usa/yearly_6bins_complete")
