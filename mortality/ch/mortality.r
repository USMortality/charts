source("lib/common.r")

df1 <- read.csv("data_static/che_deaths_n_2021.csv", sep = ";") |>
  as_tibble() |>
  setNames(c("TIME_PERIOD", "GEO", "AGE", "SEX", "OBS_STATUS", "OBS_VALUE"))

df2 <- read.csv("data/che_deaths_2022_n.csv", sep = ";") |>
  as_tibble()

unique(df$age_group)

df <- rbind(df1, df2) |>
  filter(GEO == "CH", SEX == "T") |>
  select(TIME_PERIOD, AGE, OBS_VALUE) |>
  setNames(c("date", "age_group", "deaths")) |>
  mutate(age_group = case_when(
    age_group %in% c("Y0T4", "Y5T9") ~ "0-10",
    age_group %in% c("Y10T14", "Y15T19") ~ "10-19",
    age_group %in% c("Y20T24", "Y25T29") ~ "20-29",
    age_group %in% c("Y30T34", "Y35T39") ~ "30-39",
    age_group %in% c("Y40T44", "Y45T49") ~ "40-49",
    age_group %in% c("Y50T54", "Y55T59") ~ "50-59",
    age_group %in% c("Y60T64", "Y65T69") ~ "60-69",
    age_group %in% c("Y70T74", "Y75T79") ~ "70-79",
    age_group %in% c("Y80T84", "Y85T89", "Y_GE90") ~ "80+"
  )) |>
  mutate(date = make_yearweek(
    year = as.integer(left(date, 4)),
    week = as.integer(right(date, 2))
  )) |>
  group_by(date, age_group) |>
  summarise(deaths = sum(deaths, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(age_group))

calc_excess <- function(data) {
  data <- data |>
    as_tsibble(index = date) |>
    group_by_key() |>
    index_by(year = ~ year(.)) |> # monthly aggregates
    summarise(deaths = sum(deaths, na.rm = TRUE)) |>
    filter(year > 2010)

  train_data <- data |> filter_index(2012 ~ 2019)
  model <- train_data |> model(TSLM(deaths ~ trend()))

  model |>
    forecast(h = 3) |>
    autoplot(data |> filter(year <= 2022), level = 95)
}

df_by_age <- df |> nest(data = !age_group)
data <- df_by_age$data[[9]]

# data |> mutate(data = lapply(calc_excess)
