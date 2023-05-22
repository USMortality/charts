source("lib/common.r")

us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

data1 <- read.csv("../wonder_dl/out/yearly/us_states_1999_2020_all.csv")
data2 <- read.csv("../wonder_dl/out/yearly/us_states_2021_n_all.csv")

parse_data <- function(df, jurisdiction_column, age_group) {
  df |>
    select(!!jurisdiction_column, Year.Code, Deaths) |>
    setNames(c("jurisdiction", "date", "deaths")) |>
    left_join(us_states_iso3c, by = "jurisdiction") |>
    filter(!is.na(iso3c), !is.na(date)) |>
    select(iso3c, date, deaths) |>
    mutate(age_group = gsub("_", "-", age_group), .after = date)
}

totals <- rbind(
  parse_data(data1, "State", "all"),
  parse_data(data2, "Residence.State", "all")
) |>
  as_tibble() |>
  arrange(iso3c, date) |>
  distinct(iso3c, date, .keep_all = TRUE)

process_age_groups <- function(age_groups) {
  # 1999-2020
  ag1 <- data.frame()
  for (age_group in age_groups) {
    data <- read.csv(paste0(
      "../wonder_dl/out/yearly/us_states_1999_2020_",
      age_group,
      ".csv"
    ))
    ag1 <- rbind(ag1, parse_data(data, "State", age_group))
  }

  # 2020+
  ag2 <- data.frame()
  for (age_group in age_groups) {
    data <- read.csv(paste0(
      "../wonder_dl/out/yearly/us_states_2021_n_",
      age_group,
      ".csv"
    ))
    ag2 <- rbind(ag2, parse_data(data, "Residence.State", age_group))
  }

  totals_ag <- rbind(ag1, ag2) |>
    as_tibble() |>
    arrange(iso3c, date, age_group) |>
    distinct(iso3c, date, age_group, .keep_all = TRUE)

  rbind(totals, totals_ag) |>
    arrange(iso3c, date, age_group) |>
    distinct(iso3c, date, age_group, .keep_all = TRUE) |>
    complete(iso3c, date, age_group)
}

# By 10y age group
age_groups <- c(
  "0_9",
  "10_19",
  "20_29",
  "30_39",
  "40_49",
  "50_59",
  "60_69",
  "70_79",
  "80_89",
  "90_100",
  "NS"
)
result_10y <- process_age_groups(age_groups) |>
  mutate(age_group = ifelse(age_group == "90-100", "90+", age_group))
save_csv(result_10y, "deaths/usa/yearly_10y", upload = TRUE)

# Impute NA's
result_10y_imputed <- result_10y |>
  filter(date <= 2022) |>
  group_by(iso3c, date) |>
  group_modify(~ imputeSingleNA(.x)) |>
  ungroup()
save_csv(result_10y_imputed, "deaths/usa/yearly_10y_imputed", upload = TRUE)

# By 5y age group
age_groups <- c(
  "0_4",
  "5_9",
  "10_14",
  "15_19",
  "20_24",
  "25_29",
  "30_34",
  "35_39",
  "40_44",
  "45_49",
  "50_54",
  "55_59",
  "60_64",
  "65_69",
  "70_74",
  "75_79",
  "80_84",
  "85_89",
  "90_94",
  "95_100",
  "NS"
)
result_5y <- process_age_groups(age_groups) |>
  mutate(age_group = ifelse(age_group == "95-100", "95+", age_group))
save_csv(result_5y, "deaths/usa/yearly_5y", upload = TRUE)

# Use NS from 10y
result_5y_imputed <- rbind(
  result_10y_imputed |> filter(age_group == "NS"),
  result_5y |> filter(age_group != "NS")
) |> arrange(iso3c, date, age_group)

# Impute NA's
result_5y_imputed <- result_5y_imputed |>
  filter(date <= 2022) |>
  group_by(iso3c, date) |>
  group_modify(~ imputeSingleNA(.x)) |>
  group_modify(~ imputeFromAggregate(
    .x, result_10y_imputed, "0-9", c("0-4", "5-9")
  ), .keep = TRUE) |>
  group_modify(~ imputeFromAggregate(
    .x, result_10y_imputed, "10-19", c("10-14", "15-19")
  ), .keep = TRUE) |>
  ungroup()

save_csv(result_5y_imputed, "deaths/usa/yearly_5y_imputed", upload = TRUE)
