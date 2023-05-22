source("lib/common.r")

us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

data1 <- read.csv("../wonder_dl/out/yearly/us_states_1999_2020_all.csv")
data2 <- read.csv("../wonder_dl/out/yearly/us_states_2021_n_all.csv")

imputeSingleNA <- function(df) {
  # Count NAs
  n <- sum(is.na(df$deaths))

  # No need for imputation or too many.
  if (n != 1) {
    return(df)
  }

  # Find sum target
  target <- df$deaths[df$age_group == "all"] -
    sum(df$deaths[df$age_group != "all"], na.rm = TRUE)

  df$deaths[is.na(df$deaths)] <- target
  return(df)
}

parseData <- function(df, jurisdiction_column, age_group) {
  df |>
    select(!!jurisdiction_column, Year.Code, Deaths) |>
    setNames(c("jurisdiction", "date", "deaths")) |>
    left_join(us_states_iso3c, by = "jurisdiction") |>
    filter(!is.na(iso3c), !is.na(date)) |>
    select(iso3c, date, deaths) |>
    mutate(age_group = gsub("_", "-", age_group), .after = date)
}

totals <- rbind(
  parseData(data1, "State", "all"),
  parseData(data2, "Residence.State", "all")
) |>
  as_tibble() |>
  arrange(iso3c, date) |>
  distinct(iso3c, date, .keep_all = TRUE)

processAgeGroups <- function(age_groups, ag) {
  # 1999-2020
  ag1 <- data.frame()
  for (age_group in age_groups) {
    data <- read.csv(paste0(
      "../wonder_dl/out/yearly/us_states_1999_2020_",
      age_group,
      ".csv"
    ))
    ag1 <- rbind(ag1, parseData(data, "State", age_group))
  }

  # 2020+
  ag2 <- data.frame()
  for (age_group in age_groups) {
    data <- read.csv(paste0(
      "../wonder_dl/out/yearly/us_states_2021_n_",
      age_group,
      ".csv"
    ))
    ag2 <- rbind(ag2, parseData(data, "Residence.State", age_group))
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

imputeFromAggregate <- function(df1, df2, aggregate_group, groups) {
  df <- df1 |> filter(age_group %in% groups)
  if (sum(is.na(df$deaths)) == 0) df1[3:4]
  if (sum(is.na(df$deaths)) > 2) stop("more than 1 NA")
  sum_groups <- sum(df$deaths, na.rm = TRUE)
  sum_aggregate <- (df2 |> filter(
    iso3c == unique(df1$iso3c),
    date == unique(df1$date),
    age_group == aggregate_group
  ))$deaths
  target <- sum_aggregate - sum_groups
  if (target > 9) stop("imputed value is >9")

  df1$deaths[df1$age_group %in% df$age_group & is.na(df1$deaths)] <- target
  df1[3:4]
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
result_10y <- processAgeGroups(age_groups, "10y")
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
result_5y <- processAgeGroups(age_groups, "5y")
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
