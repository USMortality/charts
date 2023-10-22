source("lib/common.r")
options(warn = 1)

mutate <- dplyr::mutate
make_yearmonth <- tsibble::make_yearmonth
.data <- dplyr::.data
select <- dplyr::select
left_join <- dplyr::left_join
tibble <- tibble::tibble

last_complete_year <- 2022

us_states_iso3c <- read_csv("./data_static/usa_states_iso3c.csv")

data1 <- read_csv("../wonder_dl/out/usa_month_all_1999_2020.csv")
data2 <- read_csv("../wonder_dl/out/usa_month_all_2018_2021.csv")
data3 <- read_csv("../wonder_dl/out/usa_month_all_2022_2023.csv")
data4 <- read_csv("../wonder_dl/out/usa_state_month_all_1999_2020.csv")
data5 <- read_csv("../wonder_dl/out/usa_state_month_all_2018_2021.csv")
data6 <- read_csv("../wonder_dl/out/usa_state_month_all_2022_2023.csv")

parse_data <- function(df, jurisdiction_column, age_group) {
  df <- df |>
    mutate(
      date = make_yearmonth(
        year = as.numeric(left(`Month Code`, 4)),
        month = as.numeric(right(`Month Code`, 2))
      )
    )
  if (nchar(jurisdiction_column) == 0) {
    df <- df |>
      select("date", "Deaths") |>
      setNames(c("date", "deaths"))
    df$iso3c <- "USA"
    df |>
      filter(!is.na(date)) |>
      select("iso3c", "date", "deaths") |>
      mutate(age_group = gsub("_", "-", age_group), .after = date)
  } else {
    df |>
      select(!!jurisdiction_column, "date", "Deaths") |>
      setNames(c("jurisdiction", "date", "deaths")) |>
      left_join(us_states_iso3c, by = "jurisdiction") |>
      filter(!is.na(iso3c), !is.na(date)) |>
      select("iso3c", "date", "deaths") |>
      mutate(age_group = gsub("_", "-", age_group), .after = date)
  }
}

result_all <- rbind(
  parse_data(df = data1, jurisdiction_column = "", age_group = "all"),
  parse_data(df = data2, jurisdiction_column = "", age_group = "all"),
  parse_data(df = data3, jurisdiction_column = "", age_group = "all"),
  parse_data(df = data4, jurisdiction_column = "State", age_group = "all"),
  parse_data(
    df = data5, jurisdiction_column = "Residence State", age_group = "all"
  ),
  parse_data(
    df = data6, jurisdiction_column = "Residence State", age_group = "all"
  )
) |>
  arrange(iso3c, date) |>
  distinct(iso3c, date, .keep_all = TRUE)

process_age_groups <- function(prefix, age_groups) {
  # 1999-2020
  ag1 <- tibble()
  for (age_group in age_groups) {
    data <- read_csv(paste0(
      "../wonder_dl/out/usa", prefix, "_month_",
      age_group,
      "_1999_2020.csv"
    ))
    if (nchar(prefix) == 0) {
      ag1 <- rbind(ag1, parse_data(data, "", age_group))
    } else {
      ag1 <- rbind(ag1, parse_data(data, "State", age_group))
    }
  }

  # 2018-2021
  ag2 <- tibble()
  for (age_group in age_groups) {
    data <- read_csv(paste0(
      "../wonder_dl/out/usa", prefix, "_month_",
      age_group,
      "_2018_2021.csv"
    ))
    if (nchar(prefix) == 0) {
      ag2 <- rbind(ag2, parse_data(data, "", age_group))
    } else {
      ag2 <- rbind(ag2, parse_data(data, "Residence State", age_group))
    }
  }

  # 2022+
  ag3 <- tibble()
  for (age_group in age_groups) {
    data <- read_csv(paste0(
      "../wonder_dl/out/usa", prefix, "_month_",
      age_group,
      "_2022_2023.csv"
    ))
    if (nchar(prefix) == 0) {
      ag3 <- rbind(ag3, parse_data(data, "", age_group))
    } else {
      ag3 <- rbind(ag3, parse_data(data, "Residence State", age_group))
    }
  }

  rbind(ag1, ag2, ag3) |>
    arrange(iso3c, date, age_group) |>
    distinct(iso3c, date, age_group, .keep_all = TRUE)
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

result_10y <- rbind(
  process_age_groups(prefix = "", age_groups),
  process_age_groups(prefix = "_state", age_groups)
) |>
  mutate(
    age_group = ifelse(age_group == "90-100", "90+", age_group),
    deaths = as.integer(ifelse(deaths == "Suppressed", NA, deaths))
  ) |>
  complete(iso3c, date, age_group)

year_10y <- read_csv(
  "./out/deaths/usa/yearly_10y_completed.csv",
  col_types = "cicic"
) |>
  rename(year = date)
result_10y_completed <-
  result_10y |>
  # Complete via state/date totals
  group_by(iso3c, date) |>
  group_modify(
    ~ complete_single_na(
      .x,
      df2 = result_all,
      col = "deaths",
      groups = colnames(.y)
    ),
    .keep = TRUE
  ) |>
  ungroup() |>
  # Complete via state/year/age_group totals
  mutate(year = year(date)) |>
  group_by(iso3c, year, age_group) |>
  group_modify(
    ~ complete_single_na(
      .x,
      df2 = year_10y,
      col = "deaths",
      groups = colnames(.y)
    ),
    .keep = TRUE
  ) |>
  ungroup() |>
  select(-year)

# Impute rest via state/year/age_group totals
result_10y_imputed <- result_10y_completed |>
  mutate(year = year(date)) |>
  group_by(iso3c, year, age_group) |>
  group_modify(
    ~ impute_weighted_sum(
      df = .x,
      df2 = year_10y,
      nom_col = "deaths",
      denom_col = NA, # Spread evenly
      groups = colnames(.y)
    ),
    .keep = TRUE
  ) |>
  ungroup() |>
  select(-year)

stopifnot(nrow(
  result_10y_imputed |> filter(
    date <= make_yearmonth(year = last_complete_year, month = 12),
    is.na(deaths)
  )
) == 0)

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
  "95_100"
)

result_5y <- rbind(
  process_age_groups("", age_groups), # USA
  process_age_groups("_state", age_groups) # States
) |>
  mutate(
    age_group = ifelse(age_group == "95-100", "95+", age_group),
    deaths = as.integer(ifelse(deaths == "Suppressed", NA, deaths))
  ) |>
  complete(iso3c, date, age_group)

# Use NS from 10y
result_5y <- rbind(
  result_5y |> mutate(comment = NA),
  result_10y_completed |> filter(age_group == "NS")
) |>
  arrange(iso3c, date, age_group)

# Complete 5y
year_5y <- read_csv(
  "./out/deaths/usa/yearly_5y_completed.csv",
  col_types = "cicic"
) |>
  rename(year = date)
result_5y_completed <-
  result_5y |>
  # Complete via state/date totals
  group_by(iso3c, date) |>
  group_modify(
    ~ complete_single_na(
      .x,
      df2 = result_all,
      col = "deaths",
      groups = colnames(.y)
    ),
    .keep = TRUE
  ) |>
  ungroup() |>
  # Complete via state/year/age_group totals
  mutate(year = year(date)) |>
  group_by(iso3c, year, age_group) |>
  group_modify(
    ~ complete_single_na(
      .x,
      df2 = year_5y,
      col = "deaths",
      groups = colnames(.y)
    ),
    .keep = TRUE
  ) |>
  ungroup() |>
  select(-year) |>
  # Complete via 10y aggregate
  group_by(iso3c, date) |>
  group_modify(
    ~ complete_from_aggregate(
      df = .x,
      df2 = result_10y_completed,
      aggregate_group = "0-9",
      target_groups = c("0-4", "5-9"),
      groups = colnames(.y)
    ),
    .keep = TRUE
  ) |>
  group_modify(
    ~ complete_from_aggregate(
      df = .x,
      df2 = result_10y_completed,
      aggregate_group = "10-19",
      target_groups = c("10-14", "15-19"),
      groups = colnames(.y)
    ),
    .keep = TRUE
  ) |>
  group_modify(
    ~ complete_from_aggregate(
      df = .x,
      df2 = result_10y_completed,
      aggregate_group = "20-29",
      target_groups = c("20-24", "25-29"),
      groups = colnames(.y)
    ),
    .keep = TRUE
  ) |>
  group_modify(
    ~ complete_from_aggregate(
      df = .x,
      df2 = result_10y_completed,
      aggregate_group = "30-39",
      target_groups = c("30-34", "35-39"),
      groups = colnames(.y)
    ),
    .keep = TRUE
  ) |>
  group_modify(
    ~ complete_from_aggregate(
      df = .x,
      df2 = result_10y_completed,
      aggregate_group = "40-49",
      target_groups = c("40-44", "45-49"),
      groups = colnames(.y)
    ),
    .keep = TRUE
  ) |>
  ungroup()

result_5y_imputed <- result_5y_completed |>
  mutate(year = year(date)) |>
  group_by(iso3c, year, age_group) |>
  # Impute rest via state/year/age_group totals
  group_modify(
    ~ impute_weighted_sum(
      df = .x,
      df2 = year_5y,
      nom_col = "deaths",
      denom_col = NA, # Spread evenly
      groups = colnames(.y)
    ),
    .keep = TRUE
  ) |>
  ungroup() |>
  select(-year)

stopifnot(nrow(
  result_5y_imputed |> filter(
    date <= make_yearmonth(year = last_complete_year, month = 12),
    is.na(deaths)
  )
) == 0)

save_csv(
  rbind(result_10y, result_all) |> arrange(iso3c, date, age_group),
  "deaths/usa/monthly_10y",
  upload = FALSE
)
save_csv(
  rbind(result_10y_completed, result_all |> mutate(comment = NA)) |>
    arrange(iso3c, date, age_group),
  "deaths/usa/monthly_10y_completed",
  upload = FALSE
)
save_csv(
  rbind(result_10y_imputed, result_all |> mutate(comment = NA)) |>
    arrange(iso3c, date, age_group),
  "deaths/usa/monthly_10y_imputed",
  upload = FALSE
)
save_csv(
  rbind(result_5y, result_all |> mutate(comment = NA)) |>
    arrange(iso3c, date, age_group),
  "deaths/usa/monthly_5y",
  upload = FALSE
)
save_csv(
  rbind(result_5y_completed, result_all |> mutate(comment = NA)) |>
    arrange(iso3c, date, age_group),
  "deaths/usa/monthly_5y_completed",
  upload = FALSE
)
save_csv(
  rbind(result_5y_imputed, result_all |> mutate(comment = NA)) |>
    arrange(iso3c, date, age_group),
  "deaths/usa/monthly_5y_imputed",
  upload = FALSE
)

# source("mortality/usa/deaths_monthly.r")
