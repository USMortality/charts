source("lib/common.r")
options(warn = 1)

select <- dplyr::select
arrange <- dplyr::arrange
distinct <- dplyr::distinct
left_join <- dplyr::left_join
mutate <- dplyr::mutate
tibble <- tibble::tibble
read_csv <- readr::read_csv

last_complete_year <- 2022

us_states_iso3c <- read_csv("./data_static/usa_states_iso3c.csv")

data1 <- read_csv("../wonder_dl/out/usa_year_all_1999_2020.csv")
data2 <- read_csv("../wonder_dl/out/usa_year_all_2018_2021.csv")
data3 <- read_csv("../wonder_dl/out/usa_year_all_2022_2023.csv")
data4 <- read_csv("../wonder_dl/out/usa_state_year_all_1999_2020.csv")
data5 <- read_csv("../wonder_dl/out/usa_state_year_all_2018_2021.csv")
data6 <- read_csv("../wonder_dl/out/usa_state_year_all_2022_2023.csv")

parse_data <- function(df, jurisdiction_column, age_group) {
  df <- df |> mutate(date = `Year Code`)
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
  parse_data(data1, "", "all"),
  parse_data(data2, "", "all"),
  parse_data(data3, "", "all"),
  parse_data(data4, "State", "all"),
  parse_data(data5, "Residence State", "all"),
  parse_data(data6, "Residence State", "all")
) |>
  arrange(iso3c, date) |>
  distinct(iso3c, date, .keep_all = TRUE) |>
  filter(!is.na(deaths))

stopifnot(nrow(result_all |> filter(is.na(deaths))) == 0)

process_age_groups <- function(prefix, age_groups) {
  # 1999-2020
  ag1 <- tibble()
  for (age_group in age_groups) {
    data <- read_csv(paste0(
      "../wonder_dl/out/usa", prefix, "_year_",
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
      "../wonder_dl/out/usa", prefix, "_year_",
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
      "../wonder_dl/out/usa", prefix, "_year_",
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

# year/10y must be already complete, except NS
result_10y_completed <- result_10y |>
  filter(date <= last_complete_year) |>
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
  ungroup()

result_10y <- rbind(result_all, result_10y) |> arrange(iso3c, date, age_group)
result_10y_completed <- rbind(
  result_all |> mutate(comment = NA),
  result_10y_completed
) |>
  arrange(iso3c, date, age_group)
stopifnot(nrow(
  result_10y_completed |> filter(date <= last_complete_year, is.na(deaths))
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
  "95_100",
  "NS"
)
result_5y <- rbind(
  process_age_groups(prefix = "", age_groups),
  process_age_groups(prefix = "_state", age_groups)
) |>
  mutate(
    age_group = ifelse(age_group == "95-100", "95+", age_group),
    deaths = as.integer(ifelse(deaths == "Suppressed", NA, deaths))
  ) |>
  complete(iso3c, date, age_group)

# Use NS from 10y
result_5y_completed <- rbind(
  result_10y_completed |> filter(age_group == "NS"),
  result_5y |> filter(age_group != "NS") |> mutate(comment = NA)
) |> arrange(iso3c, date, age_group)

# Complete NA's
result_5y_completed <- result_5y_completed |>
  filter(date <= last_complete_year) |>
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
  ungroup()

result_5y <- rbind(result_all, result_5y) |> arrange(iso3c, date, age_group)
result_5y_completed <- rbind(
  result_all |> mutate(comment = NA),
  result_5y_completed
) |>
  arrange(iso3c, date, age_group)
stopifnot(nrow(
  result_5y_completed |> filter(date <= last_complete_year, is.na(deaths))
) == 0)

save_csv(result_10y, "deaths/usa/yearly_10y", upload = FALSE)
save_csv(
  result_10y_completed,
  "deaths/usa/yearly_10y_completed",
  upload = FALSE
)
save_csv(result_5y, "deaths/usa/yearly_5y", upload = FALSE)
save_csv(result_5y_completed, "deaths/usa/yearly_5y_completed", upload = FALSE)

# source("mortality/usa/deaths_yearly.r")
