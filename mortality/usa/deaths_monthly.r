source("lib/common.r")

# Define default functions
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
group_by <- dplyr::group_by
ungroup <- dplyr::ungroup
summarise <- dplyr::summarise
inner_join <- dplyr::inner_join
relocate <- dplyr::relocate
year <- lubridate::year
month <- lubridate::month
week <- lubridate::week
days <- lubridate::days
days_in_month <- lubridate::days_in_month
as_tibble <- tibble::as_tibble
tibble <- tibble::tibble
as_tsibble <- tsibble::as_tsibble
str_replace <- stringr::str_replace
uncount <- tidyr::uncount
sym <- rlang::sym
model <- fabletools::model
date <- lubridate::date
forecast <- fabletools::forecast
select <- dplyr::select
all_of <- dplyr::all_of
nest <- tidyr::nest
unnest <- tidyr::unnest
.data <- dplyr::.data
yearmonth <- tsibble::yearmonth
yearweek <- tsibble::yearweek
ggplot <- ggplot2::ggplot
make_yearmonth <- tsibble::make_yearmonth
arrange <- dplyr::arrange
distinct <- dplyr::distinct
complete <- tidyr::complete
case_when <- dplyr::case_when

us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

data0 <- read.csv("../wonder_dl/out/monthly/us_1999_2020_all.csv")
data1 <- read.csv("../wonder_dl/out/monthly/us_2021_n_all.csv")
data2 <- read.csv("../wonder_dl/out/monthly/us_states_1999_2020_all.csv")
data3 <- read.csv("../wonder_dl/out/monthly/us_states_2021_n_all.csv")

parse_data <- function(df, jurisdiction_column, age_group) {
  df <- df |>
    mutate(
      year = as.numeric(left(.data$Month.Code, 4)),
      month = as.numeric(right(.data$Month.Code, 2))
    ) |>
    filter(!is.na(year), !is.na(month)) |>
    mutate(
      date =
        suppress_warnings(
          yearmonth(
            date_parse(
              paste(
                left(.data$Month.Code, 4),
                right(.data$Month.Code, 2), 1
              ),
              format = "%Y %m %d"
            )
          ),
          paste(
            "Failed to parse 1 string at location 1.",
            "Returning `NA` at that location."
          )
        )
    ) |>
    select(-year, -month)
  if (nrow(df) == 0) {
    return(setNames(
      data.frame(matrix(ncol = 4, nrow = 0)),
      c("iso3c", "date", "age_group", "deaths")
    ))
  }

  if (nchar(jurisdiction_column) == 0) {
    df <- df |>
      select("date", "Deaths") |>
      setNames(c("date", "deaths"))
    df$iso3c <- "USA"
    df |>
      filter(!is.na("iso3c"), !is.na("date")) |>
      select("iso3c", "date", "deaths") |>
      mutate(age_group = gsub("_", "-", age_group), .after = date)
  } else {
    df |>
      select(!!jurisdiction_column, "date", "Deaths") |>
      setNames(c("jurisdiction", "date", "deaths")) |>
      dplyr::left_join(us_states_iso3c, by = "jurisdiction") |>
      filter(!is.na(iso3c), !is.na(date)) |>
      select("iso3c", "date", "deaths") |>
      mutate(age_group = gsub("_", "-", age_group), .after = date)
  }
}

totals <- rbind(
  parse_data(
    df = data0,
    jurisdiction_column = "",
    age_group = "all"
  ),
  parse_data(
    df = data1,
    jurisdiction_column = "", age_group = "all"
  ),
  parse_data(
    df = data2,
    jurisdiction_column = "State",
    age_group = "all"
  ),
  parse_data(
    df = data3,
    jurisdiction_column = "Residence.State",
    age_group = "all"
  )
) |>
  as_tibble() |>
  arrange(iso3c, date) |>
  distinct(iso3c, date, .keep_all = TRUE)

processAgeGroups <- function(prefix, age_groups) {
  # 1999-2020
  ag1 <- data.frame()
  for (age_group in age_groups) {
    data <- read.csv(paste0(
      "../wonder_dl/out/monthly/us_", prefix, "1999_2020_",
      age_group,
      ".csv"
    ))
    if (nchar(prefix) == 0) {
      ag1 <- rbind(ag1, parse_data(
        df = data,
        jurisdiction_column = "",
        age_group
      ))
    } else {
      ag1 <- rbind(ag1, parse_data(
        df = data,
        jurisdiction_column = "State",
        age_group
      ))
    }
  }

  # 2020+
  ag2 <- data.frame()
  for (age_group in age_groups) {
    data <- read.csv(paste0(
      "../wonder_dl/out/monthly/us_", prefix, "2021_n_",
      age_group,
      ".csv"
    ))
    if (nchar(prefix) == 0) {
      ag1 <- rbind(ag1, parse_data(
        df = data,
        jurisdiction_column = "",
        age_group
      ))
    } else {
      ag1 <- rbind(ag1, parse_data(
        df = data,
        jurisdiction_column = "Residence.State",
        age_group
      ))
    }
  }

  totals_ag <- rbind(ag1, ag2) |>
    as_tibble() |>
    arrange("iso3c", "date", "age_group") |>
    distinct(.data$iso3c, .data$date, .data$age_group, .keep_all = TRUE)

  if (nchar(prefix) == 0 && length(age_groups) > 11) {
    ttl <- totals |> filter(.data$iso3c == "USA")
  } else {
    ttl <- totals |> filter(.data$iso3c != "USA")
  }

  rbind(ttl, totals_ag) |>
    arrange("iso3c", "date", "age_group") |>
    distinct(.data$iso3c, .data$date, .data$age_group, .keep_all = TRUE) |>
    complete(.data$iso3c, .data$date, .data$age_group)
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
result_10y <- processAgeGroups(prefix = "states_", age_groups) |>
  mutate(age_group = ifelse(age_group == "90-100", "90+", age_group))

result_10y_complete <- result_10y |>
  group_by(iso3c, date) |>
  group_modify(~ impute_single_na(.x)) |>
  ungroup()

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
  processAgeGroups(prefix = "", age_groups), # USA
  processAgeGroups(prefix = "states_", age_groups) # States
) |>
  mutate(age_group = ifelse(age_group == "95-100", "95+", age_group))

result_5y_complete <- result_5y |>
  group_by(iso3c, date) |>
  group_modify(~ impute_single_na(.x)) |>
  group_modify(~ impute_from_aggregate(
    .x, result_10y_complete, "0-9", c("0-4", "5-9")
  ), .keep = TRUE) |>
  group_modify(~ impute_from_aggregate(
    .x, result_10y_complete, "10-19", c("10-14", "15-19")
  ), .keep = TRUE) |>
  group_modify(~ impute_from_aggregate(
    .x, result_10y_complete, "20-29", c("20-24", "25-29")
  ), .keep = TRUE) |>
  group_modify(~ impute_from_aggregate(
    .x, result_10y_complete, "30-39", c("30-34", "35-39")
  ), .keep = TRUE) |>
  group_modify(~ impute_from_aggregate(
    .x, result_10y_complete, "40-49", c("40-44", "45-49")
  ), .keep = TRUE) |>
  ungroup()

aggregate10y <- function(df) {
  df |>
    filter(.data$iso3c == "USA") |>
    mutate(
      # Create categories
      age_group = case_when(
        .data$age_group %in% c("0-4", "5-9") ~ "0-9",
        .data$age_group %in% c("10-14", "15-19") ~ "10-19",
        .data$age_group %in% c("20-24", "25-29") ~ "20-29",
        .data$age_group %in% c("30-34", "35-39") ~ "30-39",
        .data$age_group %in% c("40-44", "45-49") ~ "40-49",
        .data$age_group %in% c("50-54", "55-59") ~ "50-59",
        .data$age_group %in% c("60-64", "65-69") ~ "60-69",
        .data$age_group %in% c("70-74", "75-79") ~ "70-79",
        .data$age_group %in% c("80-84", "85-89") ~ "80-89",
        .data$age_group %in% c("90-94", "95+") ~ "90+",
        .data$age_group %in% c("NS") ~ "NS",
        .data$age_group %in% c("all") ~ "all"
      )
    ) |>
    group_by(iso3c, date, age_group) |>
    summarise(deaths = sum(.data$deaths)) |>
    ungroup()
}

# Aggregate 10y for USA national.
save_csv(
  result_5y |> mutate(year = year(date), month = month(date)),
  "deaths/usa/monthly_5y"
)
save_csv(
  result_5y_complete |> mutate(year = year(date), month = month(date)),
  "deaths/usa/monthly_5y_complete"
)
save_csv(
  rbind(aggregate10y(result_5y), result_10y) |>
    mutate(year = year(date), month = month(date)),
  "deaths/usa/monthly_10y"
)
save_csv(
  rbind(aggregate10y(result_5y_complete), result_10y_complete) |>
    mutate(year = year(date), month = month(date)),
  "deaths/usa/monthly_10y_complete"
)
