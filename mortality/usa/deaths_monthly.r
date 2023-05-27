source("lib/common.r")

us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

data0 <- read.csv("../wonder_dl/out/monthly/us_1999_2020_all.csv")
data1 <- read.csv("../wonder_dl/out/monthly/us_2021_n_all.csv")
data2 <- read.csv("../wonder_dl/out/monthly/us_states_1999_2020_all.csv")
data3 <- read.csv("../wonder_dl/out/monthly/us_states_2021_n_all.csv")

parseData <- function(df, jurisdiction_column, age_group) {
  df <- df |>
    mutate(
      date = make_yearmonth(
        year = as.numeric(left(Month.Code, 4)),
        month = as.numeric(right(Month.Code, 2))
      )
    )
  if (nchar(jurisdiction_column) == 0) {
    df <- df |>
      select(date, Deaths) |>
      setNames(c("date", "deaths"))
    df$iso3c <- "USA"
    df |>
      filter(!is.na(iso3c), !is.na(date)) |>
      select(iso3c, date, deaths) |>
      mutate(age_group = gsub("_", "-", age_group), .after = date)
  } else {
    df |>
      select(!!jurisdiction_column, date, Deaths) |>
      setNames(c("jurisdiction", "date", "deaths")) |>
      left_join(us_states_iso3c, by = "jurisdiction") |>
      filter(!is.na(iso3c), !is.na(date)) |>
      select(iso3c, date, deaths) |>
      mutate(age_group = gsub("_", "-", age_group), .after = date)
  }
}

totals <- rbind(
  parseData(data0, "", "all"),
  parseData(data1, "", "all"),
  parseData(data2, "State", "all"),
  parseData(data3, "Residence.State", "all")
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
    )) |> as_tibble()
    if (nchar(prefix) == 0) {
      ag1 <- rbind(ag1, parseData(data, "", age_group))
    } else {
      ag1 <- rbind(ag1, parseData(data, "State", age_group))
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
      ag1 <- rbind(ag1, parseData(data, "", age_group))
    } else {
      ag1 <- rbind(ag1, parseData(data, "Residence.State", age_group))
    }
  }

  totals_ag <- rbind(ag1, ag2) |>
    as_tibble() |>
    arrange(iso3c, date, age_group) |>
    distinct(iso3c, date, age_group, .keep_all = TRUE)

  if (nchar(prefix) == 0 && length(age_groups) > 11) {
    ttl <- totals |> filter(iso3c == "USA")
  } else {
    ttl <- totals |> filter(iso3c != "USA")
  }

  rbind(ttl, totals_ag) |>
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
result_10y <- processAgeGroups("states_", age_groups)
result_10y_complete <- result_10y |>
  group_by(iso3c, date) |>
  group_modify(~ imputeSingleNA(.x)) |>
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
  processAgeGroups("", age_groups), # USA
  processAgeGroups("states_", age_groups) # States
) |>
  mutate(age_group = ifelse(age_group == "95-100", "95+", age_group))

result_5y_complete <- result_5y |>
  group_by(iso3c, date) |>
  group_modify(~ imputeSingleNA(.x)) |>
  group_modify(~ imputeFromAggregate(
    .x, result_10y_complete, "0-9", c("0-4", "5-9")
  ), .keep = TRUE) |>
  group_modify(~ imputeFromAggregate(
    .x, result_10y_complete, "10-19", c("10-14", "15-19")
  ), .keep = TRUE) |>
  group_modify(~ imputeFromAggregate(
    .x, result_10y_complete, "20-29", c("20-24", "25-29")
  ), .keep = TRUE) |>
  group_modify(~ imputeFromAggregate(
    .x, result_10y_complete, "30-39", c("30-34", "35-39")
  ), .keep = TRUE) |>
  group_modify(~ imputeFromAggregate(
    .x, result_10y_complete, "40-49", c("40-44", "45-49")
  ), .keep = TRUE) |>
  ungroup()

aggregate10y <- function(df) {
  df |>
    filter(iso3c == "USA") |>
    mutate(
      # Create categories
      age_group = case_when(
        age_group %in% c("0-4", "5-9") ~ "0-9",
        age_group %in% c("10-14", "15-19") ~ "10-19",
        age_group %in% c("20-24", "25-29") ~ "20-29",
        age_group %in% c("30-34", "35-39") ~ "30-39",
        age_group %in% c("40-44", "45-49") ~ "40-49",
        age_group %in% c("50-54", "55-59") ~ "50-59",
        age_group %in% c("60-64", "65-69") ~ "60-69",
        age_group %in% c("70-74", "75-79") ~ "70-79",
        age_group %in% c("80-84", "85-89", "90-94", "95+") ~ "80+",
        age_group %in% c("NS") ~ "NS",
        age_group %in% c("all") ~ "all"
      )
    ) |>
    group_by(iso3c, date, age_group) |>
    summarise(deaths = sum(deaths)) |>
    ungroup()
}

# Aggregate 10y for USA national.
save_csv(
  result_5y |> mutate(year = year(date), month = month(date)),
  "deaths/usa/monthly_5y",
  upload = TRUE
)
save_csv(
  result_5y_complete |> mutate(year = year(date), month = month(date)),
  "deaths/usa/monthly_5y_complete",
  upload = TRUE
)
save_csv(
  rbind(aggregate10y(result_5y), result_10y) |>
    mutate(year = year(date), month = month(date)),
  "deaths/usa/monthly_10y",
  upload = TRUE
)
save_csv(
  rbind(aggregate10y(result_5y_complete), result_10y_complete) |>
    mutate(year = year(date), month = month(date)),
  "deaths/usa/monthly_10y_complete",
  upload = TRUE
)
