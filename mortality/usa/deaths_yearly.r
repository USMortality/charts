source("lib/common.r")

us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

data1 <- read.csv("../wonder_dl/out/yearly/us_states_1999_2020_all.csv")
data2 <- read.csv("../wonder_dl/out/yearly/us_states_2020_n_all.csv")

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
      "../wonder_dl/out/yearly/us_states_2020_n_",
      age_group,
      ".csv"
    ))
    ag2 <- rbind(ag2, parseData(data, "Residence.State", age_group))
  }

  totals_ag <- rbind(ag1, ag2) |>
    as_tibble() |>
    arrange(iso3c, date, age_group) |>
    distinct(iso3c, date, age_group, .keep_all = TRUE)

  result <- rbind(totals, totals_ag) |>
    arrange(iso3c, date, age_group) |>
    distinct(iso3c, date, age_group, .keep_all = TRUE) |>
    complete(iso3c, date, age_group)

  save_csv(result, paste0("deaths/usa/yearly_", ag), upload = TRUE)
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
processAgeGroups(age_groups, "10y")

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
processAgeGroups(age_groups, "5y")
