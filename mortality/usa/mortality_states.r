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

aggregate_80_plus <- function(df) {
  df |>
    mutate(
      age_group = case_when(
        age_group %in% c("80-89", "90+") ~ "80+",
        .default = age_group
      )
    ) |>
    group_by(.data$iso3c, .data$date, .data$age_group) |>
    summarise(deaths = sum(.data$deaths)) |>
    ungroup()
}

# Weekly 2017+
wd_usa <- read_remote("deaths/usa/age_weekly_2015-n.csv") |>
  filter(year >= 2017) |> # Totals are only available from 2017.
  mutate(date = date_parse(paste(year, week, 1), format = "%G %V %u")) |>
  filter(!is.na(deaths))

md_usa_10y <- read_remote("deaths/usa/monthly_10y_complete.csv") |>
  mutate(date = date_parse(paste(year, month, 1), format = "%Y %m %d")) |>
  aggregate_80_plus()

# CMR, Weekly
dd_us1 <- wd_usa |>
  filter(age_group == "all") |>
  select(iso3c, date, deaths)
dd_us1$age_group <- "all"
dd_us1$type <- 3

# CMR, Monthly
dd_us2 <- md_usa_10y |>
  filter(age_group == "all") |>
  select(iso3c, date, deaths)
dd_us2$age_group <- "all"
dd_us2$type <- 2

dd_us <- rbind(dd_us1, dd_us2) |>
  distinct(iso3c, date, type, .keep_all = TRUE) |>
  arrange(iso3c, date, age_group, type)
dd_us$n_age_groups <- 1

rm(dd_us1, dd_us2)

# ASMR
## Weekly
n_ <- nrow(wd_usa |> filter(iso3c == "USA"))
complete_states_weekly <- wd_usa |>
  filter(!is.na(deaths)) |>
  group_by(iso3c) |>
  count(iso3c) |>
  filter(n == n_)
deaths_weekly <- wd_usa |>
  filter(
    iso3c %in% complete_states_weekly$iso3c,
    age_group != "all",
    !is.na(date)
  )
deaths_weekly$type <- 3
deaths_weekly$n_age_groups <- 6

rm(wd_usa, n_)

## Monthly
n_ <- nrow(md_usa_10y |> filter(iso3c == "USA"))
# Use USA national data as reference for completeness.
complete_states_monthly <- md_usa_10y |>
  filter(!is.na(deaths)) |>
  group_by(iso3c) |>
  count(iso3c) |>
  filter(n == n_)
deaths_monthly <- md_usa_10y |>
  filter(
    iso3c %in% complete_states_monthly$iso3c,
    age_group != "all", !is.na(date)
  )
deaths_monthly$type <- 2
deaths_monthly$n_age_groups <- 9

rm(md_usa_10y, n_)

## Yearly
deaths_yearly <- read_remote("deaths/usa/yearly_10y_complete.csv") |>
  rename(date = year) |>
  aggregate_80_plus() |>
  filter(
    !iso3c %in% complete_states_monthly$iso3c,
    age_group != "all"
  ) |>
  mutate(date = as.Date(paste0(date, "-01-01")))
deaths_yearly$type <- 1
deaths_yearly$n_age_groups <- 9

rm(complete_states_monthly)

dd_us_age <- rbind(
  deaths_monthly,
  deaths_yearly
) |>
  filter(!(iso3c %in% complete_states_weekly$iso3c) |
    iso3c %in% complete_states_weekly$iso3c) |>
  rbind(deaths_weekly |> select(-year, -week)) |>
  relocate(iso3c, date, age_group, deaths) |>
  arrange(iso3c, date, age_group)

rm(deaths_weekly, deaths_monthly, complete_states_weekly)

# Population
population <- read_remote("population/usa/10y.csv") |>
  mutate(date = date(sprintf("%d-01-01", year)), .after = iso3c) |>
  group_by(iso3c) |>
  nest() |>
  mutate(
    data = map(data, ~ . |>
      group_by(age_group) |>
      nest() |>
      mutate(data = lapply(data, interpolate_population)) |>
      unnest(cols = "data"))
  ) |>
  unnest(cols = "data") |>
  select(iso3c, date, age_group, population)

population2 <- read_remote("population/usa/5y.csv") |>
  mutate(
    age_group = case_when(
      age_group %in% c("0-4") ~ "0-24",
      age_group %in% c("5-9") ~ "0-24",
      age_group %in% c("10-14") ~ "0-24",
      age_group %in% c("15-19") ~ "0-24",
      age_group %in% c("20-24") ~ "0-24",
      age_group %in% c("25-29") ~ "25-44",
      age_group %in% c("30-34") ~ "25-44",
      age_group %in% c("35-39") ~ "25-44",
      age_group %in% c("40-44") ~ "25-44",
      age_group %in% c("45-49") ~ "45-64",
      age_group %in% c("50-54") ~ "45-64",
      age_group %in% c("55-59") ~ "45-64",
      age_group %in% c("60-64") ~ "45-64",
      age_group %in% c("65-69") ~ "65-74",
      age_group %in% c("70-74") ~ "65-74",
      age_group %in% c("75-79") ~ "75-84",
      age_group %in% c("80-84") ~ "75-84",
      age_group %in% c("85+") ~ "85+"
    )
  ) |>
  group_by(iso3c, year, age_group) |>
  summarise(population = sum(population)) |>
  ungroup() |>
  filter(!is.na(population)) |>
  mutate(date = date(sprintf("%d-01-01", year)), .after = iso3c) |>
  group_by(iso3c) |>
  nest() |>
  mutate(
    data = map(data, ~ . |>
      group_by(age_group) |>
      nest() |>
      mutate(data = lapply(data, interpolate_population)) |>
      unnest(cols = "data"))
  ) |>
  unnest(cols = "data") |>
  select(iso3c, date, age_group, population)

usa_mortality_states <- rbind(dd_us, dd_us_age) |>
  left_join(
    population,
    by = c("iso3c", "date", "age_group")
  ) |>
  left_join(
    population2,
    by = c("iso3c", "date", "age_group")
  ) |>
  mutate(
    population =
      ifelse(is.na(population.x), population.y, population.x)
  ) |>
  select(iso3c, date, age_group, deaths, population, type, n_age_groups) |>
  arrange(iso3c, date, age_group) |>
  distinct(iso3c, date, age_group, type, .keep_all = TRUE) |>
  filter(age_group != "NS", !is.na(population))

usa_mortality_states$source <- "cdc"

rm(dd_us, dd_us_age, population, population2)
