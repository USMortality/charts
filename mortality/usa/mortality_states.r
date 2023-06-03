source("lib/common.r")

aggregate_80Plus <- function(df) {
  df |>
    mutate(
      age_group = case_when(
        age_group %in% c("80-89", "90-100") ~ "80+",
        .default = age_group
      )
    ) |>
    group_by(iso3c, date, age_group) |>
    summarise(deaths = sum(deaths)) |>
    ungroup()
}

# Weekly 2017+
wd_usa <- read_remote("deaths/usa/age_weekly_2015-n.csv") |>
  filter(year >= 2017) |> # Totals are only available from 2017.
  mutate(date = make_yearweek(year = year, week = week))

md_usa_10y <- read_remote("deaths/usa/monthly_10y_complete.csv") |>
  mutate(date = make_yearmonth(year = year, month = month)) |>
  aggregate_80Plus()

# CMR, Weekly
dd_us1 <- wd_usa |>
  filter(age_group == "all") |>
  getDailyFromWeekly(c("deaths")) |>
  select(iso3c, date, deaths)
dd_us1$age_group <- "all"
dd_us1$type <- "weekly"

# CMR, Monthly
dd_us2 <- md_usa_10y |>
  filter(age_group == "all") |>
  getDailyFromMonthly(c("deaths")) |>
  select(iso3c, date, deaths)
dd_us2$age_group <- "all"
dd_us2$type <- "monthly"

dd_us <- rbind(dd_us1, dd_us2) |>
  distinct(iso3c, date, type, .keep_all = TRUE) |>
  arrange(iso3c, date, age_group, type)

# ASMR
## Weekly
n_ <- nrow(wd_usa |> filter(!is.na(deaths) & iso3c == "USA"))
complete_states_weekly <- wd_usa |>
  filter(!is.na(deaths)) |>
  group_by(iso3c) |>
  count(iso3c) |>
  filter(n == n_)
deaths_weekly <- wd_usa |>
  filter(iso3c %in% complete_states_weekly$iso3c, age_group != "all") |>
  group_by(iso3c, age_group) |>
  group_modify(~ getDailyFromWeekly(.x, c("deaths"))) |>
  ungroup()
deaths_weekly$type <- "weekly"

## Monthly
n_ <- nrow(md_usa_10y |> filter(iso3c == "USA"))
# Use USA national data as reference for completeness.
complete_states_monthly <- md_usa_10y |>
  filter(!is.na(deaths)) |>
  group_by(iso3c) |>
  count(iso3c) |>
  filter(n == n_)
deaths_monthly <- md_usa_10y |>
  filter(iso3c %in% complete_states_monthly$iso3c, age_group != "all") |>
  group_by(iso3c, age_group) |>
  group_modify(~ getDailyFromMonthly(.x, c("deaths"))) |>
  ungroup()
deaths_monthly$type <- "monthly"

## Yearly
deaths_yearly <- read_remote("deaths/usa/yearly_10y_complete.csv") |>
  aggregate_80Plus() |>
  filter(!iso3c %in% complete_states_monthly$iso3c, age_group != "all") |>
  mutate(date = as.Date(paste0(date, "-01-01"))) |>
  group_by(iso3c, age_group) |>
  group_modify(~ getDailyFromYearly(.x, c("deaths"))) |>
  ungroup()
deaths_yearly$type <- "yearly"

dd_us_age <- rbind(
  deaths_monthly,
  deaths_yearly
) |>
  filter(!(iso3c %in% complete_states_weekly$iso3c) |
    iso3c %in% complete_states_weekly$iso3c) |>
  rbind(deaths_weekly |> select(-year, -week)) |>
  relocate(iso3c, date, age_group, deaths) |>
  arrange(iso3c, date, age_group)

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
  select(iso3c, date, age_group, deaths, population, type) |>
  arrange(iso3c, date, age_group) |>
  distinct(iso3c, date, age_group, type, .keep_all = TRUE) |>
  filter(age_group != "NS", !is.na(population))

usa_mortality_states$source <- "cdc"
