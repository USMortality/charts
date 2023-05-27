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

md_usa_10y <- read_remote("deaths/usa/monthly_10y_complete.csv") |>
  mutate(date = make_yearmonth(year = year, month = month)) |>
  aggregate_80Plus()

# CMR, Monthly
dd_us <- md_usa_10y |>
  filter(age_group == "all") |>
  getDailyFromMonthly(c("deaths")) |>
  select(iso3c, date, deaths) |>
  distinct(iso3c, date, .keep_all = TRUE)
dd_us$age_group <- "all"

# ASMR
## Monthly
n_ <- nrow(md_usa_10y |> filter(iso3c == "USA"))
# Use USA national data as reference for completeness.
complete_states <- md_usa_10y |>
  filter(!is.na(deaths)) |>
  group_by(iso3c) |>
  count(iso3c) |>
  filter(n == n_)
deaths_monthly <- md_usa_10y |>
  filter(iso3c %in% complete_states$iso3c, age_group != "all") |>
  group_by(iso3c, age_group) |>
  group_modify(~ getDailyFromMonthly(.x, c("deaths"))) |>
  ungroup()

## Yearly
deaths_yearly <- read_remote("deaths/usa/yearly_10y_complete.csv") |>
  aggregate_80Plus() |>
  filter(!iso3c %in% complete_states$iso3c, age_group != "all") |>
  mutate(date = as.Date(paste0(date, "-01-01"))) |>
  group_by(iso3c, age_group) |>
  group_modify(~ getDailyFromYearly(.x, c("deaths"))) |>
  ungroup()

dd_us_age <- rbind(deaths_yearly, deaths_monthly)

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

usa_mortality_states <- rbind(dd_us, dd_us_age) |>
  inner_join(
    population,
    by = c("iso3c", "date", "age_group")
  ) |>
  arrange(iso3c, date, age_group) |>
  distinct(iso3c, date, age_group, .keep_all = TRUE)
