source("lib/common.r")
source("population/std_pop.r")

deaths <- as_tibble(read.csv("./data/mortality_org.csv", skip = 2))
std_pop <- get_esp2013_bins(c("0-14", "15-64", "65-74", "75-84", "85+"))

wd <- deaths |>
  filter(Sex == "b") |>
  select(CountryCode, Year, Week, R0_14, R15_64, R65_74, R75_84, R85p) |>
  mutate(date = make_yearweek(
    year = Year, week = Week
  )) |>
  setNames(c(
    "iso3c", "year", "week", "0-14", "15-64", "65-74", "75-84", "85+", "date"
  )) |>
  pivot_longer(
    cols = 4:8,
    names_to = "age_group",
    values_to = "mortality"
  ) |>
  inner_join(std_pop, by = "age_group") |>
  mutate(asmr = mortality / 52 * percentage) |>
  select(-5, -7) |>
  group_by(iso3c, year, week, date) |>
  summarise(asmr = sum(asmr)) |>
  mutate(asmr = asmr * 100000) |>
  ungroup()

dd_asmr <- wd |>
  getDailyFromWeekly("asmr") |>
  select(iso3c, date, asmr) |>
  distinct(iso3c, date, .keep_all = TRUE)

dd_asmr$iso3c[dd_asmr$iso3c == "DEUTNP"] <- "DEU"
dd_asmr$iso3c[dd_asmr$iso3c == "FRATNP"] <- "FRA"
dd_asmr$iso3c[dd_asmr$iso3c == "NZL_NP"] <- "NZL"
dd_asmr$iso3c[dd_asmr$iso3c == "GBR_NP"] <- "GBR"

# USA < 2015
mortality_usa <- rbind(
  get_usa_mortality("0_14"),
  get_usa_mortality("15_64"),
  get_usa_mortality("65_74"),
  get_usa_mortality("75_84"),
  get_usa_mortality("85+")
)

md_usa <- mortality_usa |>
  inner_join(std_pop, by = "age_group") |>
  mutate(asmr = mortality * percentage) |>
  select(-3, -4, -5) |>
  group_by(year, time) |>
  summarise(asmr = sum(asmr)) |>
  ungroup() |>
  setNames(c("year", "month", "asmr")) |>
  mutate(date = make_yearmonth(year = year, month = month)) |>
  getDailyFromMonthly("asmr") |>
  select(4, 3) |>
  setNames(c("date", "asmr"))

md_usa$iso3c <- "USA"
md_usa <- md_usa |> relocate(iso3c, date, asmr)

dd_asmr <- rbind(
  dd_asmr |> filter(iso3c != "USA"),
  md_usa |> filter(date < as.Date("2015-01-05")),
  dd_asmr |> filter(iso3c == "USA")
)

maybe_impute_gaps <- function(data) {
  ts <- data |> as_tsibble(index = date, validate = FALSE)
  if (has_gaps(ts)$.gaps == FALSE) {
    return(data)
  }
  result <- ts |>
    fill_gaps(.full = start()) |>
    tidyr::fill(iso3c)
  result$asmr <- na_ma(result$asmr)
  result |> as_tibble()
}

dd_asmr <- dd_asmr |>
  mutate(iso3c_g = iso3c) |>
  nest(data = c(iso3c, date, asmr)) |>
  mutate(data = lapply(data, maybe_impute_gaps)) |>
  unnest(cols = c(data)) |>
  select(-iso3c_g)
