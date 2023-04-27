source("lib/common.r")
source("lib/asmr.r")

deaths <- as_tibble(read.csv("./data/mortality_org.csv", skip = 2))

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
  dplyr::mutate(mortality := mortality / 52 * 100000)

wd$iso3c[wd$iso3c == "DEUTNP"] <- "DEU"
wd$iso3c[wd$iso3c == "FRATNP"] <- "FRA"
wd$iso3c[wd$iso3c == "NZL_NP"] <- "NZL"
wd$iso3c[wd$iso3c == "GBR_NP"] <- "GBR"

dd_asmr <- wd |>
  calculate_asmr_variants() |>
  getDailyFromWeekly(c("asmr", "asmr_esp"))

# USA < 2015
dd_usa_asmr <- rbind(
  get_usa_mortality("0_14"),
  get_usa_mortality("15_64"),
  get_usa_mortality("65_74"),
  get_usa_mortality("75_84"),
  get_usa_mortality("85+")
) |>
  mutate(date = make_yearmonth(year = year, month = time), iso3c = "USA") |>
  calculate_asmr_variants() |>
  getDailyFromMonthly(c("asmr", "asmr_esp"))

# Combined DS
dd_asmr <- rbind(
  dd_asmr |> filter(iso3c != "USA"),
  dd_usa_asmr |> filter(date < as.Date("2015-01-05")),
  dd_asmr |> filter(iso3c == "USA")
) |>
  mutate(iso3c_g = iso3c) |>
  nest(data = c(iso3c, date, asmr)) |>
  mutate(data = lapply(data, maybe_impute_gaps)) |>
  unnest(cols = c(data)) |>
  select(-iso3c_g) |>
  relocate(c("iso3c", "date", "asmr", "asmr_esp"))
