source("lib/common.r")

# CMR
md_usa <- get_usa_deaths("./data_static/usa_all.csv") # USA 1999/1 - 2020/12
# US States 1999/1 - 2020/12
us_cmr_1 <- as_tibble(read.csv("./data_static/usa_states_1999_2020.csv"))
us_cmr_2 <- as_tibble(read.csv("./data/usa_states_age_cause_weekly.csv"))
us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

# Format: iso3c, country_name, year, time, time_unit, deaths
md_us_states <- us_cmr_1 |>
  mutate(
    jurisdiction = State,
    year = as.numeric(left(Month.Code, 4)),
    time = as.numeric(right(Month.Code, 2)),
    time_unit = "monthly"
  ) |>
  left_join(
    us_states_iso3c,
    by = "jurisdiction"
  ) |>
  select(13, 2, 10, 11, 12, 6) |>
  setNames(
    c("iso3c", "country_name", "year", "time", "time_unit", "deaths")
  ) |>
  filter(!is.na(year)) |>
  rbind(md_usa)

wd_us_states <- us_cmr_2 |>
  select(2, 3, 4, 6) |>
  setNames(c("country_name", "year", "time", "deaths")) |>
  left_join(
    us_states_iso3c |> mutate(country_name = jurisdiction),
    by = "country_name"
  ) |>
  mutate(
    time_unit = "weekly",
    country_name = ifelse(
      country_name == "United States",
      "United States",
      paste0("USA - ", country_name)
    ),
    deaths = ifelse(deaths == 0, NA, deaths)
  ) |>
  select(5, 1, 2, 3, 7, 4) |>
  filter(!is.na(deaths)) |>
  filter(!is.na(iso3c))

# Combine NY/NYC
wd_us_states_ny <- wd_us_states |>
  filter(country_name %in% c("USA - New York", "USA - New York City")) |>
  group_by(year, time, time_unit) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()
wd_us_states_ny$iso3c <- "USA-NY"
wd_us_states_ny$country_name <- "USA - New York"
wd_us_states_ny <- wd_us_states_ny |> relocate(iso3c, country_name)

wd_us_states <- rbind(
  wd_us_states |> filter(iso3c != "USA-NY"),
  wd_us_states_ny
)

mdd_us <- md_us_states |>
  mutate(date = make_yearmonth(year = year, month = time)) |>
  filter(!is.na(year)) |>
  getDailyFromMonthly(c("deaths")) |>
  select(iso3c, date, deaths) |>
  distinct(iso3c, date, .keep_all = TRUE)

wdd_us <- wd_us_states |>
  mutate(date = make_yearweek(year = year, week = time)) |>
  getDailyFromWeekly(c("deaths")) |>
  select(iso3c, date, deaths) |>
  distinct(iso3c, date, .keep_all = TRUE)

# Merge Final DF, use weekly if available, otherwise monthly.
dd_us <- merge(
  x = mdd_us,
  y = wdd_us,
  by = c("iso3c", "date"), all.x = TRUE, all.y = TRUE
) |>
  mutate(
    age_group = "all",
    deaths = ifelse(!is.na(deaths.y), deaths.y, deaths.x)
  ) |>
  select(iso3c, date, age_group, deaths) |>
  as_tibble()

# By age monthly, US
deaths_age_usa <- as_tibble(read.csv("./data_static/usa_age.csv")) |>
  mutate(year = left(Month.Code, 4), month = right(Month.Code, 2)) |>
  mutate(date = make_yearmonth(year = year, month = month)) |>
  select(date, 5, 6) |>
  setNames(c("date", "age_group", "deaths")) |>
  mutate(
    age_group = case_when(
      age_group %in% c("1", "1-4", "5-9", "10-14", "15-19", "20-24") ~ "0-24",
      age_group %in% c("25-29", "30-34", "35-39", "40-44") ~ "25-44",
      age_group %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
      age_group %in% c("65-69", "70-74") ~ "65-74",
      age_group %in% c("75-79", "80-84") ~ "75-84",
      age_group %in% c("85-89", "90-94", "95-99", "100+") ~ "85+",
      .default = NA
    ),
    iso3c = "USA"
  ) |>
  filter(!is.na(age_group)) |>
  group_by(iso3c, date, age_group) |>
  summarise(deaths = sum(as.numeric(deaths))) |>
  getDailyFromMonthly(c("deaths"))

# By age weekly
deaths <- as_tibble(read.csv("./data/usa_states_age_weekly.csv")) |>
  filter(Type == "Predicted (weighted)") |>
  select(Jurisdiction, Year, Week, Age.Group, Number.of.Deaths) |>
  mutate(
    jurisdiction = Jurisdiction,
    year = as.numeric(Year),
    time = as.numeric(Week),
    age_group = Age.Group,
    deaths = Number.of.Deaths
  ) |>
  left_join(
    us_states_iso3c,
    by = "jurisdiction"
  ) |>
  select(iso3c, jurisdiction, year, time, age_group, deaths) |>
  mutate(age_group = case_when(
    age_group == "Under 25 years" ~ "0-24",
    age_group == "25-44 years" ~ "25-44",
    age_group == "45-64 years" ~ "45-64",
    age_group == "65-74 years" ~ "65-74",
    age_group == "75-84 years" ~ "75-84",
    age_group == "85 years and older" ~ "85+"
  ))

# Filter Incomplete
n_ <- length((deaths |> filter(jurisdiction == "United States"))$jurisdiction)
complete_states <- deaths |>
  count(jurisdiction) |>
  filter(n >= n_ * .99)
deaths <- deaths |> filter(jurisdiction %in% complete_states$jurisdiction)

# Combine NY/NYC
deaths_ny <- deaths |>
  filter(jurisdiction %in% c("New York", "New York City")) |>
  group_by(year, time, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()
deaths_ny$iso3c <- "USA-NY"
deaths_ny$jurisdiction <- "New York"
deaths_ny <- deaths_ny |> relocate(iso3c, jurisdiction)
deaths <- rbind(deaths |> filter(iso3c != "USA-NY"), deaths_ny) |>
  mutate(date = make_yearweek(year = year, week = time)) |>
  select(iso3c, date, age_group, deaths) |>
  getDailyFromWeekly(c("deaths"))
deaths <- rbind(
  deaths_age_usa |> filter(date < as.Date("2015-01-01")),
  deaths
)

# By age yearly
getAgeData <- function(age_group) {
  a <- as_tibble(read.csv(paste0(
    "./data_static/us_states_1999-2017/", age_group, ".csv"
  ))) |>
    select(2, 4, 6) |>
    setNames(c("jurisdiction", "year", "deaths")) |>
    filter(!is.na(date), !is.na(year)) |>
    mutate(age_group = gsub("_", "-", age_group), .after = year)
  b <- as_tibble(read.csv(paste0(
    "./data_static/us_states_2018+/", age_group, ".csv"
  ))) |>
    select(2, 5, 6) |>
    setNames(c("jurisdiction", "year", "deaths")) |>
    filter(!is.na(date), !is.na(year)) |>
    mutate(age_group = gsub("_", "-", age_group), .after = year)
  rbind(a, b)
}

df <- rbind(
  getAgeData("0_24"),
  getAgeData("25_44"),
  getAgeData("45_64"),
  getAgeData("65_74"),
  getAgeData("75_84"),
  getAgeData("85+")
) |>
  mutate(date = ymd(paste0(year, "01-01"))) |>
  left_join(
    us_states_iso3c,
    by = "jurisdiction"
  ) |>
  select(iso3c, date, age_group, deaths) |>
  getDailyFromYearly(c("deaths"))

# Merge Final DF, use weekly if available, otherwise yearly.
dd_us_age <- rbind(deaths, df) |>
  distinct(iso3c, date, age_group, .keep_all = TRUE)

# Population
population <- read_remote("population/usa/six_age_bands.csv") |>
  setNames(c(
    "iso3c", "jurisdiction", "age_group", "year", "population", "is_projection"
  )) |>
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
