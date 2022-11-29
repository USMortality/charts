source("lib/common.r")

# Load Data
world_population <- read_excel(
  "./data/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
  sheet = "Estimates",
  col_types = c(
    "text", "text", "text", "text", "numeric", "numeric", "numeric"
  ),
  range = cell_cols(6:12)
)
deaths1 <- as_tibble(read.csv("./data/world_mortality.csv"))
deaths2 <- as_tibble(read.csv("./data/mortality_org.csv", skip = 2))

# Deaths
wd1 <- deaths1 %>%
  filter(time_unit == "weekly") %>%
  mutate(date = make_yearweek(year = year, week = time)) %>%
  select(iso3c, year, time, date, deaths) %>%
  setNames(c("iso3c", "year", "week", "date", "deaths"))

wd2 <- deaths2 %>%
  filter(Sex == "b") %>%
  select(CountryCode, Year, Week, DTotal) %>%
  mutate(date = make_yearweek(
    year = Year, week = Week
  )) %>%
  select(CountryCode, Year, Week, date, DTotal) %>%
  setNames(c("iso3c", "year", "week", "date", "deaths"))

wd2$iso3c[wd2$iso3c == "DEUTNP"] <- "DEU"
wd2$iso3c[wd2$iso3c == "FRATNP"] <- "FRA"
wd2$iso3c[wd2$iso3c == "NZL_NP"] <- "NZL"
wd2$iso3c[wd2$iso3c == "GBR_NP"] <- "GBR"

wd <- full_join(wd1, wd2, by = c("iso3c", "year", "week", "date")) %>%
  mutate(deaths = as.integer(ifelse(is.na(deaths.y), deaths.x, deaths.y))) %>%
  select(iso3c, year, week, date, deaths)

wdd <- wd %>%
  uncount(7, .id = "day") %>%
  mutate(date = date_parse(
    paste0(year, "-W", week, "-1"),
    format = "%G-W%V-%u"
  )) %>%
  mutate(date = date + days(day - 1)) %>%
  mutate(deaths = deaths / 7) %>%
  select(iso3c, date, deaths) %>%
  distinct(iso3c, date, .keep_all = TRUE)

md <- deaths1 %>%
  filter(time_unit == "monthly") %>%
  mutate(date = make_yearmonth(year = year, month = time)) %>%
  select(iso3c, year, time, date, deaths) %>%
  setNames(c("iso3c", "year", "month", "date", "deaths"))

mdd <- md %>%
  uncount(days_in_month(date), .id = "day") %>%
  mutate(
    date = date_parse(
      paste0(year, "-", month, "-1"),
      format = "%Y-%m-%d"
    )
  ) %>%
  mutate(date = date + days(day) - 1) %>%
  mutate(deaths = deaths / days_in_month(date)) %>%
  select(iso3c, date, deaths)

dd <- full_join(wdd, mdd, by = c("iso3c", "date")) %>%
  mutate(deaths = as.integer(ifelse(!is.na(deaths.x), deaths.x, deaths.y))) %>%
  select(iso3c, date, deaths) %>%
  arrange(iso3c, date)

# Population
population_grouped <- world_population %>%
  select(1, 6, 7) %>%
  setNames(c("iso3c", "year", "population")) %>%
  filter(!iso3c %in% c("ISO3 Alpha-code", NA)) %>%
  mutate(year = as.integer(year)) %>%
  mutate(population = as.integer(population * 1000)) %>%
  nest(data = c("year", "population"))

# Forecast 2022/23
forecast_population <- function(data) {
  y <- data %>%
    as_tsibble(index = year) %>%
    model(NAIVE(population ~ drift())) %>%
    forecast(h = 2)

  last_available_year <- data$year[length(data$year)]
  data %>%
    add_row(
      year = as.integer(last_available_year + 1),
      population = as.integer(y$.mean[1])
    ) %>%
    add_row(
      year = as.integer(last_available_year + 2),
      population = as.integer(y$.mean[2])
    )
}

population <- population_grouped %>%
  mutate(data = lapply(data, forecast_population)) %>%
  unnest(cols = "data")

# Join deaths/population
mortality_daily <- dd %>%
  mutate(yearweek = yearweek(date), .after = date) %>%
  mutate(yearmonth = yearmonth(date), .after = date) %>%
  mutate(yearquarter = yearquarter(date), .after = date) %>%
  mutate(fluseason = fluseason(date), .after = date) %>%
  mutate(year = year(date), .after = date) %>%
  inner_join(population, by = c("iso3c", "year")) %>%
  mutate(mortality = deaths / population * 100000)

filter_by_complete_temporal_values <- function(data, col, n) {
  # data <- mortality_daily_nested[[2]][[1]]
  # col <- "yearweek"
  # n <- 7
  start <- data %>%
    filter(.data[[col]] == head(data[[col]], n = 1)) %>%
    group_by(across(all_of(col))) %>%
    filter(n() >= n) %>%
    ungroup()
  mid <- data %>%
    filter(!.data[[col]] %in% c(
      head(data, n = 1)[[col]],
      tail(data, n = 1)[[col]]
    ))
  end <- data %>%
    filter(.data[[col]] == tail(data, n = 1)[[col]]) %>%
    group_by(across(all_of(col))) %>%
    filter(n() >= n) %>%
    ungroup()

  rbind(start, mid, end) %>% group_by(across(all_of(col)))
}

aggregate_data <- function(data, fun) {
  # Filter ends
  switch(fun,
    yearweek = filter_by_complete_temporal_values(data, fun, 7),
    yearmonth = filter_by_complete_temporal_values(data, fun, 28),
    yearquarter = filter_by_complete_temporal_values(data, fun, 90),
    year = filter_by_complete_temporal_values(data, fun, 365),
    fluseason = filter_by_complete_temporal_values(data, fun, 365)
  ) %>%
    summarise(
      deaths = round(sum(deaths)),
      mortality = round(sum(mortality), digits = 1)
    ) %>%
    ungroup() %>%
    setNames(c("date", "deaths", "mortality")) %>%
    as_tibble()
}

filter_ytd <- function(data, max_date) {
  year(max_date) <- year(data$date[1])
  data %>%
    filter(date <= max_date) %>%
    mutate(max_date = max_date)
}

calc_ytd <- function(data) {
  nested <- data %>%
    select(year, date, deaths, population, mortality) %>%
    nest(data = c(date, deaths, population, mortality))
  max_date <- max(nested[[2]][[length(nested[[2]])]]$date)
  nested %>%
    mutate(data = lapply(data, filter_ytd, max_date)) %>%
    unnest(cols = "data")
}

aggregate_data_ytd <- function(data) {
  data %>%
    group_by(year, max_date) %>%
    summarise(
      deaths = round(sum(deaths)),
      mortality = round(sum(mortality), digits = 1)
    ) %>%
    ungroup() %>%
    setNames(c("date", "max_date", "deaths", "mortality")) %>%
    as_tibble()
}

mortality_daily_nested <- mortality_daily %>%
  nest(data = c(
    date, year, fluseason, yearquarter, yearmonth, yearweek, deaths, population,
    mortality
  ))

weekly <- mortality_daily_nested %>%
  mutate(data = lapply(data, aggregate_data, "yearweek")) %>%
  unnest(cols = "data")
save_csv(weekly, "mortality/world_weekly")

monthly <- mortality_daily_nested %>%
  mutate(data = lapply(data, aggregate_data, "yearmonth")) %>%
  unnest(cols = "data")
save_csv(monthly, "mortality/world_monthly")

quarterly <- mortality_daily_nested %>%
  mutate(data = lapply(data, aggregate_data, "yearquarter")) %>%
  unnest(cols = "data")
save_csv(quarterly, "mortality/world_quarterly")

yearly <- mortality_daily_nested %>%
  mutate(data = lapply(data, aggregate_data, "year")) %>%
  unnest(cols = "data")
save_csv(yearly, "mortality/world_yearly")

mortality_daily_nested_ytd <- mortality_daily_nested %>%
  mutate(data = lapply(data, calc_ytd))
yearly_ytd <- mortality_daily_nested_ytd %>%
  mutate(data = lapply(data, aggregate_data_ytd)) %>%
  unnest(cols = "data")
save_csv(yearly_ytd, "mortality/world_yearly_ytd")

fluseason <- mortality_daily_nested %>%
  mutate(data = lapply(data, aggregate_data, "fluseason")) %>%
  unnest(cols = "data")
save_csv(fluseason, "mortality/world_fluseason")
