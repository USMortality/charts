source("lib/common.r")
source("mortality/world_dataset_asmr.r")
source("mortality/usa/mortality_states.r")
source("mortality/deu/mortality_states.r")

# Load Data
deaths1 <- as_tibble(read.csv("./data/world_mortality.csv"))
deaths2 <- as_tibble(read.csv("./data/mortality_org.csv", skip = 2))
population <- read_remote("population/world.csv")

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
  getDailyFromWeekly("deaths") %>%
  select(iso3c, date, deaths) %>%
  distinct(iso3c, date, .keep_all = TRUE)

# Format: iso3c, country_name, year, time, time_unit, deaths
md <- deaths1 %>%
  filter(time_unit == "monthly") %>%
  mutate(date = make_yearmonth(year = year, month = time)) %>%
  select(iso3c, year, time, date, deaths) %>%
  setNames(c("iso3c", "year", "month", "date", "deaths"))

mdd <- md %>%
  getDailyFromMonthly("deaths") %>%
  select(iso3c, date, deaths)

dd <- full_join(wdd, mdd, by = c("iso3c", "date")) %>%
  mutate(deaths = as.integer(ifelse(!is.na(deaths.x), deaths.x, deaths.y))) %>%
  select(iso3c, date, deaths) %>%
  arrange(iso3c, date) %>%
  filter(iso3c != "USA")

dd <- rbind(dd, dd_us, dd_de)

# Join deaths/asmr/population
mortality_daily <- dd %>%
  left_join(
    rbind(dd_asmr, dd_asmr_us_states, dd_asmr_de_states),
    by = c("iso3c", "date")
  ) %>%
  mutate(yearweek = yearweek(date), .after = date) %>%
  mutate(yearmonth = yearmonth(date), .after = date) %>%
  mutate(yearquarter = yearquarter(date), .after = date) %>%
  mutate(fluseason = fluseason(date), .after = date) %>%
  mutate(year = year(date), .after = date) %>%
  inner_join(population, by = c("iso3c", "year")) %>%
  mutate(cmr = deaths / population * 100000) %>%
  select(-is_projection)

filter_by_complete_temporal_values <- function(data, col, n) {
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
      cmr = round(sum(cmr), digits = 1),
      asmr = round(sum(asmr), digits = 1)
    ) %>%
    ungroup() %>%
    setNames(c("date", "deaths", "cmr", "asmr")) %>%
    as_tibble()
}

filter_ytd <- function(data, max_date_cmr, max_date_asmr) {
  year(max_date_cmr) <- year(data$date[1])
  year(max_date_asmr) <- year(data$date[1])

  data <- data %>%
    filter(date <= max_date_cmr) %>%
    mutate(asmr = ifelse(date <= max_date_asmr, asmr, NA)) %>%
    mutate(max_date_cmr = max_date_cmr) %>%
    mutate(max_date_asmr = max_date_asmr)
}

calc_ytd <- function(data) {
  nested <- data %>%
    select(year, date, deaths, population, cmr, asmr) %>%
    nest(data = c(date, deaths, population, cmr, asmr))

  cmr_data <- nested[[2]][[length(nested[[2]])]] %>% filter(!is.na(cmr))
  asmr_data <- nested[[2]][[length(nested[[2]])]] %>% filter(!is.na(asmr))
  max_date_cmr <- max(cmr_data$date)
  max_date_asmr <- if (length(asmr_data) > 0) max(asmr_data$date) else NA
  nested %>%
    mutate(data = lapply(data, filter_ytd, max_date_cmr, max_date_asmr)) %>%
    unnest(cols = "data")
}

aggregate_data_ytd <- function(data) {
  data %>%
    group_by(year, max_date_cmr, max_date_asmr) %>%
    summarise(
      deaths = round(sum(deaths)),
      cmr = round(sum(cmr, na.rm = TRUE), digits = 1),
      asmr = round(sum(asmr, na.rm = TRUE), digits = 1)
    ) %>%
    ungroup() %>%
    setNames(
      c("date", "max_date_cmr", "max_date_asmr", "deaths", "cmr", "asmr")
    ) %>%
    as_tibble()
}

mortality_daily_nested <- mortality_daily %>%
  nest(data = c(
    date, year, fluseason, yearquarter, yearmonth, yearweek, deaths, population,
    cmr, asmr
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
ytd <- mortality_daily_nested_ytd %>%
  mutate(data = lapply(data, aggregate_data_ytd)) %>%
  unnest(cols = "data")
save_csv(ytd, "mortality/world_ytd")

fluseason <- mortality_daily_nested %>%
  mutate(data = lapply(data, aggregate_data, "fluseason")) %>%
  unnest(cols = "data")
save_csv(fluseason, "mortality/world_fluseason")

cmr_countries <- unique(weekly$name) %>% sort()
asmr_countries <- weekly %>%
  filter(!is.na(asmr)) %>%
  .$name %>%
  unique()
countries <- data.frame(country = cmr_countries)
for (country in countries) countries$has_asmr <- country %in% asmr_countries
save_csv(countries, "mortality/world_countries")
