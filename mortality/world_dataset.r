source("lib/common.r")
source("mortality/world_dataset_asmr.r")
source("mortality/usa/mortality_states.r")
source("mortality/deu/mortality_states.r")

# Load Data
deaths1 <- as_tibble(read.csv("./data/world_mortality.csv"))
deaths2 <- as_tibble(read.csv("./data/mortality_org.csv", skip = 2))
population <- read_remote("population/world.csv")
baseline_size <- read_remote("mortality/world_baseline.csv")

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

  rbind(start, mid, end) %>% group_by(across(all_of(c("iso3c", col))))
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
      deaths = round(sumIfNotEmpty(deaths)),
      cmr = round(sumIfNotEmpty(cmr), digits = 1),
      asmr = round(sumIfNotEmpty(asmr), digits = 1)
    ) %>%
    ungroup() %>%
    setNames(c("iso3c", "date", "deaths", "cmr", "asmr")) %>%
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
      deaths = round(sumIfNotEmpty(deaths)),
      cmr = round(sumIfNotEmpty(cmr), digits = 1),
      asmr = round(sumIfNotEmpty(asmr), digits = 1)
    ) %>%
    ungroup() %>%
    setNames(
      c("date", "max_date_cmr", "max_date_asmr", "deaths", "cmr", "asmr")
    ) %>%
    as_tibble()
}

calc_sma <- function(data, n) {
  data$deaths <- round(SMA(data$deaths, n = n), 0)
  data$cmr <- round(SMA(data$cmr, n = n), 2)
  if (nrow(data %>% filter(!is.na(asmr))) > 0) {
    data_non_na <- data %>% drop_na()
    data_non_na$asmr <- round(SMA(data_non_na$asmr, n = n), 2)
    data <- left_join(data,
      data_non_na %>% select(date, asmr),
      by = c("date")
    ) %>%
      select(-asmr.x) %>%
      setNames(c("iso3c", "date", "deaths", "cmr", "asmr"))
  }
  data %>% filter(!(is.na(cmr) & is.na(asmr)))
}

calculate_excess <- function(data, col_name) {
  col <- sym(col_name)
  data %>% mutate(
    "{col_name}_excess" := !!col - !!sym(paste0(col_name, "_baseline")),
    "{col_name}_excess_lower" := !!col - !!sym(paste0(col_name, "_baseline_lower")),
    "{col_name}_excess_upper" := !!col - !!sym(paste0(col_name, "_baseline_upper"))
  )
}

calculate_baseline <- function(data, col_name, chartType, h = 3) {
  iso <- unique(data$iso3c)
  baseline <- baseline_size %>%
    filter(iso3c == iso & chart_type == chartType & type == col_name)

  col <- sym(col_name)
  df <- data %>% filter(date < 2020)

  # No rows, return
  if (nrow(drop_na(df %>% select(!!col))) == 0) {
    data[paste0(col, "_baseline")] <- NA
    data[paste0(col, "_baseline_lower")] <- NA
    data[paste0(col, "_baseline_upper")] <- NA
    data[paste0(col, "_excess")] <- NA
    data[paste0(col, "_excess_lower")] <- NA
    data[paste0(col, "_excess_upper")] <- NA

    return(data)
  } else {
    bl_size <- baseline$window
    bl_data <- tail(df, bl_size)
    fc <- bl_data %>%
      model(TSLM(!!col ~ trend())) %>%
      forecast(h = h)
    fc_hl <- hilo(fc, 99.9) %>% unpack_hilo(cols = `99.9%`)

    bl <- bl_data %>%
      model(TSLM(!!col ~ trend())) %>%
      forecast(new_data = bl_data)
    bl_hl <- hilo(bl, 99.9) %>% unpack_hilo(cols = `99.9%`)

    result <- data.frame(date = c(bl$date, fc$date))
    result[paste0(col, "_baseline")] <- c(bl$.mean, fc$.mean)
    result[paste0(col, "_baseline_lower")] <- c(
      bl_hl$`99.9%_lower`,
      fc_hl$`99.9%_lower`
    )
    result[paste0(col, "_baseline_upper")] <- c(
      bl_hl$`99.9%_upper`,
      fc_hl$`99.9%_upper`
    )

    data %>%
      left_join(result, by = "date") %>%
      calculate_excess(col_name) %>%
      round_x(col_name, ifelse(col_name == "deaths", 0, 2))
  }
}

round_x <- function(data, col_name, digits = 0) {
  data %>%
    mutate(
      "{col_name}_baseline" := round(!!sym(paste0(col_name, "_baseline")), digits),
      "{col_name}_baseline_lower" := round(!!sym(paste0(col_name, "_baseline_lower")), digits),
      "{col_name}_baseline_upper" := round(!!sym(paste0(col_name, "_baseline_upper")), digits),
      "{col_name}_excess" := round(!!sym(paste0(col_name, "_excess")), digits),
      "{col_name}_excess_lower" := round(!!sym(paste0(col_name, "_excess_lower")), digits),
      "{col_name}_excess_upper" := round(!!sym(paste0(col_name, "_excess_upper")), digits)
    )
}

calculate_baseline_excess <- function(data, chartType) {
  if (chartType == "yearly") {
    ts <- data %>% as_tsibble(index = date)
  } else if (chartType == "fluseason") {
    ts <- data %>%
      mutate(date = as.integer(right(date, 4))) %>%
      as_tsibble(index = date)
  }

  result <- ts %>%
    calculate_baseline("deaths", chartType) %>%
    calculate_baseline("cmr", chartType) %>%
    calculate_baseline("asmr", chartType) %>%
    as_tibble()

  if (chartType == "fluseason") {
    # Restore Flu Season Notation
    result %>% mutate(date = paste0(date - 1, "-", date))
  } else {
    result
  }
}

mortality_daily_nested <- mortality_daily %>%
  nest(data = c(
    iso3c, date, year, fluseason, yearquarter, yearmonth, yearweek, deaths,
    population, cmr, asmr
  ))

mortality_weekly_nested <- mortality_daily_nested %>%
  mutate(data = lapply(data, aggregate_data, "yearweek"))

weekly104wsma <- mortality_weekly_nested %>%
  mutate(data = lapply(data, calc_sma, 104)) %>%
  unnest(cols = "data")
save_csv(weekly104wsma, "mortality/world_weekly_104w_sma")

weekly52wsma <- mortality_weekly_nested %>%
  mutate(data = lapply(data, calc_sma, 52)) %>%
  unnest(cols = "data")
save_csv(weekly52wsma, "mortality/world_weekly_52w_sma")

weekly26wsma <- mortality_weekly_nested %>%
  mutate(data = lapply(data, calc_sma, 26)) %>%
  unnest(cols = "data")
save_csv(weekly26wsma, "mortality/world_weekly_26w_sma")

weekly13wsma <- mortality_weekly_nested %>%
  mutate(data = lapply(data, calc_sma, 13)) %>%
  unnest(cols = "data")
save_csv(weekly13wsma, "mortality/world_weekly_13w_sma")

weekly <- mortality_weekly_nested %>%
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
  mutate(data = lapply(data, calculate_baseline_excess, "yearly")) %>%
  unnest(cols = c(data))
save_csv(yearly, "mortality/world_yearly")

mortality_daily_nested_ytd <- mortality_daily_nested %>%
  mutate(data = lapply(data, calc_ytd))
ytd <- mortality_daily_nested_ytd %>%
  mutate(data = lapply(data, aggregate_data_ytd)) %>%
  unnest(cols = "data")
save_csv(ytd, "mortality/world_ytd")

fluseason <- mortality_daily_nested %>%
  mutate(data = lapply(data, aggregate_data, "fluseason")) %>%
  mutate(data = lapply(data, calculate_baseline_excess, "fluseason")) %>%
  unnest(cols = "data")
save_csv(fluseason, "mortality/world_fluseason")
