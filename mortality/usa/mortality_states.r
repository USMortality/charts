source("lib/common.r")

# CMR
md_usa <- get_usa_deaths("./data_static/usa_all.csv") # USA 1999/1 - 2020/12
us_cmr_1 <- as_tibble(read.csv("./data_static/usa_states_1999_2020.csv")) # US States 1999/1 - 2020/12
us_cmr_2 <- as_tibble(read.csv("./data/usa_states_age_cause_weekly.csv"))
us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv")) %>%
  add_row(iso3c = "US-NYC", state = "New York City") %>%
  add_row(iso3c = "USA", state = "United States")

# Format: iso3c, country_name, year, time, time_unit, deaths
md_us_states <- us_cmr_1 %>%
  mutate(
    state = State,
    year = as.numeric(left(Month.Code, 4)),
    time = as.numeric(right(Month.Code, 2)),
    time_unit = "monthly"
  ) %>%
  left_join(
    us_states_iso3c,
    by = "state"
  ) %>%
  select(13, 2, 10, 11, 12, 6) %>%
  setNames(
    c("iso3c", "country_name", "year", "time", "time_unit", "deaths")
  ) %>%
  filter(!is.na(year))

md_us_states <- rbind(md_usa, md_us_states)

wd_us_states <- us_cmr_2 %>%
  select(2, 3, 4, 6) %>%
  setNames(c("country_name", "year", "time", "deaths")) %>%
  left_join(
    us_states_iso3c %>% mutate(country_name = state),
    by = "country_name"
  ) %>%
  mutate(time_unit = "weekly") %>%
  mutate(country_name = ifelse(
    country_name == "United States",
    "United States",
    paste0("USA - ", country_name)
  )) %>%
  select(5, 1, 2, 3, 7, 4)

# Combine NY/NYC
wd_us_states_ny <- wd_us_states %>%
  filter(country_name %in% c("USA - New York", "USA - New York City")) %>%
  group_by(year, time, time_unit) %>%
  summarise(deaths = sum(deaths)) %>%
  ungroup()
wd_us_states_ny$iso3c <- "US-NY"
wd_us_states_ny$country_name <- "USA - New York"
wd_us_states_ny <- wd_us_states_ny %>% relocate(iso3c, country_name)

wd_us_states <- rbind(
  wd_us_states %>%
    filter(!country_name %in% c("USA - New York", "USA - New York City")),
  wd_us_states_ny
)

mdd_us <- md_us_states %>%
  mutate(date = make_yearmonth(year = year, month = time)) %>%
  filter(!is.na(year)) %>%
  getDailyFromMonthly("deaths") %>%
  select(iso3c, date, deaths) %>%
  distinct(iso3c, date, .keep_all = TRUE)

wdd_us <- wd_us_states %>%
  mutate(date = make_yearweek(year = year, week = time)) %>%
  getDailyFromWeekly("deaths") %>%
  select(iso3c, date, deaths) %>%
  distinct(iso3c, date, .keep_all = TRUE)

# Merge Final DF, use weekly if available, otherwise monthly.
dd_us <- merge(
  x = mdd_us,
  y = wdd_us,
  by = c("iso3c", "date"), all.x = TRUE, all.y = TRUE
) %>%
  mutate(deaths = ifelse(!is.na(deaths.y), deaths.y, deaths.x)) %>%
  select(iso3c, date, deaths) %>%
  as_tibble()

# ASMR/Weekly
std_pop <- read_remote("population/who_std_pop_2.csv")

deaths <- as_tibble(read.csv("./data/usa_states_age_weekly.csv")) %>%
  filter(Type == "Predicted (weighted)") %>%
  select(Jurisdiction, Year, Week, Age.Group, Number.of.Deaths) %>%
  mutate(
    state = Jurisdiction,
    year = as.numeric(Year),
    time = as.numeric(Week),
    age_group = Age.Group,
    deaths = Number.of.Deaths
  ) %>%
  left_join(
    us_states_iso3c,
    by = "state"
  ) %>%
  select(iso3c, state, year, time, age_group, deaths) %>%
  mutate(age_group = case_when(
    age_group == "Under 25 years" ~ "0-24",
    age_group == "25-44 years" ~ "25-44",
    age_group == "45-64 years" ~ "45-64",
    age_group == "65-74 years" ~ "65-74",
    age_group == "75-84 years" ~ "75-84",
    age_group == "85 years and older" ~ "85+"
  ))

# Filter Incomplete
n_ <- length((deaths %>% filter(state == "United States"))$state)
complete_states <- deaths %>%
  count(state) %>%
  filter(n == n_) %>%
  filter(state != "New York") # Does not include NYC, hence exclude.
deaths <- deaths %>% filter(state %in% complete_states$state)

# Calculate Mortality
usa_pop <- read_remote("population/usa/six_age_bands.csv") %>%
  setNames(c("iso3c", "state", "age_group", "year", "population", "is_projection"))

# Calculate ASMR
dd_asmr_us_states <- deaths %>%
  inner_join(usa_pop, by = c("state", "year", "age_group")) %>%
  mutate(mortality = deaths / population * 100000) %>%
  inner_join(std_pop, by = "age_group") %>%
  mutate(date = make_yearweek(year = year, week = time)) %>%
  mutate(asmr = mortality * percentage) %>%
  select(iso3c.x, date, asmr) %>%
  setNames(c("iso3c", "date", "asmr")) %>%
  group_by(iso3c, date) %>%
  summarise(asmr = sum(asmr)) %>%
  mutate(asmr = asmr) %>%
  ungroup() %>%
  filter(iso3c != "USA") %>%
  getDailyFromWeekly("asmr")


# ASMR Yearly
getAgeData <- function(age_group) {
  a <- as_tibble(read.csv(paste0(
    "./data_static/us_states_1999-2017/", age_group, ".csv"
  ))) %>%
    select(2, 4, 6) %>%
    setNames(c("state", "year", "deaths")) %>%
    filter(!is.na(date), !is.na(year)) %>%
    mutate(age_group = gsub("_", "-", age_group), .after = year)
  b <- as_tibble(read.csv(paste0(
    "./data_static/us_states_2018+/", age_group, ".csv"
  ))) %>%
    select(2, 5, 6) %>%
    setNames(c("state", "year", "deaths")) %>%
    filter(!is.na(date), !is.na(year)) %>%
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
)

df <- df %>%
  inner_join(usa_pop, by = c("state", "year", "age_group")) %>%
  mutate(mortality = deaths / population * 100000) %>%
  inner_join(std_pop, by = "age_group") %>%
  mutate(date = ymd(paste0(year, "01-01"))) %>%
  mutate(asmr = mortality * percentage) %>%
  select(iso3c, date, asmr) %>%
  setNames(c("iso3c", "date", "asmr")) %>%
  group_by(iso3c, date) %>%
  summarise(asmr = sum(asmr)) %>%
  mutate(asmr = asmr) %>%
  ungroup() %>%
  getDailyFromYearly("asmr")

# Merge Final DF, use weekly if available, otherwise yearly.
dd_asmr_us_states <- merge(
  x = dd_asmr_us_states,
  y = df,
  by = c("iso3c", "date"), all.y = TRUE
) %>%
  mutate(asmr = ifelse(is.na(asmr.x), asmr.y, asmr.x)) %>%
  select(iso3c, date, asmr)
