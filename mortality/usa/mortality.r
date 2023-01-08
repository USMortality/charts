source("lib/common.r")

# CMR
us_cmr_1 <- as_tibble(read.csv("./data_static/usa_states_1999_2020.csv"))
us_cmr_2 <- as_tibble(read.csv("./data/usa_states_age_cause_weekly.csv"))
us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv"))

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

wd_us_states <- us_cmr_2 %>%
  select(2, 3, 4, 6) %>%
  setNames(c("country_name", "year", "time", "deaths")) %>%
  filter(country_name != "United States") %>%
  left_join(
    us_states_iso3c %>% mutate(country_name = state),
    by = "country_name"
  ) %>%
  mutate(time_unit = "weekly") %>%
  select(5, 1, 2, 3, 7, 4)

# get_usa_states_deaths <- function(file, age_group) {
#   deaths <- as_tibble(read.csv(file)) %>%
#     mutate(year = left(Month.Code, 4), time = right(Month.Code, 2)) %>%
#     select(2, 6, 9, 10)

#   unique(deaths$State) %>% filter(State == "United States")

#   deaths$time_unit <- "monthly"

#   deaths_usa %>%
#     setNames(
#       c("year", "time", "deaths", "iso3c", "country_name", "time_unit")
#     ) %>%
#     relocate(4, 5, 1, 2, 6, 3) %>%
#     mutate(
#       year = as.numeric(year),
#       time = as.numeric(time)
#     )
# }

# deaths <- as_tibble(read.csv("./data/usa_states_age_weekly.csv"))
# deaths %>% filter(Jurisdiction == "United States")
# age_group <- "0_24"
# file <- paste0("./data_static/usa_states_", age_group, ".csv")
# get_usa_mortality("0_14")
