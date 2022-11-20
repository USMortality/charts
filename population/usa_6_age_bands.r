source("lib/common.r")

# Download data
url <- paste0(
  "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/",
  "asrh/SC-EST2020-AGESEX-CIV.csv"
)
data <- read.csv(url)

url2 <- paste0(
  "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/state/",
  "asrh/sc-est2021-agesex-civ.csv"
)
data2 <- as_tibble(read.csv(url2))

# Transform data
a <- data %>%
  filter(SEX == 0) %>%
  select(
    NAME, AGE,
    POPEST2010_CIV, POPEST2011_CIV, POPEST2012_CIV, POPEST2013_CIV,
    POPEST2014_CIV, POPEST2015_CIV, POPEST2016_CIV, POPEST2017_CIV,
    POPEST2018_CIV, POPEST2019_CIV
  ) %>%
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "year", values_to = "population"
  ) %>%
  transform(year = str_sub(year, 7, 10)) %>%
  setNames(c("jurisdiction", "age", "year", "population")) %>%
  as_tibble()

b <- data2 %>%
  filter(SEX == 0) %>%
  select(
    NAME, AGE, POPEST2020_CIV, POPEST2021_CIV
  ) %>%
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "year", values_to = "population"
  ) %>%
  transform(year = str_sub(year, 7, 10)) %>%
  setNames(c("jurisdiction", "age", "year", "population")) %>%
  as_tibble()

c <- bind_rows(list(a, b))

# Create 6 age bands
population_grouped <- c %>%
  mutate(
    # Create categories
    age_group = case_when(
      age == "999" ~ "all",
      age >= 0 & age <= 24 ~ "0-24",
      age >= 25 & age <= 44 ~ "25-44",
      age >= 45 & age <= 64 ~ "45-64",
      age >= 65 & age <= 74 ~ "65-74",
      age >= 75 & age <= 84 ~ "75-84",
      age >= 85 ~ "85+"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("all", "0-24", "25-44", "45-64", "65-74", "75-84", "85+")
    )
  ) %>%
  group_by(jurisdiction, year, age_group) %>%
  summarise(population = sum(population)) %>%
  mutate(year = as.integer(year)) %>%
  ungroup()

# Forecast 2022/23
forecast_population <- function(data) {
  y <- data %>%
    as_tsibble(index = year) %>%
    model(NAIVE(population ~ drift())) %>%
    forecast(h = 2)

  last_available_year <- data$year[length(data$year)]
  data %>%
    add_row(
      jurisdiction = data$jurisdiction[1],
      year = last_available_year + 1,
      age_group = data$age_group[1],
      population = round(y$.mean[1])
    ) %>%
    add_row(
      jurisdiction = data$jurisdiction[1],
      year = last_available_year + 2,
      age_group = data$age_group[1],
      population = round(y$.mean[2])
    )
}

population_grouped_forecasted <- population_grouped %>%
  nest(data = c("year", "population")) %>%
  mutate(data = lapply(data, forecast_population)) %>%
  unnest(cols = "data") %>%
  filter(!is.na(jurisdiction))

save_csv(population_grouped_forecasted, "population/usa_6_age_bands")
