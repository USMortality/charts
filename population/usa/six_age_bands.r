source("lib/common.r")

us_states_iso3c <- as_tibble(read.csv("./data_static/usa_states_iso3c.csv")) %>%
  add_row(iso3c = "US-NYC", state = "New York City")

# Download data
data0 <- as_tibble(read.csv("./data/population_usa_2000-2010.csv"))
data1 <- as_tibble(read.csv("./data/population_usa_2010-2020.csv"))
data2 <- as_tibble(read.csv("./data/population_usa_2020-2021.csv"))
data3 <- as_tibble(read.csv("./data/population_usa_county_2000-2010.csv"))
data4 <- as_tibble(read.csv("./data/population_usa_county_2010-2020.csv"))
data5 <- as_tibble(read.csv("./data/population_usa_county_2020-2021.csv"))

# Transform data
a <- data0 %>%
  filter(SEX == 0) %>%
  select(
    NAME, AGE,
    POPESTIMATE2000, POPESTIMATE2001, POPESTIMATE2002,
    POPESTIMATE2003, POPESTIMATE2004, POPESTIMATE2005,
    POPESTIMATE2006, POPESTIMATE2007, POPESTIMATE2008,
    POPESTIMATE2009
  ) %>%
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "year", values_to = "population"
  ) %>%
  transform(year = right(year, 4)) %>%
  setNames(c("jurisdiction", "age", "year", "population"))

b <- data1 %>%
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
  setNames(c("jurisdiction", "age", "year", "population"))

c <- data2 %>%
  filter(SEX == 0) %>%
  select(
    NAME, AGE, POPEST2020_CIV, POPEST2021_CIV
  ) %>%
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "year", values_to = "population"
  ) %>%
  transform(year = str_sub(year, 7, 10)) %>%
  setNames(c("jurisdiction", "age", "year", "population"))

# Create 6 age bands
population_grouped <- bind_rows(list(a, b, c)) %>%
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

# NYC Data
ny0 <- data3 %>%
  filter(SEX == 0) %>%
  select(
    CTYNAME, AGEGRP,
    POPESTIMATE2000, POPESTIMATE2001, POPESTIMATE2002,
    POPESTIMATE2003, POPESTIMATE2004, POPESTIMATE2005,
    POPESTIMATE2006, POPESTIMATE2007, POPESTIMATE2008,
    POPESTIMATE2009
  ) %>%
  pivot_longer(
    cols = starts_with("POPEST"), names_to = "YEAR", values_to = "TOT_POP"
  ) %>%
  transform(YEAR = as.numeric(right(YEAR, 4))) %>%
  as_tibble()

ny1 <- data4 %>%
  mutate(
    YEAR = case_when(
      YEAR == 3 ~ 2010,
      YEAR == 4 ~ 2011,
      YEAR == 5 ~ 2012,
      YEAR == 6 ~ 2013,
      YEAR == 7 ~ 2014,
      YEAR == 8 ~ 2015,
      YEAR == 9 ~ 2016,
      YEAR == 10 ~ 2017,
      YEAR == 11 ~ 2018,
      YEAR == 12 ~ 2019
    )
  ) %>%
  filter(!is.na(YEAR)) %>%
  select(CTYNAME, AGEGRP, YEAR, TOT_POP) %>%
  as_tibble()

ny2 <- data5 %>%
  mutate(YEAR = case_when(YEAR == 2 ~ 2020, YEAR == 3 ~ 2021)) %>%
  filter(!is.na(YEAR)) %>%
  select(CTYNAME, AGEGRP, YEAR, TOT_POP) %>%
  as_tibble()

nyc <- bind_rows(list(ny0, ny1, ny2)) %>%
  filter(CTYNAME %in% c(
    "Bronx County", "Kings County", "New York County", "Queens County",
    "Richmond County"
  )) %>%
  select(YEAR, AGEGRP, TOT_POP) %>%
  setNames(c("year", "age_group", "population")) %>%
  mutate(
    # Translate years
    age_group = case_when(
      age_group == 0 ~ "all",
      age_group >= 1 & age_group <= 5 ~ "0-24",
      age_group >= 6 & age_group <= 9 ~ "25-44",
      age_group >= 10 & age_group <= 13 ~ "45-64",
      age_group >= 14 & age_group <= 15 ~ "65-74",
      age_group >= 16 & age_group <= 17 ~ "75-84",
      age_group == 18 ~ "85"
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  select(year, age_group, population) %>%
  mutate(year = as.integer(year)) %>%
  group_by(year, age_group) %>%
  summarise(population = sum(population)) %>%
  ungroup() %>%
  mutate(jurisdiction = "New York City", .before = "year")

population_grouped <- bind_rows(list(population_grouped, nyc)) %>%
  inner_join(us_states_iso3c, by = c("jurisdiction" = "state"))

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

save_csv(population_grouped_forecasted, "population/usa/six_age_bands")

for (j in unique(population_grouped_forecasted$jurisdiction)) {
  data <- population_grouped_forecasted %>%
    filter(jurisdiction == j) %>%
    filter(age_group != "all")
  chart <-
    ggplot(data, aes(x = year, y = population)) +
    labs(
      title = paste0("Population per Age Group [", j, "]"),
      subtitle = "Datasource: Census.gov",
      y = "Population",
      x = "Year"
    ) +
    geom_line(aes(color = age_group), size = 1) +
    twitter_theme() +
    watermark(df$yearmonth, df$value_p) +
    scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))

  save_chart(chart, paste0("population/usa/", j))
}
