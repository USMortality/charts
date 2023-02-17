source("lib/common.r")

data <- read.csv("./data_static/COVID19Death_vaccpersons_AKL10_w.csv") %>%
  as_tibble() %>%
  mutate(date = make_yearweek(
    year = as.integer(left(date, 4)),
    week = as.integer(right(date, 2))
  ))

df <- data %>%
  filter(date == make_yearweek(2021, 50), altersklasse_covid19 == "70 - 79") %>%
  select(date, altersklasse_covid19, vaccination_status, entries, pop) %>%
  setNames(c(
    "date",
    "age_group",
    "vaccination_status",
    "deaths",
    "population"
  )) %>%
  mutate(
    vaccination_status = case_when(
      vaccination_status == "not_vaccinated" ~ "unvaccinated",
      .default = "vaccinated"
    )
  ) %>%
  group_by(date, age_group, vaccination_status) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), population = sum(population, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!age_group %in% c(
    "12 - 15",
    "16 - 64",
    "5 - 11",
    "65+",
    "Unbekannt"
  )) %>%
  filter(!is.na(vaccination_status)) %>%
  mutate(death_rate = deaths / population * 100000)

# Sum up all vaccinated
df2 <- df1 %>%
  inner_join(std_pop, by = c("age_group")) %>%
  mutate(asmr = death_rate * percentage) %>%
  group_by(date, vaccination_status) %>%
  summarise(asmr = sum(asmr, na.rm = TRUE)) %>%
  ungroup()

ggplot(
  df2,
  aes(
    x = date,
    y = asmr,
    group = vaccination_status,
    color = vaccination_status
  )
) +
  labs(
    title = "Weekly COVID-19 Death Rate by Vaccination Status [Switzerland]",
    subtitle = "Age-Adjusted | Source: www.covid19.admin.ch",
    x = "Week of Year",
    y = "COVID-19 ASMR"
  ) +
  geom_line(linewidth = 1) +
  twitter_theme() +
  scale_x_yearweek(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c(
    "#404E64",
    "#A33F1C"
  )) +
  theme(legend.position = "bottom")
