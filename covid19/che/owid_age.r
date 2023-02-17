source("lib/common.r")

options(vsc.dev.args = list(
  width = 1920 * sf, height = 1080 * sf, res = 72 * sf
))

df <- read.csv("./data_static/COVID19Death_vaccpersons_AKL10_w.csv") %>%
  select(date, altersklasse_covid19, vaccination_status, entries, pop) %>%
  setNames(c(
    "date",
    "age_group",
    "vaccination_status",
    "deaths",
    "population"
  )) %>%
  as_tibble() %>%
  mutate(
    vaccination_status = case_when(
      vaccination_status %in% c(
        "fully_vaccinated",
        "fully_vaccinated_no_booster"
      ) ~ "fully_vaccinated",
      vaccination_status %in% c(
        "fully_vaccinated_first_booster"
      ) ~ "fully_vaccinated_with_booster",
      vaccination_status %in% c("not_vaccinated") ~ "unvaccinated"
    )
  ) %>%
  group_by(date, age_group, vaccination_status) %>%
  summarise(deaths = sum(deaths), population = sum(population)) %>%
  ungroup() %>%
  mutate(date = make_yearweek(
    year = as.integer(left(date, 4)),
    week = as.integer(right(date, 2))
  ))

df1 <- df %>%
  filter(!age_group %in% c(
    "12 - 15",
    "16 - 64",
    "5 - 11",
    "65+",
    "Unbekannt"
  )) %>%
  filter(!is.na(vaccination_status)) %>%
  mutate(death_rate = deaths / population * 100000)

# Crude rates by age group
ggplot(
  df1,
  aes(
    x = date,
    y = death_rate,
    group = vaccination_status,
    color = vaccination_status
  )
) +
  labs(
    title = "Weekly COVID-19 Death Rate by Vaccination Status [Switzerland]",
    subtitle = "Source: www.covid19.admin.ch",
    x = "Week of Year",
    y = "COVID-19 Death Rate"
  ) +
  geom_line(linewidth = 1) +
  twitter_theme() +
  scale_x_yearweek(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c(
    "#404E64",
    "#39827E",
    "#A33F1C"
  )) +
  facet_wrap(vars(age_group), scales = "free") +
  theme(
    panel.spacing = unit(0.3, "in"),
    legend.position = "bottom"
  )

# Age adjusted rate
std_pop <- as_tibble(
  read.csv("https://s3.mortality.watch/data/population/who_std_pop_9.csv")
)

df2 <- df1 %>%
  select(-deaths, -population) %>%
  mutate(age_group = gsub(" ", "", age_group)) %>%
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
    "#39827E",
    "#A33F1C"
  )) +
  theme(legend.position = "bottom")

# Sum up all vaccinated
df3 <- df1 %>%
  select(-death_rate) %>%
  mutate(
    vaccination_status = case_when(
      vaccination_status %in% c(
        "fully_vaccinated",
        "fully_vaccinated_with_booster"
      ) ~ "fully_vaccinated",
      vaccination_status %in% c("unvaccinated") ~ "unvaccinated"
    )
  ) %>%
  group_by(date, age_group, vaccination_status) %>%
  summarise(
    deaths = sum(deaths, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    death_rate = deaths / population * 100000,
    age_group = gsub(" ", "", age_group)
  ) %>%
  inner_join(std_pop, by = c("age_group")) %>%
  mutate(asmr = death_rate * percentage) %>%
  group_by(date, vaccination_status) %>%
  summarise(asmr = sum(asmr, na.rm = TRUE)) %>%
  ungroup()

ggplot(
  df3,
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

# df <- df %>% filter(date == make_yearweek(year = 2022, week = 01), age_group == "all")

# Add vaccination_status NA to vaccinated
df4 <- df %>%
  filter(!age_group %in% c(
    "12 - 15",
    "16 - 64",
    "5 - 11",
    "65+",
    "Unbekannt"
  )) %>%
  mutate(
    vaccination_status = case_when(
      vaccination_status %in% c(
        "fully_vaccinated",
        "fully_vaccinated_with_booster",
        NA
      ) ~ "fully_vaccinated",
      vaccination_status %in% c("unvaccinated") ~ "unvaccinated"
    )
  ) %>%
  group_by(date, age_group, vaccination_status) %>%
  summarise(
    deaths = sum(deaths, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    death_rate = deaths / population * 100000,
    age_group = gsub(" ", "", age_group)
  ) %>%
  inner_join(std_pop, by = c("age_group")) %>%
  mutate(asmr = death_rate * percentage) %>%
  group_by(date, vaccination_status) %>%
  summarise(asmr = sum(asmr, na.rm = TRUE)) %>%
  ungroup()

save_csv(df4, "test", upload = FALSE)

ggplot(
  df4,
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
