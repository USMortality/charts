source("lib/common.r")
source("population/std_pop.r")

options(vsc.dev.args = list(
  width = 1920 * sf, height = 1080 * sf, res = 72 * sf
))

df <- read.csv("./data_static/COVID19Death_vaccpersons_AKL10_w.csv") |>
  select(date, altersklasse_covid19, vaccination_status, entries, pop) |>
  setNames(c(
    "date",
    "age_group",
    "vaccination_status",
    "deaths",
    "population"
  )) |>
  as_tibble() |>
  mutate(
    vaccination_status = case_when(
      vaccination_status %in% c(
        "fully_vaccinated", "partially_vaccinated"
      ) ~ "ever_vaccinated",
      vaccination_status %in% c("not_vaccinated") ~ "unvaccinated",
      vaccination_status %in% c("unknown") ~ "unknown"
    )
  ) |>
  group_by(date, age_group, vaccination_status) |>
  summarise(deaths = sum(deaths), population = sum(population)) |>
  filter(!is.na(vaccination_status)) |>
  ungroup() |>
  mutate(date = make_yearweek(
    year = as.integer(left(date, 4)),
    week = as.integer(right(date, 2))
  ))

df1 <- df |>
  filter(!age_group %in% c(
    "12 - 15",
    "16 - 64",
    "5 - 11",
    "65+",
    "Unbekannt"
  )) |>
  filter(!is.na(vaccination_status)) |>
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
std_pop <- get_esp2013_bins(c(
  "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"
))

df2 <- df1 |>
  select(-deaths, -population) |>
  mutate(age_group = gsub(" ", "", age_group)) |>
  inner_join(std_pop, by = c("age_group")) |>
  mutate(asmr = death_rate * weight) |>
  group_by(date, vaccination_status) |>
  summarise(asmr = sum(asmr, na.rm = TRUE)) |>
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
    subtitle = "Age-Adjusted ESP2013 | Source: www.covid19.admin.ch",
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
