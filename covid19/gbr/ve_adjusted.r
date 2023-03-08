source("lib/common.r")

source("covid19/gbr/deaths_by_vaxx.r")
source("covid19/gbr/vaxx.r")
source("population/esp2013.r")


# Deaths by Vaxx Status

## Deaths
mr <- acm |>
  pivot_wider(names_from = vaccination_status, values_from = c(deaths)) |>
  setNames(c(
    "date",
    "age_group",
    "deaths_unvaxx",
    "deaths_vaxx"
  )) |>
  inner_join(vaxxed_population |> select(1:6),
    by = c("date", "age_group")
  ) |>
  mutate(
    unvaxxed = population - vaxxed,
    unvaxxed_lower = population - vaxxed_upper,
    unvaxxed_upper = population - vaxxed_lower
  ) |>
  select(-population) |>
  set_names(c(
    "date",
    "age_group",
    "deaths_unvaxx",
    "deaths_vaxx",
    "population_vaxx",
    "population_vaxx_lower",
    "population_vaxx_upper",
    "population_unvaxx",
    "population_unvaxx_lower",
    "population_unvaxx_upper"
  )) |>
  dplyr::arrange(date, age_group) |>
  group_by(date) |>
  nest() |>
  mutate(data = map(data, ~ .x |> adorn_totals("row", name = "all"))) |>
  unnest(cols = c(data)) |>
  mutate(
    mr_vaccinated = deaths_vaxx / population_vaxx * 100000,
    mr_vaccinated_lower = deaths_vaxx / population_vaxx_upper * 100000,
    mr_vaccinated_upper = deaths_vaxx / population_vaxx_lower * 100000,
    mr_unvaccinated = deaths_unvaxx / population_unvaxx * 100000,
    mr_unvaccinated_lower = deaths_unvaxx / population_unvaxx_upper * 100000,
    mr_unvaccinated_upper = deaths_unvaxx / population_unvaxx_lower * 100000
  ) |>
  select(
    date, age_group,
    mr_vaccinated, mr_vaccinated_lower, mr_vaccinated_upper,
    mr_unvaccinated, mr_unvaccinated_lower, mr_unvaccinated_upper
  )

# CMR by age
mr_crude_age <- mr |>
  filter(age_group != "all") |>
  pivot_longer(
    cols = starts_with("mr_"),
    names_to = "vaccination_status",
    values_to = "rate",
    names_prefix = "mr_"
  ) |>
  separate(
    vaccination_status,
    c("vaccination_status", "kind")
  ) |>
  mutate(kind = ifelse(is.na(kind), "rate", kind)) |>
  pivot_wider(names_from = kind, values_from = c(rate)) |>
  arrange(date, age_group, vaccination_status)

options(vsc.dev.args = list(width = 1920, height = 1080, res = 72 * sf))
ggplot(
  data = mr_crude_age,
  aes(
    x = date, y = rate,
    group = vaccination_status, color = vaccination_status
  )
) +
  labs(
    title = "Crude Mortality Rate (CMR) by Vaccination Status [England]",
    subtitle = "Source: ons.gov.uk",
    x = "Month of Year",
    y = "Deaths/100k"
  ) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(
    ymin = lower,
    ymax = upper,
    fill = vaccination_status
  ), alpha = .3, linetype = 0) +
  twitter_theme() +
  watermark(df$yearmonth, df$value_p) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)
  ) +
  facet_wrap(vars(age_group), scales = "free") +
  scale_color_manual(values = c(
    "#569FE5",
    "#ED6D85"
  )) +
  scale_fill_manual(values = c("#569FE5", "#ED6D85"))

# CMR
mr_crude <- mr |>
  filter(age_group == "all") |>
  select(-age_group) |>
  pivot_longer(
    cols = !date,
    names_to = "vaccination_status",
    values_to = "rate",
    names_prefix = "mr_"
  ) |>
  separate(
    vaccination_status,
    c("vaccination_status", "kind")
  ) |>
  mutate(kind = ifelse(is.na(kind), "rate", kind)) |>
  pivot_wider(names_from = kind, values_from = c("rate"))

ggplot(
  data = mr_crude,
  aes(
    x = date, y = rate,
    group = vaccination_status, color = vaccination_status
  )
) +
  labs(
    title = "Crude Mortality Rate (CMR) by Vaccination Status [England]",
    subtitle = "18+ | Source: ons.gov.uk",
    x = "Month of Year",
    y = "Deaths/100k"
  ) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(
    ymin = lower,
    ymax = upper,
    fill = vaccination_status
  ), alpha = .3, linetype = 0) +
  twitter_theme() +
  watermark(df$yearmonth, df$value_p) +
  scale_x_yearmonth(date_breaks = "1 months", date_labels = "%Y/%m") +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)
  ) +
  scale_color_manual(values = c(
    "#569FE5",
    "#ED6D85"
  )) +
  scale_fill_manual(values = c("#569FE5", "#ED6D85"))

# ASMR
std_pop <- get_weights(c("0-17", unique(mr$age_group)), esp2013_yearly)
mr_asmr <- mr |>
  filter(age_group != "all") |>
  inner_join(std_pop, by = "age_group") |>
  group_by(date) |>
  mutate(
    mr_vaccinated = mr_vaccinated * weight,
    mr_vaccinated_lower = mr_vaccinated_lower * weight,
    mr_vaccinated_upper = mr_vaccinated_upper * weight,
    mr_unvaccinated = mr_unvaccinated * weight,
    mr_unvaccinated_lower = mr_unvaccinated_lower * weight,
    mr_unvaccinated_upper = mr_unvaccinated_upper * weight
  ) |>
  summarise(
    mr_vaccinated = sum(mr_vaccinated),
    mr_vaccinated_lower = sum(mr_vaccinated_lower),
    mr_vaccinated_upper = sum(mr_vaccinated_upper),
    mr_unvaccinated = sum(mr_unvaccinated),
    mr_unvaccinated_lower = sum(mr_unvaccinated_lower),
    mr_unvaccinated_upper = sum(mr_unvaccinated_upper)
  ) |>
  pivot_longer(
    cols = !date,
    names_to = "vaccination_status",
    values_to = "rate",
    names_prefix = "mr_"
  ) |>
  separate(
    vaccination_status,
    c("vaccination_status", "kind")
  ) |>
  mutate(kind = ifelse(is.na(kind), "rate", kind)) |>
  pivot_wider(names_from = kind, values_from = c(rate))

ggplot(
  mr_asmr,
  aes(
    x = date, y = rate,
    group = vaccination_status, color = vaccination_status
  )
) +
  labs(
    title = "Age-Std. Mortality Rate (ASMR) by Vaccination Status [England]",
    subtitle = "18+ | Std. Pop.: ESP2013 | Source: ons.gov.uk",
    x = "Month of Year",
    y = "Deaths/100k"
  ) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(
    ymin = lower,
    ymax = upper,
    fill = vaccination_status
  ), alpha = .3, linetype = 0) +
  twitter_theme() +
  watermark(df$yearmonth, df$value_p) +
  scale_x_yearmonth(date_breaks = "1 months", date_labels = "%Y/%m") +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)
  ) +
  scale_color_manual(values = c(
    "#569FE5",
    "#ED6D85"
  )) +
  scale_fill_manual(values = c("#569FE5", "#ED6D85"))


# Death totals, dataset comparison

## Deaths from Table 5, Dataset by Vaxx St
monthly_deaths1 <- acm |>
  group_by(date) |>
  summarize(deaths = sum(deaths))

## Weekly Totals Dataset
data <- read_excel(
  "./data/uk_acm.xlsx",
  sheet = "1",
  range = "A6:H99"
)

daily_deaths <- data |>
  select(1, 6, 7, 8) |>
  setNames(c("week", "deaths_2022", "deaths_2021", "baseline")) %>%
  pivot_longer(
    cols = starts_with("deaths_"), names_prefix = "deaths_",
    names_to = "year", values_to = "deaths"
  ) |>
  mutate(date = make_yearweek(
    year = as.numeric(year), week = as.numeric(week)
  ), .before = "baseline") |>
  select(-week, -year) |>
  mutate(excess = deaths - baseline) |>
  filter(!is.na(date)) |>
  arrange(date) |>
  select(date, deaths) |>
  getDailyFromWeekly("deaths")

monthly_deaths2 <- daily_deaths |>
  mutate(date = yearmonth(date)) |>
  group_by(date) |>
  summarise(deaths = sum(deaths))

monthly_deaths <- monthly_deaths1 |>
  inner_join(monthly_deaths2, by = c("date")) |>
  setNames((c(
    "date",
    "Monthly Deaths by Age & Vaccination status (table 5)",
    "Weekly Deaths England"
  ))) |>
  pivot_longer(!date, values_to = "deaths", names_to = "source")

## Plot both
ggplot(
  data = monthly_deaths,
  aes(x = date, y = deaths, group = source, color = source)
) +
  labs(
    title = "Monthly Death Totals by different Datasets [England]",
    subtitle = "Source: ons.gov.uk",
    x = "Month of Year",
    y = "Deaths"
  ) +
  geom_line(linewidth = 1) +
  twitter_theme() +
  watermark(df$yearmonth, df$value_p) +
  scale_x_yearmonth(date_breaks = "1 months", date_labels = "%Y/%m") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)
  ) +
  scale_color_manual(values = c(
    "#ED6D85",
    "#000000"
  ))

# Yearly difference
monthly_deaths1 |>
  inner_join(monthly_deaths2, by = c("date")) |>
  mutate(date = year(date), diff = deaths.x - deaths.y) |>
  group_by(date) |>
  summarise(
    deaths.x = sum(deaths.x),
    deaths.y = sum(deaths.y),
    diff = sum(diff)
  )

monthly_deaths2 |>
  mutate(date = year(date)) |>
  group_by(date) |>
  summarise(deaths = sum(deaths))
