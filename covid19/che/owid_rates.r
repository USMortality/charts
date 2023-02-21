source("lib/common.r")

che <- read.csv("data/switzerland-covid-19-weekly-death-rate-by-vaccination-status.csv") |>
  as_tibble() |>
  filter(Entity == "All ages") |>
  select(Day, Unvaccinated, "Fully.vaccinated..no.booster") |>
  setNames(c("date", "unvaccinated", "fully_vaccinated")) |>
  mutate(iso3c = "Switzerland", .before = "date")

usa <- read.csv("data/united-states-rates-of-covid-19-deaths-by-vaccination-status.csv") |>
  as_tibble() |>
  filter(Entity == "All ages") |>
  select(Day, unvaccinated, "vaccinated_without") |>
  setNames(c("date", "unvaccinated", "fully_vaccinated")) |>
  mutate(iso3c = "USA", .before = "date")

tail(gbr)

gbr <- read.csv("data/england-covid-19-mortality-rate-by-vaccination-status.csv") |>
  as_tibble() |>
  filter(Entity == "All ages") |>
  select(Day, Unvaccinated, Fully.vaccinated) |>
  setNames(c("date", "unvaccinated", "fully_vaccinated")) |>
  mutate(iso3c = "UK", .before = "date")

chl <- read.csv("data/chile-covid-19-mortality-rate-by-vaccination-status.csv") |>
  as_tibble() |>
  filter(Entity == "All ages") |>
  select(Day, "X0.or.1.dose", "X2.doses") |>
  setNames(c("date", "unvaccinated", "fully_vaccinated")) |>
  mutate(iso3c = "Chile", .before = "date")

data <- rbind(che, usa, gbr, chl) |> mutate(date = ymd(date))

ggplot(
  data,
  aes(
    x = date,
    y = unvaccinated,
    group = iso3c,
    color = iso3c
  )
) +
  labs(
    title = "Unvaccinated COVID-19 Death Rate",
    subtitle = "Age-Adjusted | Source: https://ourworldindata.org/covid-deaths-by-vaccination",
    x = "Day of Year",
    y = "COVID-19 ASMR"
  ) +
  geom_line(linewidth = 1) +
  twitter_theme() +
  scale_x_date(date_labels = "%Y", breaks = "1 year") +
  theme(legend.position = "top") +
  scale_color_discrete(name = "")


df <- data |> mutate(ratio = unvaccinated / fully_vaccinated)

ggplot(
  df,
  aes(
    x = date,
    y = ratio,
    group = iso3c,
    color = iso3c
  )
) +
  labs(
    title = "Unvaccinated/Vaccinated COVID-19 Death Ratio",
    subtitle = "Age-Adjusted | Source: https://ourworldindata.org/covid-deaths-by-vaccination",
    x = "Day of Year",
    y = "Ratio"
  ) +
  geom_point() +
  twitter_theme() +
  scale_x_date(date_labels = "%Y", breaks = "1 year") +
  theme(legend.position = "top") +
  ylim(c(0, 30)) +
  geom_smooth() +
  scale_color_discrete(name = "")
