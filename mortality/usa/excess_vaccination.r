source("lib/common.r")

df <- read_remote("mortality/world_weekly.csv") |>
  filter(substr(iso3c, 0, 3) == "USA") |>
  mutate(date = yearweek(date))

data1 <- as_tibble(read.csv("./data/usa_states_vaccination.csv")) |>
  select(Location, Bivalent_Booster_18Plus_Pop_Pct) |>
  setNames(c("iso3c", "dose_pct")) |>
  mutate(iso3c = paste0("USA-", iso3c), dose_pct = dose_pct / 100) |>
  group_by(iso3c) |>
  summarise(dose_pct = max(dose_pct, na.rm = TRUE))

pre_vaxx <- df |>
  filter(date >= make_yearweek(year = 2020, week = 11) &
    date <= make_yearweek(year = 2020, week = 50)) |>
  group_by(iso3c) |>
  mutate(n = row_number())
pre_vaxx_n <- max(pre_vaxx$n)

post_vaxx <- df |>
  filter(date >= make_yearweek(year = 2020, week = 51)) |>
  group_by(iso3c) |>
  mutate(n = row_number())
post_vaxx_n <- max(post_vaxx$n)

result <- inner_join(
  pre_vaxx |> summarise(pre_vaxx = sum(asmr_usa_excess) / pre_vaxx_n),
  post_vaxx |> summarise(
    post_vaxx = sum(asmr_usa_excess, na.rm = TRUE) / post_vaxx_n
  ),
  by = c("iso3c")
) |>
  mutate(diff = post_vaxx - pre_vaxx) |>
  inner_join(data1, by = c("iso3c")) |>
  mutate(name = right(iso3c, 2))

ggscatter(
  result,
  x = "dose_pct",
  y = "diff",
  label = "name",
  add = "reg.line", # Add regression line
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE # Add confidence interval
) +
  stat_cor(color = "red", method = "pearson", label.x = 0.4, label.y = -3) +
  labs(
    title = "Diff. in Excess Mortality Before vs After COVID-19 Vaccination",
    subtitle =
      "Week 2020 W11 - W50 vs 2020 W51 - * | Source: www.mortality.watch",
    y = "Difference in Excess ASMR (Deaths/100k)",
    x = "Share of population COVID-19 vaccinated (Bivalent booster)"
  ) +
  twitter_theme() +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  scale_y_continuous(labels = comma_format(decimal.mark = ","))
