source("lib/common.r")

df <- read_remote("mortality/world_weekly.csv") |>
  mutate(date = yearweek(date))

data1 <- as_tibble(read.csv("./data/owid.csv")) |>
  select(iso_code, people_vaccinated_per_hundred) |>
  setNames(c("iso3c", "dose_pct")) |>
  mutate(dose_pct = dose_pct / 100) |>
  group_by(iso3c) |>
  summarise(dose_pct = max(dose_pct, na.rm = TRUE))

result <- inner_join(
  pre_vaxx |> summarise(pre_vaxx = mean(asmr_usa_excess)),
  post_vaxx |> summarise(
    post_vaxx = sum(asmr_usa_excess, na.rm = TRUE) / post_vaxx_n
  ),
  by = c("iso3c")
) |>
  mutate(diff = post_vaxx - pre_vaxx) |>
  inner_join(data1, by = c("iso3c"))

result |> filter(!is.na(diff))

ggscatter(
  result,
  x = "dose_pct",
  y = "diff",
  label = "iso3c",
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
    x = "Share of population COVID-19 vaccinated (2nd)"
  ) +
  twitter_theme() +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  scale_y_continuous(labels = comma_format(decimal.mark = ","))
