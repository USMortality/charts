source("lib/common.r")

data <- read.csv("./data/owid.csv") |> as_tibble()

df <- data |>
  filter(iso_code == "ISR") |>
  select(date, new_cases_smoothed, new_vaccinations_smoothed) |>
  mutate(date = date(date)) |>
  as_tsibble(index = date) |>
  mutate(new_vaccinations_smoothed = as.numeric(
    new_vaccinations_smoothed
  ) / 10) |>
  pivot_longer(!date)

ggplot(
  df,
  aes(x = date, y = value, group_name = name, color = name)
) +
  labs(
    title = "Israel COVID-19 Cases & Vaccine Doses",
    subtitle = "Source: OWID",
    y = "COVID-19 Cases",
    x = "Date"
  ) +
  geom_line(linewidth = 1.2) +
  twitter_theme() +
  watermark(df$date, df$cmr) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  scale_x_yearweek(date_breaks = "12 weeks", date_labels = "%Y W%W") +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3))
