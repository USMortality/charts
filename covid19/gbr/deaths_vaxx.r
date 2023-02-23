source("lib/common.r")

source("covid19/gbr/vaxx.r")
source("covid19/gbr/deaths_by_vaxx.r")

options(vsc.dev.args = list(width = 1920, height = 1080, res = 72 * sf))

deaths <- df |>
  filter(type == "All causes") |>
  select(-type)

vaxxed |>
  group_by(date) |>
  summarise(vaxxed_lower = sum(vaxxed_lower), vaxxed_upper = sum(vaxxed_upper))

d <- deaths |>
  group_by(date) |>
  summarise(deaths = sum(deaths))

v <- vaxxed_population |>
  filter(age_group == "all") |>
  select(date, vaxxed_pct)

data <- d |> inner_join(v, by = c("date"))

max_p <- 1
max_d <- max(data$deaths)

ggplot(
  data,
  aes(x = date)
) +
  labs(
    title = "All-Cause Deaths & Cumulative COVID-19 Vaccination [England]",
    subtitle = "Source: ons.gov.uk",
    x = "Month of Year",
    y = "Deaths"
  ) +
  geom_col(aes(y = deaths), fill = "black") +
  geom_line(aes(y = vaxxed_pct / (max_p / max_d)), linewidth = 2, color = "#ff0000") +
  scale_y_continuous(
    labels = label_number(suffix = "k", scale = 1e-3),
    sec.axis = sec_axis(
      ~ . * (max_p / max_d),
      name = "Vaccinated (1st)",
      labels = scales::label_percent()
    )
  ) +
  twitter_theme() +
  scale_x_yearmonth(date_breaks = "2 months", date_labels = "%Y/%m") +
  theme(
    # panel.spacing = unit(0.3, "in"),
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)
  )
